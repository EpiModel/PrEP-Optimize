library(dplyr)
library(psych)
library(ggplot2)
library(tidyverse)
# package for generalized additive models (gam)
library(mgcv)
# package for optimization algorithms
library(Rsolnp)

# import simulation output data
setwd("C:/Users/Greg/Desktop/PrEP-Optimize")
# df_prep <- readRDS("analysis/data/prepOptim-Yearly-v3-5000sets-100per.rds")
df_prep <- readRDS("analysis/data/prepOptim-Yearly-v4-1000sets-250per.rds")

# translate weekly adherence capacity into yearly capacity
df_prep$POAC_yr <- df_prep$POAC*52

# calculate 10-year outcomes
df_prep_10yr <- full_join(df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(incid, infAvert, PrEPStarts),
                                         sum),
                          df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(PrEPHighAdr, prepElig, prepCurr, pCov, PrEPHighAdr),
                                         mean),
                          by="scenario")

df_prep_10yr <- full_join(df_prep_10yr,
                          df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(POIP, PORC, POAC_yr),
                                         first),
                          by = "scenario")

df_prep_10yr <- full_join(df_prep_10yr,
                          df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(OptimInitStarts, OptimAdhrStarts, OptimRetnStarts),
                                         sum),
                          by="scenario")

df_prep_10yr <- df_prep_10yr %>% filter(POIP <= 0.10, PORC < 2000, POAC_yr < 2000)

gam <- gam(data = df_prep_10yr,
           formula = 100*(infAvert/1199.484) ~ s(POIP, k = 5) + s(POAC_yr, k = 5) + s(PORC, k = 6) + ti(POIP, PORC, k = 6) + ti(POIP, POAC_yr, k = 3) + ti(PORC, POAC_yr, k = 6),
           family = Gamma(link = "log"), method = "REML")


# Objective function: InfAvert~capacity parameters
# Predict from fitted model
# Optimization function seeks to minimize the objective function, so objective function will return the negative of the object function.
obj_fun <- function(x) {
  - exp(predict(gam, newdata = data.frame(POIP = x[1], POAC_yr = x[2], PORC = x[3])))
}

#########
#########
# Sensitivity analysis to find adherence cost cutpoints at which adherence is funded over a range of different budgets
# Generates optim_res_adh_df.rda
for (j in 1:100) {
  adj.i <- 1
  adj.a <- j/100
  adj.r <- 1

  cost.i <- adj.i * (12/6 * 10 * (80 * 395 * 100) + (10 * (46.5 + 0.1*(2*40 + 2*17.85 + 2*80.31)) * 80 * 100))
  cost.a <- adj.a * ((315.18+246+3.90) * 10)
  cost.r <- adj.r * 10 * (60.5 + 0.1*(40*4 + 17.85*2 + 80.31*2 + 9.46*2) + 12.44)

  budget_constraint <- function(x) {
    cost.i * x[1] + cost.a * x[2] + cost.r * x[3]
  }

  # number of different budget constraint values to consider
  n = 5

  # initialize optimization results data.frame
  res <- data.frame(poip = rep(NA, n),
                    poac = rep(NA, n),
                    porc = rep(NA, n),
                    infAvert = rep(NA, n),
                    budget = rep(NA, n),
                    converge = rep(NA, n))

  # specify lower and upper bounds for sequence of budgets to consider
  budget <- c(700000, 1500000 , 3000000, 4500000, 6000000)

  for (i in 1:n) {
    solnp <- solnp(c(.05, 1000, 1000),
                   obj_fun,
                   eqfun=budget_constraint,
                   eqB=budget[i],
                   LB=c(0,0,0),
                   UB=c(0.10, 2000, 2000),
                   control = list(rho = 1,
                                  outer.iter = 200,
                                  inner.iter = 8000,
                                  tol =5e-10,
                                  delta = 1e-7,
                                  trace = 1))
    pars <- solnp$pars
    res$poip[i] <- pars[1]
    res$poac[i] <- pars[2]
    res$porc[i] <- pars[3]
    res$infAvert[i] <- - last(solnp$values)
    res$converge[i] <- solnp$convergence
    res$budget[i] <- budget[i]
    res$adj.a <- adj.a
  }

  if (j != 1) {
    df <- rbind(df, res)
  } else {
    df <- res
  }
}
# save(df, file = "optim_res_adh_df.rda")
df <- df %>% arrange(budget)
test <- df %>% select(budget, adj.a, poac)

# maximum adherence cost adjustments at which adherence intervention is funded at specific max budgets
# .14 for 7,00000
# .57 for 1,500,000
# .56 for 3,000,000
# .57 for 4,500,000
# .54 for 6,000,000


###########
###########
# showcasing adherence cost adjustment to produce sensitivity analysis budget allocation figures
# Generates optim_res_adh_costsharing.rda
adj.i <- 1
adj.a <- .54
adj.r <- 1

cost.i <- adj.i * (12/6 * 10 * (80 * 395 * 100) + (10 * (46.5 + 0.1*(2*40 + 2*17.85 + 2*80.31)) * 80 * 100))
cost.a <- adj.a * ((315.18+246+3.90) * 10)
cost.r <- adj.r * 10 * (60.5 + 0.1*(40*4 + 17.85*2 + 80.31*2 + 9.46*2) + 12.44)

budget_constraint <- function(x) {
  cost.i * x[1] + cost.a * x[2] + cost.r * x[3]
}

# number of different budget constraint values to consider
n = 100

# initialize optimization results data.frame
res <- data.frame(poip = rep(NA, n),
                  poac = rep(NA, n),
                  porc = rep(NA, n),
                  infAvert = rep(NA, n),
                  budget = rep(NA, n),
                  converge = rep(NA, n))

# specify lower and upper bounds for sequence of budgets to consider
budget <- seq(from = .7e6, to =6e6, length.out = n)

for (i in 1:n) {
  solnp <- solnp(c(.05, 1000, 1000),
                 obj_fun,
                 eqfun=budget_constraint,
                 eqB=budget[i],
                 LB=c(0,0,0),
                 UB=c(0.10, 2000, 2000),
                 control = list(rho = 1,
                                outer.iter = 300,
                                inner.iter = 10000,
                                tol =3e-10,
                                delta = 1.5e-7,
                                trace = 1))
  pars <- solnp$pars
  res$poip[i] <- pars[1]
  res$poac[i] <- pars[2]
  res$porc[i] <- pars[3]
  res$infAvert[i] <- - last(solnp$values)
  res$converge[i] <- solnp$convergence
  res$budget[i] <- budget[i]
  res$adj.a <- adj.a
}
# save(res, file = "optim_res_adh_costsharing.rda")

res_plot <- res %>%
  filter(converge == 0) %>%
  rowwise() %>%
  mutate(poip_budget_prop = poip * cost.i / budget,
         poac_budget_prop = poac * cost.a / budget,
         porc_budget_prop = porc * cost.r / budget,
         poip_budget = poip * cost.i,
         poac_budget = poac * cost.a,
         porc_budget = porc * cost.r)

# plots showing what fraction of budget is allocated to each intervention as a function of budget constraint
res_plot_area_prop <- res_plot %>%
  pivot_longer(cols = c(poip_budget_prop, poac_budget_prop, porc_budget_prop),
               names_to = "program")

ggplot(res_plot_area_prop, aes(x = budget, y = value, fill = program)) +
  geom_area()

res_plot_area <- res_plot %>%
  pivot_longer(cols = c(poip_budget, poac_budget, porc_budget),
               names_to = "program")
ggplot(res_plot_area, aes(x = budget, y = value, fill = program)) +
  geom_area()

ggplot(data = res_plot, aes(x = budget, y = poip)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = porc)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = infAvert)) + geom_line()

##########
##########
# Uses basecase costing assumptions
# Generates optim_res_costsharing.rda
adj.i <- 1
adj.a <- 1
adj.r <- 1

cost.i <- adj.i * (12/6 * 10 * (80 * 395 * 100) + (10 * (46.5 + 0.1*(2*40 + 2*17.85 + 2*80.31)) * 80 * 100))
cost.a <- adj.a * ((315.18+246+3.90) * 10)
cost.r <- adj.r * 10 * (60.5 + 0.1*(40*4 + 17.85*2 + 80.31*2 + 9.46*2) + 12.44)

budget_constraint <- function(x) {
  cost.i * x[1] + cost.a * x[2] + cost.r * x[3]
}

# number of different budget constraint values to consider
n = 100

# initialize optimization results data.frame
res <- data.frame(poip = rep(NA, n),
                  poac = rep(NA, n),
                  porc = rep(NA, n),
                  infAvert = rep(NA, n),
                  budget = rep(NA, n),
                  converge = rep(NA, n))

# specify lower and upper bounds for sequence of budgets to consider
budget <- seq(from = .7e6, to =6e6, length.out = n)
for (i in 1:n) {
  solnp <- solnp(c(.05, 1000, 1000),
                 obj_fun,
                 eqfun=budget_constraint,
                 eqB=budget[i],
                 LB=c(0,0,0),
                 UB=c(0.10, 2000, 2000),
                 control = list(rho = 1,
                                outer.iter = 200,
                                inner.iter = 8000,
                                tol =5e-10,
                                delta = 1e-7,
                                trace = 1))
  pars <- solnp$pars
  res$poip[i] <- pars[1]
  res$poac[i] <- pars[2]
  res$porc[i] <- pars[3]
  res$infAvert[i] <- - last(solnp$values)
  res$converge[i] <- solnp$convergence
  res$budget[i] <- budget[i]

}
# save(res, file = "optim_res_costsharing.rda")

res_plot <- res %>%
  filter(converge == 0) %>%
  rowwise() %>%
  mutate(poip_budget_prop = poip * cost.i / budget,
         poac_budget_prop = poac * cost.a / budget,
         porc_budget_prop = porc * cost.r / budget,
         poip_budget = poip * cost.i,
         poac_budget = poac * cost.a,
         porc_budget = porc * cost.r)

# plots showing what fraction of budget is allocated to each intervention as a function of budget constraint
res_plot_area_prop <- res_plot %>%
  pivot_longer(cols = c(poip_budget_prop, poac_budget_prop, porc_budget_prop),
               names_to = "program")

ggplot(res_plot_area_prop, aes(x = budget, y = value, fill = program)) +
  geom_area()

res_plot_area <- res_plot %>%
  pivot_longer(cols = c(poip_budget, poac_budget, porc_budget),
               names_to = "program")
ggplot(res_plot_area, aes(x = budget, y = value, fill = program)) +
  geom_area()

ggplot(data = res_plot, aes(x = budget, y = poip)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = porc)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = infAvert)) + geom_line()

##########
##########
# Setup for creating table of economic outcomes.
# Select only specific budget values
# Uses basecase costing assumptions
# Generates optim_res_table_costsharing.rda

adj.i <- 1
adj.a <- 1
adj.r <- 1

cost.i <- adj.i * (12/6 * 10 * (80 * 395 * 100) + (10 * (46.5 + 0.1*(2*40 + 2*17.85 + 2*80.31)) * 80 * 100))
cost.a <- adj.a * ((315.18+246+3.90) * 10)
cost.r <- adj.r * 10 * (60.5 + 0.1*(40*4 + 17.85*2 + 80.31*2 + 9.46*2) + 12.44)

budget_constraint <- function(x) {
  cost.i * x[1] + cost.a * x[2] + cost.r * x[3]
}

# number of different budget constraint values to consider
n = 5

# initialize optimization results data.frame
res <- data.frame(poip = rep(NA, n),
                  poac = rep(NA, n),
                  porc = rep(NA, n),
                  infAvert = rep(NA, n),
                  budget = rep(NA, n),
                  converge = rep(NA, n))

# specify lower and upper bounds for sequence of budgets to consider
budget <- c(700000, 1500000 , 3000000, 4500000, 6000000)
for (i in 1:5) {
  solnp <- solnp(c(.05, 100, 1000),
                 obj_fun,
                 eqfun=budget_constraint,
                 eqB=budget[i],
                 LB=c(0,0,0),
                 UB=c(0.10, 2000, 2000),
                 control = list(rho = 1,
                                outer.iter = 200,
                                inner.iter = 8000,
                                tol =8e-9,
                                delta = 2e-7,
                                trace = 1))
  pars <- solnp$pars
  res$poip[i] <- pars[1]
  res$poac[i] <- pars[2]
  res$porc[i] <- pars[3]
  res$infAvert[i] <- - last(solnp$values)
  res$converge[i] <- solnp$convergence
  res$budget[i] <- budget[i]
}
# save(res, file = "optim_res_table_costsharing.rda")

res_plot <- res %>%
  filter(converge == 0) %>%
  rowwise() %>%
  mutate(poip_budget_prop = poip * cost.i / budget,
         poac_budget_prop = poac * cost.a / budget,
         porc_budget_prop = porc * cost.r / budget,
         poip_budget = poip * cost.i,
         poac_budget = poac * cost.a,
         porc_budget = porc * cost.r)

# plots showing what fraction of budget is allocated to each intervention as a function of budget constraint
res_plot_area_prop <- res_plot %>%
  pivot_longer(cols = c(poip_budget_prop, poac_budget_prop, porc_budget_prop),
               names_to = "program")

ggplot(res_plot_area_prop, aes(x = budget, y = value, fill = program)) +
  geom_area()

res_plot_area <- res_plot %>%
  pivot_longer(cols = c(poip_budget, poac_budget, porc_budget),
               names_to = "program")
ggplot(res_plot_area, aes(x = budget, y = value, fill = program)) +
  geom_area()

ggplot(data = res_plot, aes(x = budget, y = poip)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = porc)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = infAvert)) + geom_line()



