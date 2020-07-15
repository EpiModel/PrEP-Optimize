library(dplyr)
library(psych)
library(ggplot2)
library(tidyverse)
library(scam)
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

# only consider POIP < 5% -- higher values are likely unreasonable
# df_prep_10yr <- df_prep_10yr %>% filter(POIP <= 0.10, PORC < 1700, POAC_yr < 2000)

df_prep_vis <- df_prep_10yr %>% filter(POIP <= 0.10)
# PrEPHighAdr plateaus at lower values of POAC when POIP is low
ggplot(df_prep_vis, aes(x = POAC_yr, y = PrEPHighAdr)) + geom_point()
# POAC has no influence of pCov (this is working as intended)
ggplot(df_prep_vis, aes(x = POAC_yr, y = pCov)) + geom_point()

# PORC has no influence on PrEPHighAdr (working as intended)
# Thick horizontal band at top appears because most POAC values dwarf weekly PrEP initiations (esp at lower POIP values)
ggplot(df_prep_vis, aes(x = PORC, y = PrEPHighAdr)) + geom_point()
# pCov plateaus at lower PORC values (and at a lower pCov) when POIP is low
# With lowest POIP values, PORC provides a ~3.5% bump to pCov
ggplot(df_prep_vis, aes(x = PORC, y = pCov)) + geom_point()

ggplot(df_prep_vis, aes(x = POIP, y = PrEPHighAdr)) + geom_point()
# POIP has a similar range of effects on pCov as PORC
# Most PORC values are much higher than prevalent PrEP users, leading to thick band
ggplot(df_prep_vis, aes(x = POIP, y = pCov)) + geom_point()

df_prep_vis <- df_prep_10yr %>% filter(POIP <= 0.10, PORC < 2000, POAC_yr < 2000)

ggplot(df_prep_vis, aes(x = POAC_yr, y = PrEPHighAdr)) + geom_point()
ggplot(df_prep_vis, aes(x = POAC_yr, y = pCov)) + geom_point()

ggplot(df_prep_vis, aes(x = PORC, y = PrEPHighAdr)) + geom_point()
ggplot(df_prep_vis, aes(x = PORC, y = pCov)) + geom_point()

ggplot(df_prep_vis, aes(x = POIP, y = PrEPHighAdr)) + geom_point()
ggplot(df_prep_vis, aes(x = POIP, y = pCov)) + geom_point()


df_prep_10yr <- df_prep_10yr %>% filter(POIP <= 0.10, PORC < 2000, POAC_yr < 2000)

# GAM specification
# k <- 7

gam <- gam(data = df_prep_10yr,
           formula = infAvert ~ s(POIP, k = 4) + s(POAC_yr, k = 4) + s(PORC, k = 4) + ti(POIP, PORC, k = 4) + ti(POIP, POAC_yr, k = 4) + ti(PORC, POAC_yr, k = 4),
           family = Gamma(link = "log"))
gam.check(gam)
plot(gam, scheme = 1)
plot(gam, scheme = 2)
plot(gam, scheme = 3)
vis.gam(gam, view = c("PORC", "POIP"))
vis.gam(gam, view = c("PORC", "POIP"), plot.type = "contour")
vis.gam(gam, view = c("POAC_yr", "POIP"))
vis.gam(gam, view = c("POAC_yr", "POIP"), plot.type = "contour")
vis.gam(gam, view = c("POAC_yr", "PORC"))
vis.gam(gam, view = c("POAC_yr", "PORC"), plot.type = "contour")

# coef(gam)
# gam
# plot(gam)
# SCAM (Shape Constrained Additive Model) specification
# bs = "cv" forces spline objective function to be concave
# this smoothness/shape constraint helps the optimization functions
# k <- 4
# scam1 <- scam(data = df_prep_10yr,
#              formula = infAvert ~ s(POIP, bs = "cv", k = k) + s(POAC_yr, bs = "cv", k = k) + s(PORC, bs = "cv", k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k),
#              family = Gamma(link = "identity"))
# # scam <- scam(data = df_prep_10yr,
# #              formula = infAvert ~ s(POIP, bs = "micv", k = k) + s(POAC_yr, bs = "micv", k = k) + s(PORC, bs = "micv", k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k),
# #              family = Gamma(link = "log"))
# #
# # scam <- scam(data = df_prep_10yr,
# #               formula = infAvert ~ s(POIP, bs = "micv", k = k) + s(POAC_yr, bs = "micv", k = k) + s(PORC, bs = "micv", k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k),
# #               family = Gamma(link = "log"))
# scam <- scam(data = df_prep_10yr,
#              formula = infAvert ~ s(POIP, bs = "micv", k = k) + s(POAC_yr, bs = "micv", k = k) + s(PORC, bs = "micv", k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k) +  ti(POAC_yr, PORC, k = k),
#              family = Gamma(link = "log"))
#
# scam2 <- scam(data = df_prep_10yr,
#               formula = infAvert ~ s(POIP, bs = "micv", k = k) + s(POAC_yr, bs = "micv", k = k) + s(PORC, bs = "micv", k = k),
#               family = Gamma(link = "log"))

# scam3 <- scam(data = df_prep_10yr,
#              formula = infAvert ~ s(POIP, bs = "cv", k = k) + s(POAC_yr, bs = "cv", k = k) + s(PORC, bs = "cv", k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k),
#              family = Gamma(link = "inverse"), start = coef(gam))
# scam3 <- scam(data = df_prep_10yr,
#               formula = infAvert ~ s(POIP, bs = "cx", k = k) + s(POAC_yr, bs = "cx", k = k) + s(PORC, bs = "cx", k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k),
#               family = Gamma(link = "inverse"))
# plot(scam2)
# p <- 5
# glm <- glm(data = df_prep_10yr,
#            formula = infAvert ~ poly(POIP, p) + poly(POAC_yr, p) + poly(PORC, p) + poly(POIP*PORC, p) + poly(POIP*POAC_yr, p) + poly(POAC_yr*PORC, p),
#            family = Gamma(link = "identity"))



exp(predict(gam, newdata = data.frame(POIP = .01, POAC_yr = 1000, PORC = 1000)))
predict(gam, newdata = data.frame(POIP = 0, POAC_yr = 0, PORC = 0))

# Objective function: InfAvert~capacity parameters
# Predict from fitted model
# Optimization function seeks to minimize the objective function, so objective function will return the negative of the object function.
obj_fun <- function(x) {
  - exp(predict(gam, newdata = data.frame(POIP = x[1], POAC_yr = x[2], PORC = x[3])))
}

outlist <- list()
adj.grid <- expand.grid(adj.i = seq(from = 10, to = 15, by = 2.5),
                        adj.a = seq(from = .5, to = 1, by = .25),
                        adj.r = seq(from = .25, to = 2, by = .25))
save(adj.grid, file = "adj.grid2.rda")
# for (j in 1:2) {
for (j in 1:nrow(adj.grid)) {
  print(j)
  adj.i <- adj.grid$adj.i[j]
  adj.a <- adj.grid$adj.a[j]
  adj.r <- adj.grid$adj.r[j]

  # 100 due to percentage
  # $0.79/click and 1000 clicks to 1 app user (assumption)
  # 80 is related to the proportion of potential app users vs the target population (PrEP indicated individuals)
  cost.i <- adj.i * (80 * 790 * 100)
  # 983.39 is basecase estimate for one-time per person cost at PrEP initiation
  cost.a <- adj.a * (983.39 * 10)
  # 77.17 is one-time per person cost at PrEP initiation
  # 50.17 is monthly cost per person
  cost.r <- adj.r * 10 * (77.17/(52 * 337/365) + (50.17 * (52/12)))

  budget_constraint <- function(x) {
    # x[1] - init, x[2] - adhr, x[3] - retn
    cost.i * x[1] + cost.a * x[2] + cost.r * x[3]
  }

  # number of different budget constraint values to consider
  n = 50

  # initialize optimization results data.frame
  res <- data.frame(poip = rep(NA, n),
                    poac = rep(NA, n),
                    porc = rep(NA, n),
                    infAvert = rep(NA, n),
                    budget = rep(NA, n),
                    converge = rep(NA, n))

  # specify lower and upper bounds for sequence of budgets to consider
  budget <- seq(from = 3e6, to = 9e6, length.out = n)

  if (!(j %in% c(67))) {
    for (i in 1:n) {
      # if (i == 1 & j == 36) {
      #   browser()
      # }
      # c(runif(n = 1, 0, .1), runif(n = 1, 0, 2000), runif(n = 1, 0, 2000)
      #c(.05, 1000, 1000)
      solnp <- solnp(c(.05, 1000, 1000),
                     obj_fun,
                     eqfun=budget_constraint,
                     eqB=budget[i],
                     LB=c(0,0,0),
                     UB=c(0.10, 2000, 2000),
                     control = list(rho = 1,
                                    outer.iter = 2000,
                                    inner.iter = 16000,
                                    tol = 9e-8,
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
  }

  outlist[[j]] <- res
}
save(outlist, file = "adj_grid_outlist_loglink2.rda")
save <- outlist#
load("adj_grid_outlist_loglink2.rda")


i = 8
res <- outlist[[i]]

cost.i <- adj.grid[i,]$adj.i * (80 * 790 * 100)
cost.a <- adj.grid[i,]$adj.a * (511.38 * 10)
cost.r <- adj.grid[i,]$adj.r * 10 * (77.17/(52 * 337/365) + (50.17 * (52/12)))
# discard optimizations that did not converge
res_plot <- res %>%
  filter(converge == 0) %>%
  rowwise() %>%
  mutate(poip_budget_prop = poip * cost.i / budget,
         poac_budget_prop = poac * cost.a / budget,
         porc_budget_prop = porc * cost.r / budget,
         poip_budget = poip * cost.i,
         poac_budget = poac * cost.a,
         porc_budget = porc * cost.r)
# plots showing how optimal poip, poac, pocr, and infAvert change with budget constraint
ggplot(data = res_plot, aes(x = budget, y = poip)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = porc)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = infAvert)) + geom_line()
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


ggplot(data = res_plot, aes(x = budget, y = poip * cost.i / budget)) +
  geom_line() +
  ylab("Proportion of Budget in Uptake")
ggplot(data = res_plot, aes(x = budget, y = poac * cost.a / budget)) +
  geom_line() +
  ylab("Proportion of Budget in Adherence")
ggplot(data = res_plot, aes(x = budget, y = porc * cost.r / budget)) +
  geom_line() +
  ylab("Proportion of Budget in Retention")

# discard optimizations that did not converge
res_plot <- res %>% filter(converge == 0)

######################

n = 101
grid.df <- expand.grid(POIP = seq(0, 0.10, length.out = n),
                       POAC_yr = seq(0, 2000, length.out = n),
                       PORC = seq(0, 2000, length.out = n))
pred <- exp(predict(gam,
                newdata = grid.df))
pred.df <- cbind(pred, grid.df)

porc <- pred.df %>% filter((POIP == min(POIP) | POIP == max(POIP) | POIP == median(POIP) | POIP == quantile(POIP, 0.25) | POIP == quantile(POIP, 0.75)) &
                             (POAC_yr == min(POAC_yr) | POAC_yr == max(POAC_yr) | POAC_yr == median(POAC_yr) | POAC_yr == quantile(POAC_yr, 0.25) | POAC_yr == quantile(POAC_yr, 0.75)))

ggplot(porc, aes(x = PORC, y = pred, color = POAC_yr)) + geom_point() + facet_wrap(.~as.factor(POIP))

poac <- pred.df %>% filter((POIP == min(POIP) | POIP == max(POIP) | POIP == median(POIP) |  POIP == quantile(POIP, 0.25) | POIP == quantile(POIP, 0.75)) &
                             (PORC == min(PORC) | PORC == max(PORC) | PORC == median(PORC) |  PORC == quantile(PORC, 0.25) | PORC == quantile(PORC, 0.75)))

ggplot(poac, aes(x = POAC_yr, y = pred, color = PORC)) + geom_point() + facet_wrap(.~as.factor(POIP))


poip <- pred.df %>% filter((PORC == min(PORC) | PORC == max(PORC) | PORC == median(PORC) |  PORC == quantile(PORC, 0.25) | PORC == quantile(PORC, 0.75)) &
                             (POAC_yr == min(POAC_yr) | POAC_yr == max(POAC_yr) | POAC_yr == median(POAC_yr) | POAC_yr == quantile(POAC_yr, 0.25) | POAC_yr == quantile(POAC_yr, 0.75)))

ggplot(poip, aes(x = POIP, y = pred, color = PORC)) + geom_point() + facet_wrap(.~as.factor(POAC_yr))

############


#
# porc <- pred.df %>% filter((POIP == min(POIP) | POIP == max(POIP) | POIP == median(POIP) | POIP == quantile(POIP, 0.25) | POIP == quantile(POIP, 0.75)) &
#                              (POAC_yr == min(POAC_yr) | POAC_yr == max(POAC_yr) | POAC_yr == median(POAC_yr) | POAC_yr == quantile(POAC_yr, 0.25) | POAC_yr == quantile(POAC_yr, 0.75)))
#
# ggplot(porc, aes(x = PORC, y = pred, color = POAC_yr)) + geom_point() + facet_wrap(.~as.factor(POIP))
#
# poac <- pred.df %>% filter((POIP == min(POIP) | POIP == max(POIP) | POIP == median(POIP) |  POIP == quantile(POIP, 0.25) | POIP == quantile(POIP, 0.75)) &
#                              (PORC == min(PORC) | PORC == max(PORC) | PORC == median(PORC) |  PORC == quantile(PORC, 0.25) | PORC == quantile(PORC, 0.75)))
#
# ggplot(poac, aes(x = POAC_yr, y = pred, color = PORC)) + geom_point() + facet_wrap(.~as.factor(POIP))
#
#
# poip <- pred.df %>% filter((PORC == min(PORC) | PORC == max(PORC) | PORC == median(PORC) |  PORC == quantile(PORC, 0.25) | PORC == quantile(PORC, 0.75)) &
#                              (POAC_yr == min(POAC_yr) | POAC_yr == max(POAC_yr) | POAC_yr == median(POAC_yr) | POAC_yr == quantile(POAC_yr, 0.25) | POAC_yr == quantile(POAC_yr, 0.75)))
#
# ggplot(poip, aes(x = POIP, y = pred, color = PORC)) + geom_point() + facet_wrap(.~as.factor(POAC_yr))


##########

adj.i <- 10
adj.a <- 1
adj.r <- .33

# adj.i <- 10
# adj.a <- 1000
# adj.r <- .25

cost.i <- adj.i * (80 * 790 * 100)
cost.a <- adj.a * (511.38 * 10)
cost.r <- adj.r * 10 * (77.17/(52 * 337/365) + (50.17 * (52/12)))

budget_constraint <- function(x) {
  # x[1] - init, x[2] - adhr, x[3] - retn
  cost.i * x[1] + cost.a * x[2] + cost.r * x[3]
}

# number of different budget constraint values to consider
n = 200

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


  # if (i == 1 & j == 36) {
  #   browser()
  # # }
  # c(runif(1, .02, .08), runif(1, 100, 1800), runif(1, 100, 1800)
  # #

  solnp <- solnp(c(.02, 100, 1000),
                 obj_fun,
                 eqfun=budget_constraint,
                 eqB=budget[i],
                 LB=c(0,0,0),
                 UB=c(0.10, 2000, 2000),
                 control = list(rho = 1,
                                outer.iter = 10000,
                                inner.iter = 8000,
                                tol =2e-7,
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
save(res, file = "optim_res.rda")

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

