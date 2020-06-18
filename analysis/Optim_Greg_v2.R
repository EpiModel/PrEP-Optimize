library(dplyr)
library(psych)
library(ggplot2)
# package for generalized additive models (gam)
library(mgcv)
# package for optimization algorithms
library(Rsolnp)

# import simulation output data
df_prep <- readRDS("analysis/data/prepOptim-Yearly-v3-5000sets-100per.rds")

# translate weekly adherence capacity into yearly capacity
df_prep$POAC_yr <- df_prep$POAC*52

# calculate 10-year outcomes
df_prep_10yr <- full_join(df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(incid, infAvert),
                                         sum),
                          df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(PrEPHighAdr, prepElig, prepCurr),
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
df_prep_10yr <- df_prep_10yr %>% filter(POIP <= 0.05)

gam <- gam(data = df_prep_10yr,
           formula = infAvert ~ ti(POIP) + ti(POAC_yr) + ti(PORC) + ti(POIP, PORC) + ti(POIP, POAC_yr) + ti(POAC_yr, PORC),
           family = Gamma(link = "identity"))

summary(gam)
plot(gam)
gam$coefficients
gam$model

predict(gam, newdata = data.frame(POIP = .01, POAC_yr = 1000, PORC = 1000))
predict(gam, newdata = data.frame(POIP = 0, POAC_yr = 0, PORC = 0))


# gam(formula,family=gaussian(),data=list(),weights=NULL,subset=NULL,
#     na.action,offset=NULL,method="GCV.Cp",
#     optimizer=c("outer","newton"),control=list(),scale=0,
#     select=FALSE,knots=NULL,sp=NULL,min.sp=NULL,H=NULL,gamma=1,
#     fit=TRUE,paraPen=NULL,G=NULL,in.out,drop.unused.levels=TRUE,
#     drop.intercept=NULL,discrete=FALSE,...)


# Objective function: InfAvert~capacity parameters
# Predict from fitted model
# Optimization function seeks to minimize the objective function, so objective function will return the negative of the object function.
obj_fun <- function(x) {
  - predict(gam, newdata = data.frame(POIP = x[1], POAC_yr = x[2], PORC = x[3]))
}

# test objective function
obj_fun(x = c(.25, 1000, 1000))

# playing around with what values are needed to produce an interesting optimization result
adj.i <- 60
adj.a <- 1
adj.r <- 1

budget_constraint <- function(x) {
  # x[1] - init, x[2] - adhr, x[3] - retn
  adj.i * (5815659 / .2239268) * x[1] + adj.a * (983*10) * x[2] + adj.r * (679.25*10) * x[3]
}

# test budget constraint
budget_constraint(x = c(.01, 1000, 1000))

# test optimization function for single budget constraint value
#the optimization function - minimises by default
solnp <- solnp(c(.01, 100, 100), # starting values
               obj_fun, # function to optimise
               eqfun = budget_constraint, # budget constraint function
               eqB=9e7,   # the budget constraint
               LB=c(0,0,0), # lower bound for parameters i.e. greater than zero
               UB=c(0.5, 3588, 3199)) #upper bound for parameters

# parameters that yield maximum infections averted
solnp$pars
# indication of optimization convergence
solnp$convergence
# objective function output values for each iteration of optimization algorithm
solnp$values
# plugging optimal parameter values back into objective function to verify output
obj_fun(solnp$pars)


# number of different budget constraint values to consider
n = 100

# initialize optimization results data.frame
res <- data.frame(poip = rep(NA, n),
                  poac = rep(NA, n),
                  pocr = rep(NA, n),
                  infAvert = rep(NA, n),
                  budget = rep(NA, n),
                  converge = rep(NA, n))


# specify lower and upper bounds for sequence of budgets to consider
budget <- seq(from = 1e5, to = 9e7, length.out = n)

for (i in 1:n) {
  solnp <- solnp(c(.01, 100, 100), #starting values (random - obviously need to be positive and sum to 15)
                 obj_fun, #function to optimise
                 eqfun=budget_constraint, #equality function
                 eqB=budget[i],   #the equality constraint
                 LB=c(0,0,0), #lower bound for parameters i.e. greater than zero
                 UB=c(0.5, 3588, 3199)) #upper bound for parameters
  pars <- solnp$pars
  res$poip[i] <- pars[1]
  res$poac[i] <- pars[2]
  res$pocr[i] <- pars[3]
  res$infAvert[i] <- - last(solnp$values)
  res$converge[i] <- solnp$convergence
  res$budget[i] <- budget[i]
}

# discard optimizations that did not converge
res_plot <- res %>% filter(converge == 0)

# plots showing how optimal poip, poac, pocr, and infAvert change with budget constraint
ggplot(data = res_plot, aes(x = budget, y = poip)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = pocr)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = infAvert)) + geom_line()

# plots showing what fraction of budget is allocated to each intervention as a function of budget constraint
ggplot(data = res_plot, aes(x = budget, y = poip * adj.i * (5815659 / .2239268) / budget)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac * adj.a * (983*10) / budget)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = pocr * adj.r * (679.25*10) / budget)) + geom_line()

