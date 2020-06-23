library(dplyr)
library(psych)
library(ggplot2)
library(scam)
# package for generalized additive models (gam)
library(mgcv)
# package for optimization algorithms
library(Rsolnp)

# import simulation output data
setwd("C:/Users/Greg/Desktop/PrEP-Optimize")
df_prep <- readRDS("analysis/data/prepOptim-Yearly-v3-5000sets-100per.rds")
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
# k <- 4
# gam <- gam(data = df_prep_10yr,
#            formula = infAvert ~ ti(POIP, k = k) + ti(POAC_yr, k = k) + ti(PORC, k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k) + ti(POAC_yr, PORC, k = k),
#            family = Gamma(link = "identity"))

# SCAM (Shape Constrained Additive Model) specification
# bs = "cv" forces spline objective function to be concave
# this smoothness/shape constraint helps the optimization functions
k <- 4
scam <- scam(data = df_prep_10yr,
             formula = infAvert ~ s(POIP, bs = "cv", k = k) + s(POAC_yr, bs = "cv", k = k) + s(PORC, bs = "cv", k = k) + ti(POIP, PORC, k = k) + ti(POIP, POAC_yr, k = k),
             family = Gamma(link = "identity"))

# p <- 5
# glm <- glm(data = df_prep_10yr,
#            formula = infAvert ~ poly(POIP, p) + poly(POAC_yr, p) + poly(PORC, p) + poly(POIP*PORC, p) + poly(POIP*POAC_yr, p) + poly(POAC_yr*PORC, p),
#            family = Gamma(link = "identity"))

# summary(scam)
# vis.scam(scam, view = c("PORC", "POAC_yr"))
# vis.scam(scam, view = c("POIP", "PORC"))
# plot(scam)
# plot(residuals(scam))
# scam.check(scam)

# summary(gam)
# vis.gam(gam, view = c("PORC", "POAC_yr"))
# vis.gam(gam, view = c("POIP", "PORC"))
# plot(gam)
# plot(residuals(gam))
# gam.check(gam)

predict(scam, newdata = data.frame(POIP = .01, POAC_yr = 1000, PORC = 1000))
predict(scam, newdata = data.frame(POIP = 0, POAC_yr = 0, PORC = 0))

# Objective function: InfAvert~capacity parameters
# Predict from fitted model
# Optimization function seeks to minimize the objective function, so objective function will return the negative of the object function.
obj_fun <- function(x) {
  - predict(scam, newdata = data.frame(POIP = x[1], POAC_yr = x[2], PORC = x[3]))
}

# # test objective function
# obj_fun(x = c(.25, 1000, 1000))
#
# # playing around with what values are needed to produce an interesting optimization result
# adj.i <- 60
# adj.a <- 1
# adj.r <- 1
#
# budget_constraint <- function(x) {
#   # x[1] - init, x[2] - adhr, x[3] - retn
#   adj.i * (5815659 / .2239268) * x[1] + adj.a * (983*10) * x[2] + adj.r * (679.25*10) * x[3]
# }

# playing around with what values are needed to produce an interesting optimization result
# adj.i <- 10
# adj.a <- 1
# adj.r <- 1

adj.i <- 10
adj.a <- .75
adj.r <- 1.5

cost.i <- adj.i * (80 * 790 * 100)
cost.a <- adj.a * (511.38 * 10)
cost.r <- adj.r * 10 * (77.17/(52 * 337/365) + (50.17 * (52/12)))

budget_constraint <- function(x) {
  # x[1] - init, x[2] - adhr, x[3] - retn
  cost.i * x[1] + cost.a * x[2] + cost.r * x[3]
}

# UPTAKE $ CAP 316000 * adj
# ADHERENCE $ CAP 18348314 * adj
# RETENTION $ CAP 7006152 * adj


# test budget constraint
budget_constraint(x = c(.01, 100, 100))

# test optimization function for single budget constraint value
#the optimization function - minimises by default
solnp <- solnp(c(.01, 100, 100), # starting values
               obj_fun, # function to optimise
               eqfun = budget_constraint, # budget constraint function
               eqB=9e5,   # the budget constraint
               LB=c(0,0,0), # lower bound for parameters i.e. greater than zero
               UB=c(0.10, 2000, 2000),
               control = list(inner.iter = 800)) #upper bound for parameters

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
                  porc = rep(NA, n),
                  infAvert = rep(NA, n),
                  budget = rep(NA, n),
                  converge = rep(NA, n))


# specify lower and upper bounds for sequence of budgets to consider
budget <- seq(from = 9e5, to = 9e6, length.out = n)

for (i in 1:n) {
  solnp <- solnp(c(.01, 100, 100),
                 obj_fun, #function to optimise
                 eqfun=budget_constraint, #equality function
                 eqB=budget[i],   #the equality constraint
                 LB=c(0,0,0), #lower bound for parameters i.e. greater than zero
                 UB=c(0.10, 2000, 2000),
                 control = list(inner.iter = 800)) #upper bound for parameters
  # solnp <- solnp(c(.01, 100, 100),
  #                obj_fun, #function to optimise
  #                eqfun=budget_constraint, #equality function
  #                eqB=budget[i],   #the equality constraint
  #                LB=c(0,0,0), #lower bound for parameters i.e. greater than zero
  #                UB=c(0.05, 3588, 3199)) #upper bound for parameters
  pars <- solnp$pars
  res$poip[i] <- pars[1]
  res$poac[i] <- pars[2]
  res$porc[i] <- pars[3]
  res$infAvert[i] <- - last(solnp$values)
  res$converge[i] <- solnp$convergence
  res$budget[i] <- budget[i]
}

# discard optimizations that did not converge
res_plot <- res %>% filter(converge == 0)

# plots showing how optimal poip, poac, pocr, and infAvert change with budget constraint
ggplot(data = res_plot, aes(x = budget, y = poip)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = porc)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = infAvert)) + geom_line()

# plots showing what fraction of budget is allocated to each intervention as a function of budget constraint
ggplot(data = res_plot, aes(x = budget, y = poip * cost.i / budget)) +
  geom_line() +
  ylab("Proportion of Budget in Uptake")
ggplot(data = res_plot, aes(x = budget, y = poac * cost.a / budget)) +
  geom_line() +
  ylab("Proportion of Budget in Adherence")
ggplot(data = res_plot, aes(x = budget, y = porc * cost.r / budget)) +
  geom_line() +
  ylab("Proportion of Budget in Retention")

n = 101
grid.df <- expand.grid(POIP = seq(0, 0.10, length.out = n),
                       POAC_yr = seq(0, 2000, length.out = n),
                       PORC = seq(0, 2000, length.out = n))
pred <- predict(scam,
                newdata = grid.df)
pred.df <- cbind(pred, grid.df)

# ggplot(pred.df %>% filter(PORC == 0, POAC_yr == 0), aes(x = POIP, y = pred)) + geom_point() + ylim(0, 120)
# ggplot(pred.df %>% filter(POIP == 0, PORC == 0), aes(x = POAC_yr, y = pred)) + geom_point() + ylim(0, 120)
# ggplot(pred.df %>% filter(POIP == 0, POAC_yr == 0), aes(x = PORC, y = pred)) + geom_point() + ylim(0, 120)

# ggplot(pred.df %>% filter(PORC > 1000, PORC < 1100, POAC_yr == 0), aes(x = POIP, y = pred)) + geom_point() + ylim(0, 120)
# ggplot(pred.df %>% filter(POIP > 0.05, POIP < 0.055, POAC_yr == 0), aes(x = PORC, y = pred)) + geom_point() + ylim(0, 120)

porc <- pred.df %>% filter((POIP == min(POIP) | POIP == max(POIP) | POIP == median(POIP)) &
                             (POAC_yr == min(POAC_yr) | POAC_yr == max(POAC_yr) | POAC_yr == median(POAC_yr)))

ggplot(porc, aes(x = PORC, y = pred, color = POAC_yr)) + geom_point() + facet_wrap(.~as.factor(POIP))

poac <- pred.df %>% filter((POIP == min(POIP) | POIP == max(POIP) | POIP == median(POIP)) &
                             (PORC == min(PORC) | PORC == max(PORC) | PORC == median(PORC)))

ggplot(poac, aes(x = POAC_yr, y = pred, color = PORC)) + geom_point() + facet_wrap(.~as.factor(POIP))


poip <- pred.df %>% filter((PORC == min(PORC) | PORC == max(PORC) | PORC == median(PORC)) &
                             (POAC_yr == min(POAC_yr) | POAC_yr == max(POAC_yr) | POAC_yr == median(POAC_yr)))

ggplot(poip, aes(x = POIP, y = pred, color = PORC)) + geom_point() + facet_wrap(.~as.factor(POAC_yr))

