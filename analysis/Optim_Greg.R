library(Rsolnp)
library(ggplot2)

# Objective function: InfAvert~capacity parameters
# Predict from fitted model
# obj_fun <- function(x) {
#   - predict(L3, newdata = data.frame(POIP = x[1], POAC_yr = x[2], PORC = x[3]))
# }

obj_fun <- function(x) {
  - predict(gam, newdata = data.frame(POIP = x[1], POAC_yr = x[2], PORC = x[3]))
}
obj_fun(x = c(.25, 1000, 1000))
#specify the equality function. The number 15 (to which the function is equal)
#is specified as an additional argument

# rough approximation from cea
# budget_constraint <- function(x) {
#  (5815659 / .2239268) * x[1] + (983*10) * x[2] + (679.25*10) * x[3] # 1 - init, 2 - adhr, 3 - retn
# }

# playing around with what values are needed to produce an interesting optimization result

# adj.i <- 7
# adj.a <- .1
# adj.r <- .1

adj.i <- 60
adj.a <- 1
adj.r <- 1

budget_constraint <- function(x) {
  adj.i * (5815659 / .2239268) * x[1] + adj.a * (983*10) * x[2] + adj.r * (679.25*10) * x[3] # 1 - init, 2 - adhr, 3 - retn
}
budget_constraint(x = c(.01, 100, 100))

#the optimiser - minimises by default
solnp <- solnp(c(.01, 100, 100), #starting values (random - obviously need to be positive and sum to 15)
               obj_fun, #function to optimise
               eqfun=budget_constraint, #equality function
               eqB=9e7,   #the equality constraint
               LB=c(0,0,0), #lower bound for parameters i.e. greater than zero
               UB=c(0.5, 3588, 3199)) #upper bound for parameters
solnp$pars
solnp$convergence
solnp$values
obj_fun(solnp$pars)

# # init monthly fixed
# 12 * 1659.54 * 10
#
# # init pp
# 93.28
n = 100
res <- data.frame(poip = rep(NA, n),
                   poac = rep(NA, n),
                   pocr = rep(NA, n),
                   infAvert = rep(NA, n),
                   budget = rep(NA, n),
                   converge = rep(NA, n))
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

library(dplyr)
res_plot <- res %>% filter(converge == 0)

ggplot(data = res_plot, aes(x = budget, y = poip)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = pocr)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = infAvert)) + geom_line()

ggplot(data = res_plot, aes(x = budget, y = poip * adj.i * (5815659 / .2239268) / budget)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = poac * adj.a * (983*10) / budget)) + geom_line()
ggplot(data = res_plot, aes(x = budget, y = pocr * adj.r * (679.25*10) / budget)) + geom_line()

# Intervention costs
# prep.optim.init.init.pp = 93.28
# prep.optim.init.monthly.fixed = 1659.54
# prep.optim.adhr.init.pp = 983.39
# prep.optim.retn.init.pp = 679.25
# prep.optim.retn.monthly.fixed = 1325.24
