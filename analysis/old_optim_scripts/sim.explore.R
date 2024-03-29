
## PrEP-Optim Intervention File Exploration

library("EpiModelHIV")
source("analysis/fx.R")
sim <- NULL
# unlink("data/*.rda")

fn <- list.files("intervention/data/", pattern = "sim", full.names = TRUE)
cbind(fn)

load("intervention/data/sim.n200.rda")

sim$param$prep.start
sim$param$prep.optim.start
sim$param$prep.start.prob
sim$param$prep.optim.start
sim$param$prep.optim.init.prob
sim$param$prep.start.prob.optim
sim$param$prep.optim.adhr.cap
sim$param$prep.adhr.dist
sim$param$prep.adhr.dist.optim
sim$param$prep.adhr.hr
sim$param$prep.discont.rate
sim$param$prep.optim.retn.cap
sim$param$prep.discont.rate.optim

1 - (2^(-1/(1*224.4/7)))

sim <- mutate_epi(sim, pFrac = prepCurr / prepElig,
                       pFracA = prepCurr / num,
                       pFracAO = OptimAdhrPrev / prepCurr,
                       pFracRO = OptimRetnPrev / prepCurr,
                       PrEPStopsRandT = PrEPStopsRand + PrEPStopsRandOptim)
sim <- mutate_epi(sim, psrr = PrEPStopsRand / (prepCurr - OptimRetnPrev),
                       psrro = PrEPStopsRandOptim / OptimRetnPrev)
df <- as.data.frame(sim, out = "mean")
names(df)

# Single scenario

par(mar = c(3,3,1,1), mgp = c(2,1,0))

plot(sim, y = "ir100", ylim = c(0, 5), mean.smooth = FALSE, mean.lwd = 1, mean.col = "black")
abline(v = 52*5, h = 1.29, lty = 2)
calc_quants_ir(sim, "ir100")
sum(df$incid)
sum(tail(df$incid, 520))
# (1169.184-sum(tail(df$incid, 520)))/1169.184

plot(sim, y = "num", ylim = c(0, 20000))

plot(sim, y = "prepElig", ylim = c(0, 10000))
plot(sim, y = "prepCurr")
calc_quants_prev(sim, "prepCurr", at = 781, mult = 1)
plot(sim, y = c("pFrac", "pFracA"), ylim = c(0, 1), legend = TRUE, mean.smooth = FALSE)
abline(v = 52*5, h = 0.15, lty = 2)
calc_quants_prev(sim, "pFrac", at = 781, mult = 100)

# Cumulative probabilty of starting PrEP per-year in base group and intervention group
1-(1-mean(tail(df$PrEPStarts / (df$prepElig - df$OptimInitPrev), 52)))^52
1-(1-mean(tail(df$PrEPStartsOptim / df$OptimInitPrev, 52)))^52

plot(sim, y = "OptimInitStarts")
plot(sim, y = "OptimInitStarts", mean.smooth = FALSE)
calc_quants_ir(sim, "OptimInitStarts")
plot(sim, y = "OptimInitPrev")
plot(sim, y = "PrEPStarts")
calc_quants_ir(sim, "PrEPStarts", round = 1)
plot(sim, y = "PrEPStartsOptim")
calc_quants_ir(sim, "PrEPStartsOptim", round = 1)

plot(sim, y = "OptimAdhrStarts", mean.smooth = FALSE)
calc_quants_ir(sim, "OptimAdhrStarts", round = 1)
plot(sim, y = "OptimAdhrPrev")
plot(sim, y = "pFracAO")
calc_quants_prev(sim, "pFracAO", at = 781, mult = 100)
plot(sim, y = "PrEPHighAdr", ylim = c(0, 1))
abline(v = 52*5, h = 0.600, lty = 2)
calc_quants_prev(sim, "PrEPHighAdr", at = 781, mult = 100)

plot(sim, y = "OptimRetnStarts")
plot(sim, y = "OptimRetnStarts", mean.smooth = FALSE)
df$OptimRetnStarts
plot(sim, y = "OptimRetnPrev")
plot(sim, y = "pFracRO")
calc_quants_prev(sim, "pFracRO", at = 781, mult = 100)

plot(sim, y = "PrEPStopsInd", ylim = c(0, 20))
calc_quants_ir(sim, "PrEPStopsInd", round = 1)
plot(sim, y = "PrEPStopsRand")
calc_quants_ir(sim, "PrEPStopsRand", round = 1)
plot(sim, y = "PrEPStopsRandOptim")
calc_quants_ir(sim, "PrEPStopsRandOptim")
plot(sim, y = "PrEPStopsRandT")
calc_quants_ir(sim, "PrEPStopsRandT")

plot(sim, y = c("psrr", "psrro"))
calc_quants_ir(sim, "psrr", round = 4)
calc_quants_ir(sim, "psrro", round = 4)


ag <- reallocate_pcp(c(0.089, 0.127, 0.784), -0.184+0)
hr <- c(0.69, 0.19, 0.01)

ag[1]*hr[1] + ag[2]*hr[2] + ag[3]*hr[3]

ag <- c(0.211, 0.07, 0.1, 0.619)
hr <- c(1, 1-0.31, 1-0.81, 1-0.95)

ag[1]*hr[1] + ag[2]*hr[2] + ag[3]*hr[3] + ag[4]*hr[4]
