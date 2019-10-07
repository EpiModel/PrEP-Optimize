
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Environmental Arguments
pull_env_vars(num.vars = "PSP")

## Parameters
netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
burnin <- readRDS("est/burnin.ATL.3race.FSonly.rda")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   trans.scale = c(2.21, 0.405, 0.255),
                   prep.start = (52*60) + 1,
                   riskh.start = 52*59,
                   prep.start.prob = PSP, # 0.712,
                   prep.require.lnt = TRUE,
                   prep.risk.reassess.method = "year",

                   prep.optim.start = Inf, #(52*65) + 1,
                   prep.optim.init.prob = 0,
                   prep.start.prob.optim = 0.01,
                   prep.optim.adhr.cap = 0,
                   prep.adhr.dist.optim = reallocate_pcp(c(0.089, 0.127, 0.784), 0.2),
                   prep.optim.retn.cap = 0,
                   prep.discont.rate.optim = 1 - (2^(-1/(2*224.4237/7))))
init <- init_msm()
control <- control_msm(simno = fsimno,
                       start = (52*60) + 1,
                       nsteps = 52*75,
                       nsims = ncores,
                       ncores = ncores,
                       initialize.FUN = reinit_msm,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE)

## Simulation
sim <- netsim(burnin, param, init, control)

# Merging
savesim(sim, save.min = TRUE, save.max = FALSE)
process_simfiles(simno = simno, min.n = njobs, nsims = nsims,
                 truncate.at = 52*60)
