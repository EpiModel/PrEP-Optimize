
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Environmental Arguments
pull_env_vars(num.vars = c("POIP", "PSPO", "POAC", "PADO", "PORC", "PDRO"))

## Parameters
netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
burnin <- readRDS("est/burnin.ATL.3race.FSonly.rda")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   trans.scale = c(2.21, 0.405, 0.255),
                   prep.start = (52*60) + 1,
                   riskh.start = 52*59,
                   prep.adhr.hr = c(0.69, 0.19, 0.01),
                   prep.start.prob = 0.715,
                   prep.adhr.dist = reallocate_pcp(c(0.089, 0.127, 0.784), -0.184),
                   prep.discont.rate = 1 - (2^(-1/(224.4/7))),
                   prep.require.lnt = TRUE,
                   prep.risk.reassess.method = "year",
                   prep.optim.start = (52*65) + 1,
                   prep.optim.init.prob = POIP,
                   prep.start.prob.optim = PSPO,
                   prep.optim.adhr.cap = POAC,
                   prep.adhr.dist.optim = reallocate_pcp(c(0.089, 0.127, 0.784), -0.184+PADO),
                   prep.optim.retn.cap = PORC,
                   prep.discont.rate.optim = 1 - (2^(-1/(PDRO*224.4/7))))
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
vars <- c("ir100", "incid", "num", "prepCurr", "prepElig",
          "OptimInitStarts", "OptimInitPrev",
          "PrEPStarts", "PrEPStartsOptim",
          "OptimAdhrStarts", "OptimAdhrPrev", "PrEPHighAdr",
          "OptimRetnStarts", "OptimRetnPrev",
          "PrEPStopsInd", "PrEPStopsRand", "PrEPStopsRandOptim")
process_simfiles(simno = simno, min.n = njobs, nsims = nsims,
                 truncate.at = 52*60, vars = vars)
