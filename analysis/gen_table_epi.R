
##
## PrEP Optimization Model
## Epi Data for Tables
##

source("analysis/fx.R")

fn <- list.files("intervention/data/", full.names = TRUE)
load(fn[1])

sim.base <- truncate_sim(sim, at = 261)

load(fn[2])
sim.comp <- truncate_sim(sim, at = 261)

## Test fx


sim.base <- mutate_epi(sim.base, pCov = prepCurr / prepElig,
                                 pStopRate = (PrEPStopsRand +
                                                PrEPStopsRandOptim)/prepCurr)
'PrEPHighAdr'

calc_quants_prev(sim.base, "pStopRate", at = 520,
                 mult = 100, round = 1,
                 qnt.low = 0.025, qnt.high = 0.975)

calc_quants_ir(sim.base, "ir100",
               qnt.low = 0.025, qnt.high = 0.975, round = 2)

calc_quants_ci(sim.comp, "incid", mult = 1,
               qnt.low = 0.025, qnt.high = 0.975, round = 0)

calc_quants_ia(sim.base, sim.comp, "incid",
               qnt.low = 0.025, qnt.high = 0.975, nsims = 1000)

## Table 2

scenario_set <- list.files("intervention/data/", pattern = "sim.n80", full.names = TRUE)

qlow <- 0.025
qhigh <- 0.975

tFull <- list()
for (ii in 1:length(scenario_set)) {
  rr <- list()
  load(scenario_set[ii])
  sim.comp <- truncate_sim(sim, at = 261)
  sim.comp <- mutate_epi(sim.comp, pCov = prepCurr / prepElig,
                         pStopRate = (PrEPStopsRand +
                                        PrEPStopsRandOptim)/prepCurr)

  rr[[1]] <- calc_quants_prev(sim.comp, "pCov", at = 520,
                   mult = 100, round = 1,
                   qnt.low = 0.025, qnt.high = 0.975)
  rr[[2]] <- calc_quants_prev(sim.comp, "PrEPHighAdr", at = 520,
                              mult = 100, round = 1,
                              qnt.low = 0.025, qnt.high = 0.975)
  rr[[3]] <- calc_quants_prev(sim.comp, "pStopRate", at = 520,
                              mult = 100, round = 1,
                              qnt.low = 0.025, qnt.high = 0.975)
  rr[[4]] <- calc_quants_ir(sim.comp, "ir100",
                            qnt.low = 0.025, qnt.high = 0.975, round = 2)
  rr[[5]] <- calc_quants_ci(sim.comp, "incid", mult = 1,
                            qnt.low = 0.025, qnt.high = 0.975, round = 0)
  if (ii > 1) {
    temp <- calc_quants_ia(sim.base, sim.comp, "incid",
                           qnt.low = 0.025, qnt.high = 0.975, nsims = 1000)
    rr[[6]] <- temp$nia
    rr[[7]] <- temp$pia
  } else {
    rr[[6]] <- NA
    rr[[7]] <- NA
  }

  rr <- do.call("c", rr)
  tFull[[ii]] <- rr
}

tFull <- as.data.frame(do.call("cbind", tFull))
tFull

readr::write_csv(tFull, "analysis/T2-epi.csv")


## Supplemental Table

scenario_set <- list.files("intervention/data/", pattern = "sim.n81", full.names = TRUE)

tFull <- list()
for (ii in 1:length(scenario_set)) {
  rr <- list()
  load(scenario_set[ii])
  sim.comp <- truncate_sim(sim, at = 261)
  sim.comp <- mutate_epi(sim.comp, pCov = prepCurr / prepElig,
                         pStopRate = (PrEPStopsRand +
                                        PrEPStopsRandOptim)/prepCurr)

  rr[[1]] <- calc_quants_prev(sim.comp, "pCov", at = 520,
                              mult = 100, round = 1,
                              qnt.low = 0.025, qnt.high = 0.975)
  rr[[2]] <- calc_quants_prev(sim.comp, "PrEPHighAdr", at = 520,
                              mult = 100, round = 1,
                              qnt.low = 0.025, qnt.high = 0.975)
  rr[[3]] <- calc_quants_prev(sim.comp, "pStopRate", at = 520,
                              mult = 100, round = 1,
                              qnt.low = 0.025, qnt.high = 0.975)
  rr[[4]] <- calc_quants_ir(sim.comp, "ir100",
                            qnt.low = 0.025, qnt.high = 0.975, round = 2)
  rr[[5]] <- calc_quants_ci(sim.comp, "incid", mult = 1,
                            qnt.low = 0.025, qnt.high = 0.975, round = 0)
  if (ii > 1) {
    temp <- calc_quants_ia(sim.base, sim.comp, "incid",
                           qnt.low = 0.025, qnt.high = 0.975, nsims = 1000)
    rr[[6]] <- temp$nia
    rr[[7]] <- temp$pia
  } else {
    rr[[6]] <- NA
    rr[[7]] <- NA
  }

  rr <- do.call("c", rr)
  tFull[[ii]] <- rr
}

tFull <- as.data.frame(do.call("cbind", tFull))
tFull

readr::write_csv(tFull, "analysis/ST2-epi.csv")
