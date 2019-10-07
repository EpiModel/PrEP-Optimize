
## build master.sh script ##

library("EpiModelHPC")
setwd("intervention/")

# Reference Scenario ---------------------------------------------

# vars <- list(PSP = 0.66,
#              PHA = 0,
#              PRD = 224.4237)
vars <- list(PSP = c(0, 0.25, 0.5, seq(0.6, 0.8, 0.02)))
# vars <- NULL
sbatch_master(vars = vars,
              master.file = "master.sh",
              runsim.file = "runsim.sh",
              simno.start = 500,
              ckpt = TRUE,
              nsims = 112,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Counterfactual Scenarios ---------------------------------------------

library("lhs")

set.seed(12345)
l <- randomLHS(1000, 3)

PSP <- c(0.66, 1.00)
PHA <- c(0.00, 0.216)
PRD <- c(224.4237*1, 224.4237*4)

PSPs <- (l[, 1]*(PSP[2]-PSP[1]))+PSP[1]
PHAs <- (l[, 2]*(PHA[2]-PHA[1]))+PHA[1]
PRDs <- (l[, 3]*(PRD[2]-PRD[1]))+PRD[1]

vars <- list(PSP = PSPs,
             PHA = PHAs,
             PRD = PRDs)
vars

sbatch_master(vars = vars,
              expand.vars = FALSE,
              master.file = "intervention/master.lhs.sh",
              runsim.file = "runsim.sh",
              param.sheet = "intervention/params.csv",
              simno.start = 1001,
              append = FALSE,
              ckpt = TRUE,
              nsims = 500,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")



