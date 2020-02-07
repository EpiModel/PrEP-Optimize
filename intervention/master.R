
## build master.sh script ##

library("EpiModelHPC")

# Reference Scenario ---------------------------------------------

vars <- list(POIP = 0,
             PSPO = 0.07,
             POAC = 0,
             PADO = 0.39,
             PORC = 0,
             PDRO = 10)
# vars <- NULL
sbatch_master(vars = vars,
              working.dir = "intervention/",
              master.file = "master.ref.sh",
              simno.start = 500,
              ckpt = TRUE,
              nsims = 500,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Counterfactual Scenarios ---------------------------------------------

# Ranges
# POIP: 0-0.50
# PSPO: 0.07
# POAC: 0-10000
# PADO: 0.399
# PORC: 0-10000
# PDRO: 1.54

library("lhs")

set.seed(12345)
l <- randomLHS(1000, 3)

POIP <- c(0, 0.50)
POAC <- c(0, 10000)
PORC <- c(0, 10000)

POIPs <- round((l[, 1]*(POIP[2]-POIP[1]))+POIP[1], 4)
POACs <- floor((l[, 2]*(POAC[2]-POAC[1]))+POAC[1])
PORCs <- floor((l[, 3]*(PORC[2]-PORC[1]))+PORC[1])

vars <- list(POIP = POIPs,
             PSPO = 0.07,
             POAC = POACs,
             PADO = 0.399,
             PORC = PORCs,
             PDRO = 1.54)
vars <- as.data.frame(vars)
head(as.data.frame(vars))

sbatch_master(vars = vars,
              expand.vars = FALSE,
              working.dir = "intervention/",
              master.file = "master.lhs.sh",
              runsim.file = "runsim.sh",
              param.file = "params.lhs.csv",
              simno.start = 1000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 500,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

sbatch_master(vars = vars[500:1000, ],
              expand.vars = FALSE,
              working.dir = "intervention/",
              master.file = "master.lhs2.sh",
              runsim.file = "runsim.sh",
              # param.file = "params.lhs.csv",
              simno.start = 1500,
              append = FALSE,
              ckpt = FALSE,
              nsims = 100,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
