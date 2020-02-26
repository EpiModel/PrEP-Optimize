
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
              master.file = "master.sh",
              simno.start = 500,
              ckpt = TRUE,
              nsims = 112,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Counterfactual Scenarios ---------------------------------------------

# Ranges
# POIP: 0-0.50
# PSPO: 0.07
# POAC: 0-70
# PADO: 0.399
# PORC: 0-3200
# PDRO: 1.54

library("lhs")

set.seed(234567)
total.set.size <- 5000
l <- randomLHS(total.set.size, 3)

POIP <- c(0, 0.50)
POAC <- c(0, 70)
PORC <- c(0, 3200)

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
str(vars)

simno <- 5500:5999

sbatch_master(vars = vars[simno - 999, ],
              expand.vars = FALSE,
              working.dir = "intervention/",
              master.file = "master.lhs.curr.sh",
              runsim.file = "runsim.sh",
              simno.start = min(simno),
              append = FALSE,
              ckpt = TRUE,
              nsims = 112,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
