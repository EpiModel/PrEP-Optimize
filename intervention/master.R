
## build master.sh script ##

library("EpiModelHPC")

# Reference Scenario ---------------------------------------------

vars <- list(POIP = 0,
             PSPO = 0.07,
             POAC = 0,
             PADO = 0.399,
             PORC = 0,
             PDRO = 1.54)
# vars <- NULL
sbatch_master(vars = vars,
              working.dir = "intervention/",
              master.file = "master.base.sh",
              simno.start = 0,
              ckpt = FALSE,
              nsims = 252,
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


# Counterfactual Scenario Revisions: June 2020 ----------------------------

# Limited counterfactual ranges by on suggestion by GK

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

POIP <- c(0, 0.10)
POAC <- c(0, 40)
PORC <- c(0, 2000)

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

save(vars, file = "intervention/cfVars-5k.rda")
load("intervention/cfVars-5k.rda")

simno <- 800:1000

sbatch_master(vars = vars[simno, ],
              expand.vars = FALSE,
              working.dir = "intervention/",
              master.file = "master.lhs.curr.sh",
              runsim.file = "runsim.sh",
              simno.start = min(simno),
              append = FALSE,
              ckpt = FALSE,
              nsims = 252,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Generate Epi Sims for Fitted Budget Optim Values ------------------------
# October 2020

# Primary table scenarios

POIPs <- c(0, 0,   0.1/100, 2.3/100, 4.3/100, 6.3/100)
POACs <- c(0, 0,   0,       0,       0,       0)
PORCs <- c(0, 634, 1300,    1295,    1382,    1460)

vars <- list(POIP = POIPs,
             PSPO = 0.07,
             POAC = POACs,
             PADO = 0.399,
             PORC = PORCs,
             PDRO = 1.54)
vars <- as.data.frame(vars)
vars

sbatch_master(vars = vars,
              expand.vars = FALSE,
              working.dir = "intervention/",
              master.file = "master.T1.epi.sh",
              build.runsim = TRUE,
              runsim.file = "runsim.sh",
              env.file = "loadR.sh",
              simno.start = 8000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 252,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Supplemental table scenarios

POIPs <- c(0, 0,   0,    0,    1.1/100, 3.0/100)
POACs <- c(0, 0,   0.8,  11.9, 17.4,    18.0)
PORCs <- c(0, 634, 1261, 1289, 1323,    1364)

vars <- list(POIP = POIPs,
             PSPO = 0.07,
             POAC = POACs,
             PADO = 0.399,
             PORC = PORCs,
             PDRO = 1.54)
vars <- as.data.frame(vars)
vars

sbatch_master(vars = vars,
              expand.vars = FALSE,
              working.dir = "intervention/",
              master.file = "master.ST1.epi.sh",
              build.runsim = TRUE,
              runsim.file = "runsim.sh",
              env.file = "loadR.sh",
              simno.start = 8100,
              append = FALSE,
              ckpt = TRUE,
              nsims = 252,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
