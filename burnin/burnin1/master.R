
## build master.sh script ##

library("EpiModelHPC")

# Test stability of run
vars <- NULL
sbatch_master(vars = vars,
              master.file = "burnin/burninFS/master.sh",
              simno.start = 100,
              ckpt = TRUE,
              nsims = 200,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# Big run batch for model selection
sbatch_master(vars = vars,
              master.file = "burnin/burninFS/master.sh",
              simno.start = 200,
              ckpt = TRUE,
              nsims = 20000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
