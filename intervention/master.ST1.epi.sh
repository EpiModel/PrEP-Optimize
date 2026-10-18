#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8100 --export=ALL,SIMNO=8100,NJOBS=9,NSIMS=252,POIP=0,PSPO=0.07,POAC=0,PADO=0.399,PORC=0,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8101 --export=ALL,SIMNO=8101,NJOBS=9,NSIMS=252,POIP=0,PSPO=0.07,POAC=0,PADO=0.399,PORC=634,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8102 --export=ALL,SIMNO=8102,NJOBS=9,NSIMS=252,POIP=0,PSPO=0.07,POAC=0.8,PADO=0.399,PORC=1261,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8103 --export=ALL,SIMNO=8103,NJOBS=9,NSIMS=252,POIP=0,PSPO=0.07,POAC=11.9,PADO=0.399,PORC=1289,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8104 --export=ALL,SIMNO=8104,NJOBS=9,NSIMS=252,POIP=0.011,PSPO=0.07,POAC=17.4,PADO=0.399,PORC=1323,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8105 --export=ALL,SIMNO=8105,NJOBS=9,NSIMS=252,POIP=0.03,PSPO=0.07,POAC=18,PADO=0.399,PORC=1364,PDRO=1.54 runsim.sh
