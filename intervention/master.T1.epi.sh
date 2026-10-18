#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8000 --export=ALL,SIMNO=8000,NJOBS=9,NSIMS=252,POIP=0,PSPO=0.07,POAC=0,PADO=0.399,PORC=0,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8001 --export=ALL,SIMNO=8001,NJOBS=9,NSIMS=252,POIP=0,PSPO=0.07,POAC=0,PADO=0.399,PORC=634,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8002 --export=ALL,SIMNO=8002,NJOBS=9,NSIMS=252,POIP=0.001,PSPO=0.07,POAC=0,PADO=0.399,PORC=1300,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8003 --export=ALL,SIMNO=8003,NJOBS=9,NSIMS=252,POIP=0.023,PSPO=0.07,POAC=0,PADO=0.399,PORC=1295,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8004 --export=ALL,SIMNO=8004,NJOBS=9,NSIMS=252,POIP=0.043,PSPO=0.07,POAC=0,PADO=0.399,PORC=1382,PDRO=1.54 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s8005 --export=ALL,SIMNO=8005,NJOBS=9,NSIMS=252,POIP=0.063,PSPO=0.07,POAC=0,PADO=0.399,PORC=1460,PDRO=1.54 runsim.sh
