#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s500 --export=ALL,SIMNO=500,NJOBS=9,NSIMS=250,POIP=0.25,PSPO=0.25,POAC=0,PADO=0.39,PORC=2500,PDRO=10 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s501 --export=ALL,SIMNO=501,NJOBS=9,NSIMS=250,POIP=0.25,PSPO=0.25,POAC=0,PADO=0.39,PORC=5000,PDRO=10 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s502 --export=ALL,SIMNO=502,NJOBS=9,NSIMS=250,POIP=0.25,PSPO=0.25,POAC=0,PADO=0.39,PORC=2500,PDRO=20 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s503 --export=ALL,SIMNO=503,NJOBS=9,NSIMS=250,POIP=0.25,PSPO=0.25,POAC=0,PADO=0.39,PORC=5000,PDRO=20 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s504 --export=ALL,SIMNO=504,NJOBS=9,NSIMS=250,POIP=0.25,PSPO=0.25,POAC=0,PADO=0.39,PORC=2500,PDRO=100 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s505 --export=ALL,SIMNO=505,NJOBS=9,NSIMS=250,POIP=0.25,PSPO=0.25,POAC=0,PADO=0.39,PORC=5000,PDRO=100 runsim.sh
