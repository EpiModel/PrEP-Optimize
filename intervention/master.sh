#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-16 --nodes=1 --ntasks-per-node=16 --time=00:30:00 --mem=50G --job-name=s200 --export=ALL,SIMNO=200,NJOBS=16,NSIMS=250,POIP=0.25,PSPO=0.1,POAC=0,PADO=0.39,PORC=0,PDRO=10 runsim.sh
