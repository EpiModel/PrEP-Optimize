#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-18 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s500 --export=ALL,SIMNO=500,NJOBS=18,NSIMS=500,POIP=0,PSPO=0.07,POAC=0,PADO=0.39,PORC=0,PDRO=10 runsim.sh
