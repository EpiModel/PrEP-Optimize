#!/bin/bash

sbatch -p csde -A csde --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s0 --export=ALL,SIMNO=0,NJOBS=9,NSIMS=252,POIP=0,PSPO=0.07,POAC=0,PADO=0.399,PORC=0,PDRO=1.54 runsim.sh
