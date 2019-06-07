#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-9 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s1000 --export=ALL,SIMNO=1000,NJOBS=9,NSIMS=250,PSP=0.66,PHA=0,PRD=224.4237 runsim.sh
