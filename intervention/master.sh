#!/bin/bash

sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s500 --export=ALL,SIMNO=500,NJOBS=4,NSIMS=112,PSP=0 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s501 --export=ALL,SIMNO=501,NJOBS=4,NSIMS=112,PSP=0.25 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s502 --export=ALL,SIMNO=502,NJOBS=4,NSIMS=112,PSP=0.5 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s503 --export=ALL,SIMNO=503,NJOBS=4,NSIMS=112,PSP=0.6 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s504 --export=ALL,SIMNO=504,NJOBS=4,NSIMS=112,PSP=0.62 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s505 --export=ALL,SIMNO=505,NJOBS=4,NSIMS=112,PSP=0.64 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s506 --export=ALL,SIMNO=506,NJOBS=4,NSIMS=112,PSP=0.66 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s507 --export=ALL,SIMNO=507,NJOBS=4,NSIMS=112,PSP=0.68 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s508 --export=ALL,SIMNO=508,NJOBS=4,NSIMS=112,PSP=0.7 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s509 --export=ALL,SIMNO=509,NJOBS=4,NSIMS=112,PSP=0.72 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s510 --export=ALL,SIMNO=510,NJOBS=4,NSIMS=112,PSP=0.74 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s511 --export=ALL,SIMNO=511,NJOBS=4,NSIMS=112,PSP=0.76 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s512 --export=ALL,SIMNO=512,NJOBS=4,NSIMS=112,PSP=0.78 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-4 --nodes=1 --ntasks-per-node=28 --time=00:30:00 --mem=100G --job-name=s513 --export=ALL,SIMNO=513,NJOBS=4,NSIMS=112,PSP=0.8 runsim.sh
