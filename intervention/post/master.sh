#!/bin/bash

sbatch -p csde -A csde --nodes=1 --ntasks-per-node=40 --time=01:00:00 --mem=100G --job-name=buildData runsim.sh
