#!/bin/bash

## MOX ##

# Send
scp renv.lock mox:/gscratch/csde/sjenness/poptim/
scp est/*.rda mox:/gscratch/csde/sjenness/poptim/est
scp intervention/sim.R intervention/runsim.sh intervention/master.* mox:/gscratch/csde/sjenness/poptim

# Receive
scp mox:/gscratch/csde/sjenness/poptim/data/*.rda intervention/data/

scp mox:/gscratch/csde/sjenness/poptim/data/sim.n100[1-9].rda intervention/data/

scp mox:/gscratch/csde/sjenness/poptim/data/*.rds analysis/data/
