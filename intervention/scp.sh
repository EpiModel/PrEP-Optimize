#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/poptim/est
scp intervention/sim.R intervention/runsim.sh intervention/master.sh mox:/gscratch/csde/sjenness/poptim

# Receive
scp mox:/gscratch/csde/sjenness/poptim/data/*.rda analysis/data/

scp mox:/gscratch/csde/sjenness/poptim/data/sim.n503.rda analysis/data/

scp mox:/gscratch/csde/sjenness/poptim/data/hold/prepOptim*.rda analysis/data/

