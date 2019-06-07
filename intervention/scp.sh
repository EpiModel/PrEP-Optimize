#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/poptim/est
scp intervention/sim.R intervention/runsim.sh intervention/master.lhs.sh mox:/gscratch/csde/sjenness/poptim

# Receive
scp mox:/gscratch/csde/sjenness/poptim/data/*.rda intervention/data/


## IKT ##

# Send
scp est/*.rda hyak:/gscratch/csde/sjenness/poptim/est
scp intervention/sim.R intervention/runsim.sh intervention/master.sh hyak:/gscratch/csde/sjenness/poptim

# Receive
scp hyak:/gscratch/csde/sjenness/poptim/data/*.rda intervention/data/

