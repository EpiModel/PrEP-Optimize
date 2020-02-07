#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/poptim/est
scp intervention/sim.R intervention/runsim.sh intervention/master.* mox:/gscratch/csde/sjenness/poptim

# Receive
scp mox:/gscratch/csde/sjenness/poptim/data/*.rda intervention/data/

scp mox:/gscratch/csde/sjenness/poptim/data/sim.n100[1-9].rda intervention/data/


## IKT ##

# Send
scp est/*.rda hyak:/gscratch/csde/sjenness/poptim/est
scp intervention/sim.R intervention/runsim.sh intervention/master.sh hyak:/gscratch/csde/sjenness/poptim

# Receive
scp hyak:/gscratch/csde/sjenness/poptim/data/*.rda intervention/data/

scp hyak:/gscratch/csde/sjenness/poptim/data/sim.n1[0-9][0-9].rda intervention/data
