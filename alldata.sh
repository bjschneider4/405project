#!/bin/bash

itinerarie=$1

tar -xzf R413.tar.gz

tar -xzf packages_FITSio_tidyverse.tar.gz

# and the working directory as its home location
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages


Rscript alldata.R $1
