#!/bin/bash


airport=$1
echo $airport
cat itineraries.csv  | tail -n +2 | cut -d ',' -f2,3,4,5,7,14,15,27,23,13 | awk -v airport="$airport" -F ',' '{ if ($3 == airport) {print $0} }'   > ${airport}.csv
 
echo "Done" 

# untar your R installation
tar -xzf R413.tar.gz

# make sure the script will use your R installation, 
# and the working directory as its home location
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages



Rscript process.R $1
