rm(list=ls())

args = (commandArgs(trailingOnly=TRUE))
if(length(args) == 1{
  airport = args[1]
} else {
  cat('usage: Rscript hw4.R <template spectrum> <data directory>\n', file=stderr())
  stop()
}

print(airport)

data=airport.csv


model <- lm(data, data[,6] ~ data[,7,8])

capture.output(summary(model), airport.txt)