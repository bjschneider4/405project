rm(list=ls())

args = (commandArgs(trailingOnly=TRUE))
if(length(args) == 1){
  airport = args[1]
} else {
  cat('usage: Rscript hw4.R <template spectrum> <data directory>\n', file=stderr())
  stop()
}


print(airport)

airportData=read.csv(paste0(airport, ".csv"), header=FALSE, col.names=c("searchDate", "flightDate", "departureairport", "arrivalairport", "travelDuration", "totalfare", "seatsremaining", "totaltraveldistance", "segmentsAirlineName", "segmentsCabinCode"))

airportData$segmentsAirlineName <- strsplit(airportData[["segmentsAirlineName"]], "\\|\\|")
airportData$segmentsCabinCode <- strsplit(airportData[["segmentsCabinCode"]], "\\|\\|")

airportData$segments <- sapply(airportData[["segmentsCabinCode"]], length)

airportData$searchDate <- as.Date(airportData$searchDate)
airportData$flightDate <- as.Date(airportData$flightDate)

airportData$datedifference <- airportData$flightDate - airportData$searchDate
travelTime <- function(inp) {
hpos <- regexpr("\\d+H", inp)
mpos <- regexpr("\\d+M", inp)
hours <- regmatches(inp, hpos)
minutes <- regmatches(inp,mpos)
total = as.numeric(substring(hours, 1, nchar(hours)-1)) * 60 + as.numeric(substring(minutes, 1, nchar(minutes)-1))
return(total)
}
airportData$travelDuration <- sapply(airportData[["travelDuration"]], travelTime)

airportData$travelDuration <- as.numeric(airportData$travelDuration)

print(head(airportData))

prices=tapply(X=airportData$totalfare, INDEX=airportData$arrivalairport, FUN=mean, na.rm=TRUE)
print(prices)

model <- lm(airportData$totalfare ~ airportData$seatsremaining+airportData$segments+airportData$datedifference+airportData$travelDuration, data=airportData)

print(summary(model))

#capture.output(summary(model), paste0(airport, ".txt")