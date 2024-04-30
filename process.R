rm(list=ls())

args = (commandArgs(trailingOnly=TRUE))
if(length(args) == 1){
  airport = args[1]
} else {
  cat('usage: Rscript process.R airport', file=stderr())
  stop()
}


print(airport)

airportData=read.csv(paste0(airport, ".csv"), header=FALSE, col.names=c("searchDate", "flightDate", "departureairport", "arrivalairport", "travelDuration", "totalfare", "seatsremaining", "totaltraveldistance", "segmentsAirlineName", "segmentsCabinCode"))

airportData$segmentsAirlineName <- strsplit(airportData[["segmentsAirlineName"]], "\\|\\|")
airportData$segmentsCabinCode <- strsplit(airportData[["segmentsCabinCode"]], "\\|\\|")

airportData$segmentsCabinCode <- sapply(airportData[["segmentsCabinCode"]], unique)
airportData$segmentsCabinCode <- sapply(airportData[["segmentsCabinCode"]], length)

airportData$numAirlines <- sapply(airportData[["segmentsAirlineName"]], unique)
airportData$numAirlines <- sapply(airportData[["segmentsAirlineName"]], length)

airportData$searchDate <- as.Date(airportData$searchDate)
airportData$flightDate <- as.Date(airportData$flightDate)

airportData$flightmonth <- format(airportData$flightDate, "%m")
airportData$searchmonth <- format(airportData$searchDate, "%m")
airportData$searchweekday <- weekdays(airportData$searchDate)
airportData$flightweekday <- weekdays(airportData$flightDate)  
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

averageprice=tapply(X=airportData$totalfare, INDEX=airportData$departureairport, FUN=mean, na.rm=TRUE)
print(averageprice)

dateprice=tapply(X=airportData$totalfare, INDEX=airportData$datedifference, FUN=mean, na.rm=TRUE)
print(dateprice)


airportdata <- na.omit(airportData)

correlations <- cor(airportdata[, sapply(airportdata,is.numeric)]) 
print(correlations) 

model <- lm(airportData$totalfare ~ airportData$seatsremaining+airportData$datedifference+airportData$travelDuration+airportData$searchweekday+airportData$flightweekday, data=airportData)

print(summary(model))

n <- length(airportData) 
testlength <- n*.2
trainlength <- n*.8
print(testlength)
print(trainlength)
traindata <- airportData[1:trainlength, ]
testdata <- airportData[(trainlength:trainlength+testlength), ]


trainmodel <- lm(airportData$totalfare ~ airportData$seatsremaining++airportData$flightmonth+airportData$datedifference+airportData$travelDuration+airportData$searchweekday+airportData$flightweekday, data=traindata)
print(summary(trainmodel))

predictions <- predict(trainmodel , newdata=testdata)

squared_errors <- (predictions - testdata$totalfare)^2
squared_errors <- squared_errors[!is.na(squared_errors)]  # Exclude NAs

print(sqrt(mean(squared_errors)))

rmse <- sqrt(mean(squared_errors))
print(rmse)


write.csv(rmse, "mytest.csv")

