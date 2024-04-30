rm(list=ls())
if (require("tidyverse")) {
  print("Loaded package tidyverse.")
} else {
  print("Failed to load package tidyverse.")  
}


args = (commandArgs(trailingOnly=TRUE))
if(length(args) == 1){
  itineraries = args[1]
} else {
  cat('usage: Rscript process.R airport', file=stderr())
  stop()
}

airportData=read.csv(paste0(itineraries, ".csv"))

print(head(airportData))

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


weekly_prices <- airportData %>% group_by(searchweekday, flightweekday) %>% summarize(avgfare = mean(totalFare, na.rm=TRUE), .groups= 'drop') %>% arrange(searchweekday, flightweekday)

flight_weekly_prices <- airportData %>% group_by(flightweekday) %>% summarize(avgfare = mean(totalFare, na.rm=TRUE), .groups= 'drop') %>% arrange(flightweekday)

search_weekly_prices <- airportData %>% group_by(searchweekday) %>% summarize(avgfare = mean(totalFare, na.rm=TRUE), .groups= 'drop') %>% arrange(searchweekday)

options(max.print= 1000)
print(flight_weekly_prices)
print(weekly_prices)
print(search_weekly_prices)

write.csv(weekly_prices, "data.csv")


ggplot(weekly_prices, aes(x=searchweekday, y =flightweekday, fill=avgfare) + geom_tile() + scale_fill_gradient(low="blue", high="red") + labs(title = "Average Flight Prices by Search and Flight Weekday", x = "Search Weekday", y="Flight Weekday", fill = "Average Fare (USD)") + them(axis.text.x = eleement_text(angle=45))


