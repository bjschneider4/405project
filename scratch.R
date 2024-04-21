airportData <- read.csv("path_to_csv.csv")

## Reshaping for only the columns we want to keep
## We can take this out if we want to do this in shell instead
keep = c("searchDate", "flightDate", "destinationAirport","travelDuration","seatsRemaining","segmentsCabinCode","totalTravelDistance","segmentsAirlineName","totalFare")
airportData <- airportData[, (names(airportData) %in% keep)]

## Converting segmentsAirlineName and segmentsCabinCode from strings delimitted by || to lists
airportData$segmentsAirlineName <- strsplit(airportData[["segmentsAirlineName"]], "\\|\\|")
airportData$segmentsCabinCode <- strsplit(airportData[["segmentsCabinCode"]], "\\|\\|")

## Making a new column segments to represent how many segments or journeys total on the flight as an integer
airportData$segments <- sapply(airportData[["segmentsCabinCode"]], length)

## Turns values from PTxHxM into numeric number of minutes using regex
travelTime <- function(inp) {
  hpos <- regexpr("\\d+H", inp)
  mpos <- regexpr("\\d+M", inp)
  hours <- regmatches(inp, hpos)
  minutes <- regmatches(inp,mpos)
  total = as.numeric(substring(hours, 1, nchar(hours)-1)) * 60 + as.numeric(substring(minutes, 1, nchar(minutes)-1))
  return(total)
}

## Applying previous function to travelDuration so that travelDuration is an integer of number of elapsed minutes
airportData$travelDuration <- sapply(airportData[["travelDuration"]], travelTime)

## Converts all dates from strings to date objects in R
airportData$searchDate <- as.Date(airportData[["searchDate"]])
airportData$flightDate <- as.Date(airportData[["flightDate"]])
