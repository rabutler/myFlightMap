library(dplyr)

# get the data and combine it
getFlightData <- function(xx = 'data/TestFlights.csv')
{
  fd <- read.csv(xx)
  latLon <- read.csv('data/LatLon.csv', comment.char = '#')
  
  # change from City to Departure, so we can easily join by Departure
  names(latLon)[1] <- 'Departure'
  
  fd <- dplyr::full_join(fd, latLon)
  # edit names so that can also have arrival lat lon
  fd$D.Lat <- fd$Lat
  fd$D.Lon <- fd$Lon
  fd$Lat <- fd$Lon <- NULL
  
  # repeat with Arrival
  names(latLon)[1] <- 'Arrival'
  fd <- dplyr::full_join(fd, latLon)
  fd$A.Lat <- fd$Lat
  fd$A.Lon <- fd$Lon
  fd$Lat <- fd$Lon <- NULL
  
  fd
}

# first go will be using ggplot

ggMap <- function()
{
  
}