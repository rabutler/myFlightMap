library(dplyr)
library(maps)
library(ggplot2)

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
  fd <- getFlightData()
  usamap <- map_data("state")
  
  # plot USA w/ arrival cities as red points
  gg <- ggplot() + geom_polygon(data = usamap, aes(x = long, y = lat, group = group)) + 
    geom_path(data = usamap, aes(x = long, y = lat, group = group), color = 'grey50') +
    geom_point(data = fd, aes(x = A.Lon, y = A.Lat), color = 'red',size = 4)

  gg
}
