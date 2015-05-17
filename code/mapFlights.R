library(dplyr)
library(maps)
library(ggplot2)
library(grid)
source('code/edgeMaker.R')

mapExt <<- data.frame('x' = c(-125,-100), 'y' = c(30,50))

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
  usamap <- ggplot2::map_data("state")
  
  # for now, only show path of flights departing DEN
  fd2 <- dplyr::filter(fd, Departure == 'DEN')
  
  # aggregate to get number of times flying each leg
  fd2$count <- 1
  fd2 <- fd2 %>% group_by(Departure, Arrival, D.Lat, D.Lon, A.Lat, A.Lon) %>%
    summarise(count = sum(count))
  
  # compute paths using edgeMaker
  
  ## TO DO:
  ## change to compute fPath on unique flights only
  
  fPath <- do.call(rbind, lapply(lapply(1:nrow(fd), function(i){edgeMaker(fd[i,],mapExt)}), 
                                 function(X) X))
  
  # plot USA w/ arrival cities as red points
  # can use size or color to depict the number of flights flown between two cities
  # this can be shown by commenting out/in the color and size lines under geom_segment
  # if using color, should use a different color gradient than the default
  
  gg <- ggplot() + geom_polygon(data = usamap, aes(x = long, y = lat, group = group)) + 
    geom_path(data = usamap, aes(x = long, y = lat, group = group), color = 'grey50') +
    geom_segment(data = fd2, aes(x = D.Lon, xend = A.Lon, y=D.Lat, yend = A.Lat, 
                                 color = count), size = 1.5, 
    #                            size = count), color = 'blue', 
                arrow = grid::arrow(length = unit(.5, 'cm'))) +
    geom_point(data = fd, aes(x = A.Lon, y = A.Lat), color = 'red',size = 4) +
    coord_cartesian(xlim = mapExt$x, ylim = mapExt$y) +
    geom_path(data = fPath, aes(x = x, y = y), color = 'blue')

  gg
}
