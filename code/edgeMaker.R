# originally from https://gist.github.com/dsparks/4331058#file-tapered-intensity-curved_edges-r
# http://is-r.tumblr.com/post/38459242505/beautiful-network-diagrams-with-ggplot2

library(Hmisc)

# pExt is the overal plot extents

edgeMaker <- function(whichRow, pExt, len = 100, curved = TRUE)
{

  #fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  #toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  fromC = c(whichRow$D.Lon, whichRow$D.Lat)
  toC <- c(whichRow$A.Lon, whichRow$A.Lat)
  #browser()
  # Add curve:
  graphCenter <- colMeans(pExt)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  # removed the re-set of bezierMid so that curves go a different direction for departing from
  # a city than returning to the same city
  #if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
  #  bezierMid <- c(toC[1], fromC[2])
  #}  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
  
  edge <- data.frame(Hmisc::bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Group <- paste(whichRow$Departure, whichRow$Arrival, sep = '-')
  return(edge)
}