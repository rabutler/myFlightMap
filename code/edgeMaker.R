# originally from https://gist.github.com/dsparks/4331058#file-tapered-intensity-curved_edges-r

# pExt is the overal plot extents

edgeMaker <- function(whichRow, pExt, len = 100, curved = TRUE)
{

  #fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  #toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  fromC = c(whichRow$D.Lon, whichRow$D.Lat)
  toC <- c(whichRow$A.Lon, whichRow$A.Lat)
  
  # Add curve:
  graphCenter <- colMeans(pExt)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
  }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
  
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Group <- paste(whichRow$Departure, whichRow$Arrival, sep = '-')
  return(edge)
}