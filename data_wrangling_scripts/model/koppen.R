##### Use kgc package to get koppen classifications for the points #####
library(kgc)

# return koppen classifications for a df of points with given resolution
# df must have LON and LAT columns
koppen_classification <- function(df,
                                  res = "course") {
  # round coords for kgc package
  df$rndCoord.lon <- RoundCoordinates(df$LON, res=res, latlong="lon")
  df$rndCoord.lat <- RoundCoordinates(df$LAT, res=res, latlong="lat")
  
  # get koppen classifications for the rounded cords
  koppen <- LookupCZ(df[,c("rndCoord.lon","rndCoord.lat")], res=res)
  
  # warning if any points are missing classification
  missing_val <- which(koppen == "Climate Zone info missing")
  if(length(missing_val) > 0) {
    print("WARNING: The following points (rows) are missing koppen value:")
    print(missing_val)
  }
  
  # return classifications
  as.character(koppen)
}
