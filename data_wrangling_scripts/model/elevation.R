##### Find elevation for cities and model_points #####
library(elevatr)

# return the elevation of a df of points
# df must have LON and LAT columns
# elevation is in meters
elevation <- function(df, 
                      prj_dd = "EPSG:4326") {
  val <- get_elev_point(df[,c("LON","LAT")], prj = prj_dd, src = "epqs")
  round(val$elevation, 2)
}

