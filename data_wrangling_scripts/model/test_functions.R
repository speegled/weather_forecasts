


source("data_wrangling_scripts/model/koppen.R")
source("data_wrangling_scripts/model/elevation.R")
source("data_wrangling_scripts/model/elevation_change.R")
source("data_wrangling_scripts/model/distance_to_coast.R")
source("data_wrangling_scripts/model/wind.R")


scrapeCityData <- function(df, n) {
  print("Getting koppen classifications...")
  df$KOPPEN <- koppen_classification(df)
  print("Finished getting koppen classifications")
  
  print("Getting elevations...")
  df$ELEVATION <- elevation(df)
  print("Finished getting elevations")
  
  print("Getting max elevation changes...")
  df$ELEVATION_CHANGE <- citiesElevationChange(n, df)
  print("Finished getting max elevation changes")
  
  print("Getting distance to coast...")
  df$DISTANCE_TO_COAST <- distanceToCoast(df)
  print("Finished getting distance to coast")
  
  print("Getting mean wind speeds...")
  df$WIND <- meanWindSpeed(df)
  print("Finished getting mean wind speeds")
  df
}



source("data_wrangling_scripts/model/model_points.R")



buildModelPoints <- function(miles, 
                             n,
                             filename = "data/model_points_testing.csv") {
  print("Creating point grid...")
  df <- latlon_grid(miles)
  write.csv(df, filename, row.names = FALSE)
  print("Finished creating point grid")
  
  print("Getting koppen classifications...")
  df$KOPPEN <- koppen_classification(df)
  write.csv(df, filename, row.names = FALSE)
  print("Finished getting koppen classifications")
  
  print("Getting elevations...")
  df$ELEVATION <- elevation(df)
  write.csv(df, filename, row.names = FALSE)
  print("Finished getting elevations")
  
  print("Getting max elevation changes...")
  factor <- miles / 66
  df$ELEVATION_CHANGE <- modelElevationChange(factor, df)
  write.csv(df, filename, row.names = FALSE)
  print("Finished getting max elevation changes")
  
  print("Getting distance to coast...")
  df$DISTANCE_TO_COAST <- distanceToCoast(df)
  write.csv(df, filename, row.names = FALSE)
  print("Finished getting distance to coast")
  
  print("Getting mean wind speeds...")
  df$WIND <- meanWindSpeed(df)
  write.csv(df, filename, row.names = FALSE)
  print("Finished getting mean wind speeds")
  df
}



verifyDfs <- function(df1, df2) {
  mismatching <- c()
  for(c in 1:ncol(df1)){
    match <- all.equal(df1[,c],df2[,c])
    if(!isTRUE(match)) {
      print(c)
      print(match)
    }
  }
}




grid <- buildModelPoints(20, 4)
grid

model_points_og <- read.csv("data/model_points.csv")
model_points_testing <- read.csv("data/model_points_testing.csv")
verifyDfs(model_points_og, model_points_testing)

model_points_og$ELEVATION_CHANGE == model_points_testing$ELEVATION_CHANGE










