# return the mean wind speed of points in df
# df must have LON and LAT columns
# extracts data from file at given location
# USE FOR CITIES AND MODEL POINTS
library(raster)

meanWindSpeed <- function(df, 
                          file = 'data/USA_wind-speed_10m.tif') {
  imported_raster <- raster(file)
  # extract wind speed at given points from file
  wind <- extract(imported_raster, df[,c("LON", "LAT")], df=TRUE)
  # return rounded mean wind speed values
  round(wind[,2], 2)
}



# save mean wind speed plot
# png("plots/mean_wind_speed.png")
# map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
# map <- map + geom_point(data = model_points, mapping = aes(x=LON, y=LAT, color=WIND)) + 
#   scale_color_gradientn(colours = c("blue", "green", "yellow", "orange", "red"))
# print(map)
# dev.off()




# return the wind speed for points in df on a given date and time
# df must have LON and LAT columns
# DO NOT USE FOR CITIES OR MODEL POINTS (NOT MEAN WIND SPEED)
library(rWind)

getWindOnDate <- function(df, 
                          year, 
                          month, 
                          day,
                          time,
                          box = c(-125.75, -50.25, 20.75, 55.25)) {
  
  # wind speed on date and time for points .5 degrees away in given lat / lon box
  wind <- wind.dl(year, month, day, time, box[1], box[2], box[3], box[4])
  
  # extract closest wind speed for all points in df
  speed <- c()
  for(x in 1:nrow(df)){
    # round df points to nearest .5 degree
    point_lon <- round_any(df$LON[x], .5)
    point_lat <- round_any(df$LAT[x], .5)
    
    # find wind speed for rounded points
    mask <- (wind$lon == point_lon) & (wind$lat == point_lat)
    s <- wind[mask,]$speed
    speed[x] <- s
  }
  # return wind speed on date and time
  speed
}





