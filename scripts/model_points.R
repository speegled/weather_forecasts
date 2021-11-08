library(sf)
library(spData)
library(ggplot2)
library(ggmap)
library(kgc)

# spData::us_states, spData::hawaii, spData::alaska
# "Alaska" %in% spData::us_states$NAME

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {

  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}


latlon_grid <- function(miles) {
  step <- 1/66 * miles
  
  # get a grid of lat / lon values inside the box corners
  # the distance between points is given number of miles
  # (50, -130) is top right corner
  # (20, -60) is bottom left corner
  box <- expand.grid(LON = seq(-130, -60, by=step),
                     LAT = seq(20, 50, by=step))
  
  box$STATE <- lonlat_to_state(box)
  box[!is.na(box$STATE),]
}

save_plot <- function(df, name, color) {
  png(name)
  map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
  map <- map + geom_point(data = df, mapping = aes(x=LON, y=LAT, color=color))
  print(map)
  dev.off()
}


##### Find optimal distance between points #####
#miles <- seq(10, 50, 10)
#for(i in miles) {
#  grid <- latlon_grid(i)
#  name <- paste("test_", i, ".png")
#  save_plot(grid, name, color="black")
#}



##### Use kgc package to get koppen classifications for the points #####
res <- "course"
grid <- latlon_grid(20)

grid$rndCoord.lon <- RoundCoordinates(grid$LON, res=res, latlong="lon")
grid$rndCoord.lat <- RoundCoordinates(grid$LAT, res=res, latlong="lat")
grid <- grid[,c(3,1,2,4,5)]

grid$koppen <- LookupCZ(grid, res=res)
grid

# write.csv(grid, file="model_points.csv", row.names=F)
# save_plot(grid, "koppen.png", color=factor(koppen))



##### Compare scraped koppen classifications to classifications from kgc #####
cities <- read.csv("data/cities.csv")
temp <- data.frame(CITY = cities$CITY,
                   LON = cities$LON,
                   LAT = cities$LAT,
                   rndCoord.lon = RoundCoordinates(cities$LON, res=res, latlong="lon"),
                   rndCoord.lat = RoundCoordinates(cities$LAT, res=res, latlong="lat"),
                   STATE = cities$STATE)
temp$koppen <- LookupCZ(temp, res=res)
# merge dfs
full <- merge(cities, temp, by = c("CITY","STATE"), all = TRUE)
full$koppen <- as.character(full$koppen)
# where classifications do not match
diff <- full[full$CLIMATE != full$koppen, c(1,2,5,10)]
diff


##### Change koppen classifications in cities.csv to match kgc package #####
new_cities <- full[,c(1,2,3,4,10)]
colnames(new_cities) <- c("CITY","STATE","LAT","LON","CLIMATE")
# manually change Milwaukee and W Palm Beach
new_cities[new_cities$CITY == "MILWAUKEE", 5] <- "Dfa"
new_cities[new_cities$CITY == "W_PALM_BEACH", 5] <- "Af"
# check for any more missing values
new_cities[new_cities$CLIMATE == "Climate Zone info missing", ]
# save to cities.csv
# write.csv(new_cities, file="data/cities.csv", row.names=F)





##### Find elevation for cities.csv and model_points.csv #####
library(elevatr)
library(rgdal)
prj_dd <- "EPSG:4326"

model_points <- read.csv("data/model_points.csv", header=T)
cities <- read.csv("data/cities.csv", header=T)

model_temp <- model_points[,c(2,3,1,4,5,6)]
# elevation is in meters
model_elev <- get_elev_point(model_temp, prj = prj_dd, src = "epqs")
model_points$ELEVATION <- model_elev$elevation
# write.csv(model_points, file="data/model_points.csv", row.names=F)

cities_temp <- cities[,c(4,3,1,2,5)]
cities_elev <- get_elev_point(cities_temp, prj = prj_dd, src = "epqs")
cities$ELEVATION <- cities_elev$elevation
# write.csv(cities, file="data/cities.csv", row.names=F)














