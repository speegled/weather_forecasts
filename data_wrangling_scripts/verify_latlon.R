
cities <- read.csv("data/cities.csv")
cities

x <- cities[,1:4]
x
library("xlsx")
#write.xlsx(x, "data/verify_latlon.xlsx", sheetName = "cities", 
#           col.names = FALSE, row.names = FALSE, append = FALSE)


val_cities <- read.csv('data/validated_cities.csv')
val_cities

val_cities$city <- as.character(val_cities$city)
val_cities$state <- as.character(val_cities$state)
val_cities$lon <- as.numeric(val_cities$lon)
val_cities$lat <- as.numeric(val_cities$lat)
val_cities

library(rvest)
randomlyCheckCoords <- function(df) {
  rows <- 1:nrow(df)
  for(i in 1:5){
    j <- sample(rows, 1)
    rows <- rows[!(rows == j)]
    city <- df[j,1]
    state <- df[j,2]
    lon <- df[j, 3]
    lat <- df[j, 4]
    print(city)
    print(state)
    url <- paste('https://www.google.com/search?q=', as.character(lat[1]), '%C2%B0N+', as.character(abs(lon[1])),'%C2%B0W', sep="")  
    print(url)
  }
}
randomlyCheckCoords(val_cities)

write.csv(val_cities, 'data/validated_cities.csv', row.names = FALSE)

source('data_wrangling_scripts/model/koppen.R')
source('data_wrangling_scripts/model/elevation.R')
source('data_wrangling_scripts/model/wind.R')
source('data_wrangling_scripts/model/distance_to_coast.R')

# val_cities$koppen <- koppen_classification(val_cities)
# unique(val_cities$koppen)

val_cities <- read.csv('data/validated_cities.csv')

val_cities$koppen <- koppen_classification(val_cities, 'fine')
val_cities$koppen[val_cities$koppen == 'Ocean'] <- NA
unique(val_cities$koppen)

# val_cities$koppen <- val_cities$koppen_fine
# val_cities <- val_cities[,1:7]
# val_cities
# val_cities[val_cities$koppen != val_cities$koppen_fine,]

val_cities$elevation <- elevation(val_cities)
val_cities[is.na(val_cities$elevation),]
# OLD_HARBOR is NA, values in meters
val_cities$elevation[val_cities$city == 'OLD_HARBOR'] <- 0.9144
val_cities

val_cities$distance_to_coast <- distanceToCoast(val_cities)
# calculating only from continental US, islands (HI, VIR, PR, AK) will be wrong
val_cities[is.na(val_cities$distance_to_coast),]

val_cities

val_cities$wind <- meanWindSpeed(val_cities)
val_cities[is.na(val_cities$wind),]




# new model_points csv
source('data_wrangling_scripts/model/model_points.R')

write.csv(val_model, 'data/validated_model_points.csv', row.names = FALSE)

val_model <- read.csv('data/validated_model_points.csv')
# 10 miles apart
val_model <- latlon_grid(miles = 10)
colnames(val_model) <- c('lon', 'lat')

val_model$koppen <- koppen_classification(val_model)
val_model[is.na(val_model$koppen),]S
unique(val_model$koppen)
val_model[val_model$koppen == 'Climate Zone info missing',] <- NA


val_model$elevation <- elevation(val_model)
val_model[is.na(val_model$elevation),]
min(val_model$elevation[!is.na(val_model$elevation)])

val_model$distance_to_coast <- distanceToCoast(val_model)
val_model[is.na(val_model$distance_to_coast),]

val_model$wind <- meanWindSpeed(val_model)
max(val_model$wind[!is.na(val_model$wind)])



# elevation change
val_cities
source('data_wrangling_scripts/model/elevation_change.R')
val_cities$elevation_change_eight <- citiesElevationChange(8, val_cities, val_model)
write.csv(val_cities, 'data/validated_cities.csv', row.names = FALSE)
val_cities[is.na(val_cities$elevation_change_eight),]
max(val_cities$elevation_change_eight[!is.na(val_cities$elevation_change_eight)])

source('data_wrangling_scripts/model/elevation_change.R')
val_model$elevation_change_four <- modelElevationChangeFour(val_model)
val_model$elevation_change_eight <- modelElevationChangeEight(val_model)
val_model
val_model[is.na(val_model$elevation_change_eight),]
min(val_model$elevation_change_eight[!is.na(val_model$elevation_change_eight)])
write.csv(val_model, 'data/validated_model_points.csv', row.names = FALSE)


max(abs(val_cities$elevation_change_four - val_cities$elevation_change_eight))

