library(ggplot2)
library(ggmap)
library(digest)
library(glue)
library(maps)
library(rjson)
library(curl)


cities <- read.csv("cities.csv")
key <- fromJSON(file = "creds.json")

register_google(key)
has_google_key()

map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
cities_map <- map + geom_point(data = cities, mapping = aes(x=LON, y=LAT), color = 'black')
cities_map

# check outliers:
# cities[which(cities$LAT < 26),]
# cities[which(cities$LON < -125),]
# cities[which(cities$LON > -30),]