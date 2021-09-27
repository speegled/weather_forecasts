library(ggplot2)
library(ggmap)
library(digest)
library(glue)
library(maps)
library(rjson)
library(curl)
<<<<<<< HEAD


cities <- read.csv("cities.csv")
key <- fromJSON(file = "creds.json")

register_google(key)
has_google_key()

png("citiesMap.png")
map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
=======
library(stringr)

cities <- read.csv("cities.csv")
cities
key <- fromJSON(file = "creds.json")

register_google(key["Google"])
has_google_key()

png("citiesMap.png")
map <- ggplot() + borders('world', xlim = c(-200,-50), ylim = c(10,80), color ='black', fill='lightblue')
>>>>>>> 042a6ce45a76b0406c495f381b20aabfb565d1de
map + geom_point(data = cities, mapping = aes(x=LON, y=LAT), color = 'black')
dev.off()

# check outliers:
# cities[which(cities$LAT < 26),]
# cities[which(cities$LON < -125),]
# cities[which(cities$LON > -30),]