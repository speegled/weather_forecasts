library(ggplot2)
library(ggmap)
library(digest)
library(glue)
library(maps)
library(rjson)
library(curl)
library(stringr)
library(dplyr)
library(tidyverse)



# plot cities by lat / lon 
cities <- read.csv("data/cities.csv")
png("plots/cities.png")

map <- ggplot() + borders('world', xlim = c(-225, -60), ylim = c(15, 75), color ='black', fill='lightblue')
map <- map + geom_point(data = cities, mapping = aes(x=LON, y=LAT))
print(map)
dev.off()



model_points <- read.csv("data/model_points.csv")

# plot model points and color by koppen classification
png("plots/model_koppen.png")
map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
map <- map + geom_point(data = model_points, mapping = aes(x=LON, y=LAT, color=factor(koppen)))
print(map)
dev.off()


# plot model points and color by elevation
png("plots/model_elevation.png")
map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
map <- map + geom_point(data = model_points, mapping = aes(x=LON, y=LAT, color=ELEVATION)) + 
             scale_color_gradientn(colours = c("blue", "green", "yellow", "orange", "red"))
print(map)
dev.off()




