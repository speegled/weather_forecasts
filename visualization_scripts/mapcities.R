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


filename <- "cities.csv"
plot_file <- "all_cities.png"

# read csv and credentials
cities <- read.csv(filename)
key <- fromJSON(file = "creds.json")

# check that api key works
register_google(key)
has_google_key()

# plot cities based on LAT / LON and save to plot_file
png(plot_file)
map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
map + geom_point(data = cities, mapping = aes(x=LON, y=LAT), color = 'black')
dev.off()


