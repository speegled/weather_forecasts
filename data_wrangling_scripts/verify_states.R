email_data <- read.csv("data/email_data.csv")
city_names <- as.character(unique(email_data$city))
city_names

install.packages("xlsx")
x <- data.frame(city_names)
x
library("xlsx")
write.xlsx(x, "data/verify_cities.xlsx", sheetName = "cities", 
           col.names = FALSE, row.names = FALSE, append = FALSE)



states <- c()
no_states <- c()
for(i in 1:length(city_names)){
  split <- strsplit(city_names[i], " +")[[1]] 
  if(nchar(split[length(split)]) == 2){
    states <- c(states, city_names[i])
  }
  else {
    no_states <- c(no_states, city_names[i])
  }
}
states
no_states
length(no_states)

length(states) + length(no_states)
length(city_names)





email_data <- read.csv("data/email_data_reorganized.csv")
max(ymd(unique(email_data$date)))





# changing data
# Caribou, Macon, Fairbanks, San Antonio, St STE Marie
cities <- read.csv("data/cities.csv")

library(tidyverse)
change <- cities %>% filter(city %in% c("CARIBOU", "MACON", "FAIRBANKS", "SAN_ANTONIO", "ST_STE_MARIE"))

change[change$city == "CARIBOU",]$state <- "ME"
change[change$city == "MACON",]$state <- "GA"
change[change$city == "FAIRBANKS",]$state <- "AK"
change[change$city == "SAN_ANTONIO",]$state <- "TX"
change[change$city == "ST_STE_MARIE",]$state <- "MI"
change


# lon = negative, lat = positive

change[change$city == "CARIBOU",]$lat <- 46.87
change[change$city == "CARIBOU",]$lon <- -68.01

change[change$city == "MACON",]$lat <- 32.69
change[change$city == "MACON",]$lon <- -83.65

change[change$city == "FAIRBANKS",]$lat <- 64.8
change[change$city == "FAIRBANKS",]$lon <- -147.88

change[change$city == "SAN_ANTONIO",]$lat <- 29.38
change[change$city == "SAN_ANTONIO",]$lon <- -98.58

change[change$city == "ST_STE_MARIE",]$lat <- 46.48
change[change$city == "ST_STE_MARIE",]$lon <- -84.36

change$koppen <- as.character(change$koppen)

mask <- change$city == "CARIBOU"  & change$state == "ME"
change[mask,]$koppen
df


change_value <- function(df, city, state, column_index, new_value) {
  mask <- df$city == city & df$state == state
  df[mask,column_index] <- new_value
  df
}


# change_value(change, "CARIBOU", "ME", 5, "abc")

change <- change[,1:9]
change

source("data_wrangling_scripts/model/koppen.R")
source("data_wrangling_scripts/model/elevation.R")
source("data_wrangling_scripts/model/elevation_change.R")
source("data_wrangling_scripts/model/distance_to_coast.R")
source("data_wrangling_scripts/model/wind.R")

lat_lons <- change[,3:4]
lat_lons

koppens <- koppen_classification(lat_lons)
elevations <- elevation(lat_lons)

model_points <- read.csv("data/model_points.csv")
citiesElevationChange(n=4, df1=lat_lons, df2=model_points)

# check lat and lon of cities
# push to github


