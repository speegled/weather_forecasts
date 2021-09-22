
missing_cities <- read.csv("missingCities.csv")
missing_cities

library(stringr)

missing_cities$LAT <- as.numeric(str_sub(missing_cities$LAT, 1, -3))
missing_cities$LON <- as.numeric(str_sub(missing_cities$LON, 1, -3)) * -1
# write.csv(missing_cities, file = "missingCities.csv", row.names = F)

missing_cities[which(is.na(missing_cities$CLIMATE)),]

cities <- read.csv("cities.csv")
cities
new_cities <- rbind(missing_cities, cities)
new_cities

write.csv(new_cities, file = "cities.csv", row.names = F)
