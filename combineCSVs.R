library(stringr)

koppen <- read.csv("koppen.csv", header = TRUE)
latlong <- read.csv("latlong.csv")

test <- merge(latlong, koppen, by=c("CITY"))
test[which(test["STATE.x"] != test["STATE.y"]),]

cities <- merge(latlong, koppen, by=c("CITY","STATE"))

cities$LAT <- paste(str_sub(cities$LAT, 1, -3), str_sub(cities$LAT, -1, -1), sep="")
cities$LON <- paste(str_sub(cities$LON, 1, -3), str_sub(cities$LON, -1, -1), sep="")

write.csv(cities, file="cities.csv", row.names=F)




