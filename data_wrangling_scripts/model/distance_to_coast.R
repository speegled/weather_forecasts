library(tidyverse)
library("rgdal")
library("tools")

# Reference:
# https://rstudio-pubs-static.s3.amazonaws.com/199150_7becf121a2ac45c7ada9bc26d7cde985.html

path.ne.coast <- ("/Users/hlani/OneDrive/Documents/SLU_Shit/Senior/weather_forecasts/data")
fnam.ne.coast <- "ne_10m_coastline.shp"
dat.coast <- readOGR(dsn = path.ne.coast, 
                     layer = file_path_sans_ext(fnam.ne.coast))


# Fortify the shapefile data using `fortify.shape()`:
dat.coast@data$id <- rownames(dat.coast@data)
x.f <- fortify(dat.coast, region = "id")
dat.shape <- inner_join(x.f, dat.coast@data, by = "id")
dat.shape
# domain over all us
domain <- c(-128, -60, 20, 50)

dat.coast.shape <-  filter(dat.shape, long > domain[1] & 
                                     long < domain[2] & 
                                     lat > domain[3] & 
                                     lat < domain[4])



# Specify the spatial extent for our map (i.e., our study area; notice that its
# dimensions are different from the domain for which we extracted the
# coastline):
xlims <- c(-135, -60)
ylims <- c(20, 50)

model <- read.csv("data/model_points.csv")
cities <- read.csv("data/cities.csv")


# Generate a base map with the coastline:
p0 <- ggplot() + 
  geom_path(data = dat.coast.wc, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.25) + 
  coord_map(projection = "mercator") + 
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "", x = "Longitude", y = "Latitude")) +
  geom_point(data = cities, mapping = aes(x=LON, y=LAT))
p0


cities_xy <- cities[,4:3]
cities_spdf <- SpatialPointsDataFrame(coords = cities_xy, data = cities)
cities_xy[1,]
model[,2:3]
model_xy <- model[,2:3]
model_spdf <- SpatialPointsDataFrame(coords = model_xy, data = model)



coast_xy <- dat.coast.shape[,1:2]
coast_spdf <- SpatialPointsDataFrame(coords = coast_xy, data = dat.coast.shape)

         
dist <- c()
for (i in 1:nrow(model_spdf)) {
  dist[i] <- gDistance(model_spdf[i,],coast_spdf) * 69
}

model$DISTANCE <- dist 
model


cities$distance <- dist
cities <- cities[,c(1:6, 8)]
cities$DISTANCE <- cities$distance
cities
#write.csv(cities, file="data/cities.csv", row.names=F)
#write.csv(model, file="data/model_points.csv", row.names=F)










