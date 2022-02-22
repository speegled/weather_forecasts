library(dplyr)
library(rgdal)
library(rgeos)
library(tools)
library(ggplot2)

# Reference:
# https://rstudio-pubs-static.s3.amazonaws.com/199150_7becf121a2ac45c7ada9bc26d7cde985.html

fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f <- fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}


# return the minimum distance in miles for points in df
# df must have LON and LAT columns
distanceToCoast <- function(df,
                            coast_file_loc = "data/ne_10m_coastline",
                            file = "ne_10m_coastline.shp") {
  # read shapefile 
  coast <- readOGR(coast_file_loc, 
                   file_path_sans_ext(file))
  # convert coast shapefile to object class dataframe
  coast <- fortify.shape(coast)
  
  # convert coordinates to sp dfs
  df_spdf <- SpatialPointsDataFrame(coords = df[,c("LON", "LAT")], 
                                    data = df)
  coast_spdf <- SpatialPointsDataFrame(coords = coast[,c("long","lat")], 
                                       data = coast)

  # calculate distance to closest point on coast
  # distance in miles
  dist <- c()
  for (i in 1:nrow(df_spdf)) {
    dist[i] <- gDistance(df_spdf[i,], coast_spdf) * 66
  }
  round(dist, 2)
}





# plot city points with coastline
# library(ggplot2)

# xlims <- c(-135, -60)
# ylims <- c(20, 50)

# p0 <- ggplot() + 
#   geom_path(data = coast, aes(x = long, y = lat, group = group), 
#             color = "black", size = 0.25) + 
#   coord_map(projection = "mercator") + 
#   scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
#   scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
#  labs(list(title = "", x = "Longitude", y = "Latitude")) +
#   geom_point(data = cities, mapping = aes(x=LON, y=LAT))
# p0











