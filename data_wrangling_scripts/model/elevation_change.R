

# return euclidean distance between two points
euclidean <- function(a, b) {
  if(is.numeric(a) && is.numeric(b)) {
    sqrt(sum((a - b)^2))
  } else {
    NA
  }
}

# return return the n points closest to p in df
# df must have LON and LAT columns
# uses euclidean distance function
findClosestPoints <- function(df, p, n) {
  dist <- c()
  # iterate over all points in df
  for(i in 1:nrow(df)){
    # point coordinates
    mp <- c(df[i,1], df[i,2]) 
    # calculate points distance to p
    d <- euclidean(p, mp)
    # save distance
    dist[i] <- d
  }
  # sort df by distance
  df_sorted <- df[order(dist),]
  # get the top n points
  top <- head(df_sorted, n)
  closest <- list()
  for(i in 1:n) {
    cp <- c(top$lon[i], top$lat[i])
    closest[[i]] <- cp
  }
  # return to pn closest points to p
  closest
}


citiesElevationChange <- function(n, 
                                  df1,
                                  df2) {
  # calculate the greatest change in elevation for df1 using points in df2
  elevation_change <- c()
  # iterate over each point in df1
  for(c in 1:nrow(df1)) {
    # point coordinate
    p <- c(df1$lon[c], df1$lat[c])
    print(as.character(df1$city[c]))
    # point elevation
    elev <- df1$elevation[c]
    # get the n closests points in model_points
    closest <- findClosestPoints(df2, p, n)
    # find the max elevation change amoung closest points
    max_d <- -Inf
    # iterate over each close point
    for(m in 1:length(closest)) {
      # elevation of the close point
      mask <- (df2$lon == closest[m][[1]][1]) & (df2$lat == closest[m][[1]][2])
      e <- df2$elevation[mask]
      # save if diff between elevations if higher than prev
      d <- abs(elev - e)
      max_d <- max(d, max_d)
    }
    # save greatest elevation change
    elevation_change[c] <- max_d
  }
  round(elevation_change, 2)
}




# return greatest change in elevation for model_points
# closest points are always (c - 1, r), (c + 1, r), (c, r - 1), (c, r + 1) 
# factor is step between points in model_points in degrees (miles_apart / miles_per_degree)
modelElevationChangeFour <- function(df) {
  
  factor <- abs(df$lon[1] - df$lon[2])
  print(factor)
  
  elevation_change <- c()
  # iterate over each point in df
  for(n in 1:nrow(df)) {
    
    # point coordinates
    pm <- c(df$lon[n], df$lat[n])
    print(pm)
    
    # point elevation
    pe <- df$elevation[n]
  
    # get 4 closest points and calculate change in elevation from point
    m1 <- (df$lon == round(pm[1] + factor, 13)) & (df$lat == pm[2])
    e1 <- df$elevation[m1]
    d1 <- abs(pe - e1)
    
    m2 <- (df$lon == round(pm[1] - factor, 13)) & (df$lat == pm[2])
    e2 <- df$elevation[m2]
    d2 <- abs(pe - e2)
    
    m3 <- (df$lon == pm[1]) & (df$lat == round(pm[2] + factor, 13))
    e3 <- df$elevation[m3]
    d3 <- abs(pe - e3)
    
    m4 <- (df$lon == pm[1]) & (df$lat == round(pm[2] - factor, 13))
    e4 <- df$elevation[m4]
    d4 <- abs(pe - e4)
    
    # get max elevation change between points
    a <- c(d1,d2,d3,d4)
    max_d <- max(a,na.rm=TRUE)
    elevation_change[n] <- max_d
  }
  # convert all -Inf values to 0
  elevation_change[elevation_change == -Inf] <- 0
  round(elevation_change, 2)
}



modelElevationChangeEight <- function(df) {
  
  factor <- abs(df$lon[1] - df$lon[2])
  print(factor)
  
  elevation_change <- c()
  # iterate over each point in df
  for(n in 1:nrow(df)) {
    
    # point coordinates
    pm <- c(df$lon[n], df$lat[n])
    print(pm)
    
    # point elevation
    pe <- df$elevation[n]
    
    # get 4 closest points and calculate change in elevation from point
    m1 <- (df$lon == round(pm[1] + factor, 13)) & (df$lat == pm[2])
    e1 <- df$elevation[m1]
    d1 <- abs(pe - e1)
    
    m2 <- (df$lon == round(pm[1] - factor, 13)) & (df$lat == pm[2])
    e2 <- df$elevation[m2]
    d2 <- abs(pe - e2)
    
    m3 <- (df$lon == pm[1]) & (df$lat == round(pm[2] + factor, 13))
    e3 <- df$elevation[m3]
    d3 <- abs(pe - e3)
    
    m4 <- (df$lon == pm[1]) & (df$lat == round(pm[2] - factor, 13))
    e4 <- df$elevation[m4]
    d4 <- abs(pe - e4)
    
    # diagonal points
    m5 <- (df$lon == round(pm[1] + factor, 13)) & (df$lat == round(pm[2] + factor, 13))
    e5 <- df$elevation[m5]
    d5 <- abs(pe - e5)
    
    m6 <- (df$lon == round(pm[1] + factor, 13)) & (df$lat == round(pm[2] - factor, 13))
    e6 <- df$elevation[m6]
    d6 <- abs(pe - e6)
    
    m7 <- (df$lon == round(pm[1] - factor, 13)) & (df$lat == round(pm[2] + factor, 13))
    e7 <- df$elevation[m7]
    d7 <- abs(pe - e7)
    
    m8 <- (df$lon == round(pm[1] - factor, 13)) & (df$lat == round(pm[2] - factor, 13))
    e8 <- df$elevation[m8]
    d8 <- abs(pe - e8)
    
    # get max elevation change between points
    a <- c(d1,d2,d3,d4,d5,d6,d7,d8)
    max_d <- max(a,na.rm=TRUE)
    elevation_change[n] <- max_d
  }
  # convert all -Inf values to 0
  elevation_change[elevation_change == -Inf] <- 0
  round(elevation_change, 2)
}


# save elevation change plot
# png("plots/elevation_change.png")
# map <- ggplot() + borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue')
# map <- map + geom_point(data = model_points, mapping = aes(x=LON, y=LAT, color=ELEVATION_CHANGE)) + 
#  scale_color_gradientn(colours = c("blue", "green", "yellow", "orange", "red"))
# print(map)
# dev.off()




# run parallel clusters
# library(parallel)
# library(tidyverse)
# cl <- parallel::makeCluster(8, setup_strategy = "sequential")
# clusterExport(cl, list("model_points"))
# Ns <- 1:nrow(model_points)
# sim_data <- parLapply(cl, Ns, modelElevationChange)
# sim_data <- bind_rows(sim_data)
# sim_data
# stopCluster(cl)




# 8 closest points instead of 4
#cities <- read.csv('data/cities.csv')
#model_points <- read.csv('data/model_points.csv')

#eight_nearest <- citiesElevationChange(8, cities, model_points)
#cities$ELEVATION_CHANGE_EIGHT <- eight_nearest
#cities$EC_DIFF <- abs(cities$ELEVATION_CHANGE - cities$ELEVATION_CHANGE_EIGHT)
#select_elevation <- cities[,c("CITY", "STATE", "ELEVATION", "ELEVATION_CHANGE", "ELEVATION_CHANGE_EIGHT", "EC_DIFF")]
#select_elevation <- select_elevation[-(select_elevation$STATE %in% c("AK", "HI")),]
#select_elevation

#library(tidyverse)
#select_elevation %>% 
#  arrange(desc(EC_DIFF)) %>% 
#  head()

#colnames(cities) <- c("city", "state", "lon", "lat", "koppen", "elevation", "elevation_change",
#                      "distance_to_coast", "wind", "elevation_change_eight", "ec_diff")
#head(cities)
#cities <- cities %>% arrange(desc(ec_diff))
#cities
#write.csv(cities, 'data/cities.csv', row.names = FALSE)
