

# return euclidean distance between two points
euclidean <- function(a, b) sqrt(sum((a - b)^2))


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
    print(p)
    print(mp)
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
    cp <- c(top$LON[i], top$LAT[i])
    closest[[i]] <- cp
  }
  # return to pn closest points to p
  closest
}


citiesElevationChange <- function(n = 4, 
                                  df1 = cities,
                                  df2 = model_points) {
  # calculate the greatest change in elevation for df1 using points in df2
  elevation_change <- c()
  # iterate over each point in df1
  for(c in 1:nrow(df1)) {
    # point coordinate
    p <- c(df1$LON[c], df1$LAT[c])
    # point elevation
    elev <- df1$ELEVATION[c]
    # get the n closests points in model_points
    closest <- findClosestPoints(df2, p, n)
    # find the max elevation change amoung closest points
    max_d <- -Inf
    # iterate over each close point
    for(m in 1:length(closest)) {
      # elevation of the close point
      mask <- (df2$LON == closest[m][[1]][1]) & (df2$LAT == closest[m][[1]][2])
      e <- df2$ELEVATION[mask]
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
modelElevationChange <- function(factor,
                                 df = model_points) {
  elevation_change <- c()
  # iterate over each point in df
  for(n in 1:nrow(df)) {
    
    # point coordinates
    pm <- c(df$LON[n], df$LAT[n])
    # point elevation
    pe <- df$ELEVATION[n]
  
    # get 4 closest points and calculate change in elevation from point
    m1 <- (df$LON == round(pm[1] + factor, 13)) & (df$LAT == pm[2])
    e1 <- df$ELEVATION[m1]
    d1 <- abs(pe - e1)
    
    m2 <- (df$LON == round(pm[1] - factor, 13)) & (df$LAT == pm[2])
    e2 <- df$ELEVATION[m2]
    d2 <- abs(pe - e2)
    
    m3 <- (df$LON == pm[1]) & (df$LAT == round(pm[2] + factor, 13))
    e3 <- df$ELEVATION[m3]
    d3 <- abs(pe - e3)
    
    m4 <- (df$LON == pm[1]) & (df$LAT == round(pm[2] - factor, 13))
    e4 <- df$ELEVATION[m4]
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

new <- modelElevationChange(20 / 66, model_points_testing)
max(new[which(new != model_points_og$ELEVATION_CHANGE)] - model_points_og$ELEVATION_CHANGE[which(model_points_og$ELEVATION_CHANGE != new)])
model_points_testing$ELEVATION_CHANGE == new

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
cities <- read.csv('data/cities.csv')
model_points <- read.csv('data/model_points.csv')

eight_nearest <- citiesElevationChange(8, cities, model_points)
cities$ELEVATION_CHANGE_EIGHT <- eight_nearest
cities$EC_DIFF <- abs(cities$ELEVATION_CHANGE - cities$ELEVATION_CHANGE_EIGHT)
select_elevation <- cities[,c("CITY", "STATE", "ELEVATION", "ELEVATION_CHANGE", "ELEVATION_CHANGE_EIGHT", "EC_DIFF")]
select_elevation <- select_elevation[-(select_elevation$STATE %in% c("AK", "HI")),]
select_elevation

library(tidyverse)
select_elevation %>% 
  arrange(desc(EC_DIFF)) %>% 
  head()

colnames(cities) <- c("city", "state", "lon", "lat", "koppen", "elevation", "elevation_change",
                      "distance_to_coast", "wind", "elevation_change_eight", "ec_diff")
head(cities)
