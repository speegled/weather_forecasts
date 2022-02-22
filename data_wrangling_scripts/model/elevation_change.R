

# load in cities and model points csvs
cities <- read.csv("data/cities.csv")
model_points <- read.csv('data/model_points.csv')

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


findClosestPoints(model_points, c(-67.79,46.12), 6)


citiesElevationChange <- function(n, 
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
  elevation_change
}

citiesElevationChange(4)


# return greatest change in elevation 
# for model_points
# closest points are always (c - 1, r), (c + 1, r), (c, r - 1), (c, r + 1) 
modelElevationChange <- function(n, points=model_points) {
  # point coordinates
  pm <- c(points[n,1], points[n,2])
  # point elevation
  pe <- points[n,3]

  # get 4 closest points
  factor <- 20/66
  m1 <- (points$LON == round(pm[1] + factor,13)) & (points$LAT == pm[2])
  e1 <- points[m1,3]
  d1 <- abs(pe - e1)
  m2 <- (points$LON == round(pm[1] - factor,13)) & (points$LAT == pm[2])
  e2 <- points[m2,3]
  d2 <- abs(pe - e2)
  m3 <- (points$LON == pm[1]) & (points$LAT == round(pm[2] + factor,13))
  e3 <- points[m3,3]
  d3 <- abs(pe - e3)
  m4 <- (points$LON == pm[1]) & (points$LAT == round(pm[2] - factor,13))
  e4 <- points[m4,3]
  d4 <- abs(pe - e4)

  a <- c(d1,d2,d3,d4)
  max_d <- max(a,na.rm=TRUE)
  max_d
}

model_points

melevation <- c()
for(i in 1:nrow(model_points)){
  max_d <- modelElevationChange(i)
  melevation[i] <- max_d
}
melevation
melevation[melevation == -Inf] <- 0

model_points$ELEVATION_CHANGE <- melevation








# run parallel clusters
library(parallel)
library(tidyverse)

cl <- parallel::makeCluster(8, setup_strategy = "sequential")
clusterExport(cl, list("model_points"))
Ns <- 1:nrow(model_points)
sim_data <- parLapply(cl, Ns, modelElevationChange)
sim_data <- bind_rows(sim_data)
sim_data

stopCluster(cl)


