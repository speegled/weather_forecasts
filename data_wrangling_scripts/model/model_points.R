library(sf)
library(spData)


# check if a lat / lon point is in a state
# does not include alaska or hawaii
lonlat_to_state <- function(df,
                            states = spData::us_states,
                            name_col = "NAME") {

  # convert df to sf, coords are first two columns
  pts <- st_as_sf(df, coords = c("LON", "LAT"), crs = 4326)
  
  # transform to 3857 coord ref system
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  state_names <- states[[name_col]]
  # return states that the points are in
  # NA if a point is not in a state
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}


# return a grid of lat / lon points that are miles apart
# all points are in the contenental US
latlon_grid <- function(miles) {
  
  # each mile is 1/66 of a lat / lon degree
  step <- 1/66 * miles
  
  # get a grid of lat / lon values inside the box corners
  # the distance between points is given number of miles
  # (50, -130) is top right corner
  # (20, -60) is bottom left corner
  box <- expand.grid(LON = seq(-130, -60, by=step),
                     LAT = seq(20, 50, by=step))
  
  # get states for all points
  box$STATE <- lonlat_to_state(box)
  
  # return points that are in a state (LON, LAT)
  box[!is.na(box$STATE),c("LON", "LAT")]
}






