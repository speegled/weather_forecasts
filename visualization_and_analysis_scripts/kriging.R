#' KRIGING - Created by Clayton Strauch - 30 MAR 2022
#' 

library(tidyverse)
library(sp)
library(gstat)
library(automap) #used for another kriging method

#### KRIG 1:  TEMPERATURE PREDICTION ERROR (HI) ####

#Read in data - high temp predicted 24 hours before (minus missing first day forecast value observations)
df <- read.csv("data/email_data_expanded.csv") %>%
  filter(high_or_low == "high" & forecast_hours_before == 24 & !is.na(forecast_temp) & !(state %in% c("AK","HI","VI","PR"))) %>%
  select(date, city, state, observed_temp, forecast_temp) %>%
  mutate(error = abs(forecast_temp - observed_temp)) %>%
  group_by(city, state) %>%
  summarize(mean_error = mean(error, na.rm=T), .groups = "drop") %>%
  inner_join(cities, by=c("city"="CITY", "state"="STATE")) %>%
  select(-CLIMATE, -AVG_ANNUAL_PRECIP)

plot(df$LON, df$LAT)

coordinates(df) <- ~LAT + LON


# VARIOGRAM

lzn.vgm <- variogram(mean_error ~ 1, df) # calculates sample variogram values 

lzn.fit = fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

plot(lzn.vgm, lzn.fit)

# KRIGGING

# Read in model_points "grid"
grid <- read.csv("data/model_points.csv") %>%
  select(long=lon, lat)
plot(grid$lon, grid$lat)

# Change grid to SPDF
coordinates(grid) <- ~lat + long

# Krig with Variogram
lzn.kriged <- krige(mean_error ~ 1, df, grid, model=lzn.fit)

#Plot kriged values
lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=long, y=lat)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  theme_bw() + 
  geom_point(data = data.frame(df), aes(x=LON, y=LAT)) 



