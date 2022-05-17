#' KRIGING - Created by Clayton Strauch - 30 MAR 2022
#' 

library(tidyverse)
library(sp) 
library(gstat)
library(automap) #used for another kriging method

#### KRIG 1:  TEMPERATURE PREDICTION ERROR (HI) ####

# Read in Cities data
cities <- read.csv('data/cities.csv')

# Read in Weather Data - high temp predicted 24 hours before (minus missing first day forecast value observations)
df <- read.csv("data/email_data_expanded.csv") %>%
  #filter(high_or_low == "high" & forecast_hours_before == 24 & !is.na(forecast_temp) & !(state %in% c("AK","HI","VI","PR"))) %>%
  filter(high_or_low == "high" & !is.na(forecast_temp) & !(state %in% c("AK","HI","VI","PR"))) %>%
  select(date, city, state, observed_temp, forecast_temp) %>%
  mutate(error = abs(forecast_temp - observed_temp)) %>%
  group_by(city, state) %>%
  summarize(mean_error = mean(error, na.rm=T), .groups = "drop") %>%
  inner_join(cities, by=c("city", "state")) %>%
  select(-koppen, -avg_annual_precip)

plot(df$lon, df$lat)

coordinates(df) <- ~lat + lon


# VARIOGRAM

lzn.vgm <- variogram(mean_error ~ 1, df) # calculates sample variogram values 

lzn.fit = fit.variogram(lzn.vgm, vgm(c("Sph")), fit.kappa = TRUE)

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

state <- map_data("state")



+ geom_map(data=state, map=state,
           aes(long, lat, map_id=region),
           color="black", fill=NA, size=0.1)

#Plot kriged values
k <- lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=long, y=lat)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red", name="Temperature Error (°F)") +
  theme_bw() + 
  geom_point(data = data.frame(df), aes(x=lon, y=lat)) +
  labs(title = "Kriging of Mean High Temperature Forecast Error",
       subtitle = "Points Indicate Cities with Recorded Values",
       x = "Longitude", y = "Latitude") +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
  geom_map(data=state, map=state,
           aes(long, lat, map_id=region),
           color="black", fill=NA, size=0.1) +
  theme(legend.position = "bottom")
k

#savePlot(k,filename = "plots/kriging_high_mean_error", type = "png")






