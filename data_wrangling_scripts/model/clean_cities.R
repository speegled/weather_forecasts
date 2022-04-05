
cities <- read.csv('data/cities.csv')
cities %>% 
  filter(STATE %in% c('VI','PR','AK','HI')) %>% 
  count()


df <- read.csv('../weather_forecasts/email_cities_exp.csv')
library(tidyverse)

df %>% 
  filter(date == '2021-08-30')

cities

# make sure all city names are in email data
# make sure all lat lon points are from weather.gov
# recalculate max eleavtion change with city point as center, calc 4 points 
# try elevation_change with 8 n
# change variable names to lowercase

cities %>% 
  arrange(desc(ELEVATION_CHANGE)) %>% 
  head(20)


email_data <- read.csv('data/email_data.csv')
email_cities <- unique(email_data$city)
email_cities
cities_sample <- sample(email_cities, 80)
cities_sample
write.csv(cities_sample, 'data/cities_sample.csv', row.names = FALSE)




# caribou ME
