library(tidyverse)

data <- read.csv("data/email_data_expanded.csv")

data <- data %>% mutate(possible_error = case_when(forecast_temp < 0 & observed_temp > 25 ~ "forecast_temp",
                                                   forecast_outlook == "VRYCLD" & forecast_temp > 15 ~ "forecast_outlook",
                                                   city == "KANSAS_CITY" & date == "2021-06-12" & high_or_low == "low" ~ "observed_temp",
                                                   city == "SIOUX_FALLS" & date == "2021-12-05" & high_or_low == "high" ~ "observed_temp",
                                                   TRUE ~ "none"))

ggplot() +
  geom_point(data = data %>% filter(possible_error == "none"), 
             aes(observed_temp, forecast_temp), alpha = 0.01) +
  geom_point(data = data %>% filter(possible_error != "none"),
             aes(observed_temp, forecast_temp, col = possible_error))