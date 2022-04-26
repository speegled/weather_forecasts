library(tidyverse)

data <- read.csv("data/email_data_expanded.csv")
data <- data %>% mutate(possible_error = case_when(forecast_temp < 0 & observed_temp > 25 ~ "forecast_temp",
                                                   forecast_outlook == "VRYCLD" & forecast_temp > 15 ~ "forecast_outlook",
                                                   city == "KANSAS_CITY" & date == "2021-06-12" & high_or_low == "low" ~ "observed_temp",
                                                   city == "SIOUX_FALLS" & date == "2021-12-05" & high_or_low == "high" ~ "observed_temp",
                                                   TRUE ~ "none"))

write.csv(data, file = "data/email_data_expanded.csv",  row.names = FALSE)


data <- filter(data, city == "KANSAS_CITY" | city == "SAN_ANGELO" | city == "CARIBOU")

ggplot() +
  geom_point(data = data %>% filter(possible_error == "none"), 
             aes(observed_temp, forecast_temp),
             fill = "black", size = 3, pch = 21, alpha = 0.01) +
  geom_point(data = data %>% filter(possible_error != "none"),
             aes(observed_temp, forecast_temp, fill = possible_error),
             col = "black", pch = 21, size = 3, alpha = 0.75) +
  ggtitle("Observed and Forecast Temperatures") +
  labs(x = "Observed Temperature (F)",
       y = "Forecast Temperture (F)",
       fill = "Possible Error") +
  scale_fill_discrete(labels = c("Forecast Outlook",
                                 "Forecast Temperature",
                                 "Observed Temperature")) +
  theme_light() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0)) +
  guides(fill = guide_legend(nrow = 2))
