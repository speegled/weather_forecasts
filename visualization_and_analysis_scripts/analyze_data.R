library(tidyverse)

data <- read.csv("data/email_data_expanded.csv")

plot(data$observed_temp, data$forecast_temp)
ggplot(data, aes(observed_temp, forecast_temp)) +
  geom_point() +
  geom_smooth(method = "lm")
summary(lm(observed_temp ~ forecast_temp, data))

# outliers with forecast_temp = -10 and much higher observed_temp
filter(data, forecast_temp < 0 & observed_temp > 25)

precip_outlooks <- c("SNOW", "RAIN", "SHWRS", "FZDRZL", "SNOSHW",
                     "RNSNOW", "SLEET", "FZRAIN", "TSTRMS", "BLZZRD",
                     "FLYRRS", "BLGSNO", "DRZL")
data %>% filter(!is.na(observed_precip), !is.na(forecast_outlook)) %>%
  mutate(precip = observed_precip > 0) %>%
  group_by(forecast_outlook) %>%
  summarize(p_precip = sum(precip) / n()) %>%
  arrange(-p_precip) %>%
  as.data.frame()

hist(filter(data, forecast_outlook == "VRYHOT")$forecast_temp)
ggplot(filter(data, forecast_outlook == "VRYHOT"),
       aes(observed_temp, forecast_temp)) +
  geom_point()

hist(filter(data, forecast_outlook == "VRYCLD")$forecast_temp)
ggplot(filter(data, forecast_outlook == "VRYCLD"),
       aes(observed_temp, forecast_temp)) +
  geom_point()

precip_outlooks_cold <- c("SNOW", "FZDRZL", "SNOSHW", "RNSNOW", "SLEET",
                          "FZRAIN", "BLZZRD", "FLRRYS", "BLGSNO")
ggplot(filter(data, forecast_outlook %in% precip_outlooks_cold),
       aes(observed_temp, forecast_temp)) +
  geom_point()

mutate(data, abs_error = abs(forecast_temp - observed_temp)) %>% 
  arrange(-abs_error) %>% 
  select(city, state, date, high_or_low, observed_temp, forecast_temp, abs_error) %>%
  top_n(50) 