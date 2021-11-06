library(lubridate)
library(tidyverse)


# create the mean errors data frame
find_mean_errors <- function() {
  # read in reorganized email data
  # format date variable properly
  data <- read.csv("data/email_data_reorganized.csv")
  data$date <- parse_date_time(data$date, "%Y-%m-%d")
  
  # create a data frame with errors for each (date, city) pair
  errors <- data %>%
    mutate(error_lo_2_prev_PM = actual_lo_next_AM - forecast_lo_2_prev_PM,
           error_hi_2_prev_PM = actual_hi_next_AM - forecast_hi_2_prev_PM,
           error_lo_prev_AM = actual_lo_next_AM - forecast_lo_prev_AM,
           error_hi_prev_AM = actual_hi_next_AM - forecast_hi_prev_AM,
           error_lo_prev_PM = actual_lo_next_AM - forecast_lo_prev_PM,
           error_hi_prev_PM = actual_hi_next_AM - forecast_hi_prev_PM,
           error_lo_current_AM = actual_lo_next_AM - forecast_lo_current_AM,
           error_hi_current_AM = actual_hi_next_AM - forecast_hi_current_AM) %>%
    select(date, city, 
           error_lo_2_prev_PM, error_hi_2_prev_PM,
           error_lo_prev_AM, error_hi_prev_AM,
           error_lo_prev_PM, error_hi_prev_PM,
           error_lo_current_AM, error_hi_current_AM, state)
  errors$city_and_state <- paste0(errors$city, ", ", errors$state)
  
  # create a data frame with mean errors for each city
  # add two columns containing the mean errors for all high and low forecasts
  mean_errors <- errors %>%
    group_by(city_and_state) %>%
    summarize(mean_error_lo_2_prev_PM = mean(error_lo_2_prev_PM, na.rm = TRUE),
              mean_error_hi_2_prev_PM = mean(error_hi_2_prev_PM, na.rm = TRUE),
              mean_error_lo_prev_AM = mean(error_lo_prev_AM, na.rm = TRUE),
              mean_error_hi_prev_AM = mean(error_hi_prev_AM, na.rm = TRUE),
              mean_error_lo_prev_PM = mean(error_lo_prev_PM, na.rm = TRUE),
              mean_error_hi_prev_PM = mean(error_hi_prev_PM, na.rm = TRUE),
              mean_error_lo_current_AM = mean(error_lo_current_AM, na.rm = TRUE),
              mean_error_hi_current_AM = mean(error_hi_current_AM, na.rm = TRUE))
  mean_errors$mean_error_lo <- apply(mean_errors[,c(2, 4, 6, 8)], 1, mean)
  mean_errors$mean_error_hi <- apply(mean_errors[,c(3, 5, 7, 9)], 1, mean)
  
  return(mean_errors)
}
mean_errors <- find_mean_errors()


# create a histogram of mean errors for high and low temperature forecasts
num_cities <- length(mean_errors$city_and_state)
plot_hi_lo <- data.frame(mean_error = c(mean_errors$mean_error_lo, mean_errors$mean_error_hi),
                         hi_or_lo = c(rep("lo", num_cities), rep("hi", num_cities)))

plot_means <- plot_hi_lo %>%
  group_by(hi_or_lo) %>%
  summarize(mean = mean(mean_error))

ggplot(plot_hi_lo, aes(x = mean_error, fill = hi_or_lo, col = hi_or_lo)) + 
  geom_histogram(bins = 25, position = "identity", alpha = 0.5) + 
  scale_fill_manual(values = c("red", "blue"),
                    name = "High or Low",
                    labels = c("High", "Low")) +
  scale_color_manual(values = c("red", "blue"),
                     name = "High or Low",
                     labels = c("High", "Low")) +
  geom_vline(data = plot_means, aes(xintercept = mean, color = hi_or_lo), linetype = "dashed") +
  xlab("Mean Error") +
  ylab("Count") +
  ggtitle("Mean Error When Predicting High and Low Temperatures") +
  theme_minimal()


# create a histogram of absolute value mean errors for high and low temperature forecasts
plot_hi_lo$mean_error <- abs(plot_hi_lo$mean_error)

plot_means <- plot_hi_lo %>%
  group_by(hi_or_lo) %>%
  summarize(mean = mean(mean_error))

ggplot(plot_hi_lo, aes(x = mean_error, fill = hi_or_lo, col = hi_or_lo)) + 
  geom_histogram(bins = 25, position = "identity", alpha = 0.5) + 
  scale_fill_manual(values = c("red", "blue"),
                    name = "High or Low",
                    labels = c("High", "Low")) +
  scale_color_manual(values = c("red", "blue"),
                     name = "High or Low",
                     labels = c("High", "Low")) +
  geom_vline(data = plot_means, aes(xintercept = mean, color = hi_or_lo), linetype = "dashed") +
  xlab("Mean Error") +
  ylab("Count") +
  ggtitle("Mean Absolute Value Error When Predicting High and Low Temperatures") +
  theme_minimal()


# create a histogram of mean errors for different low forecast days
plot_lo <- data.frame(mean_error = c(mean_errors$mean_error_lo_2_prev_PM, mean_errors$mean_error_lo_prev_AM,
                                     mean_errors$mean_error_lo_prev_PM, mean_errors$mean_error_lo_current_AM),
                      day_and_time = c(rep("2 previous PM", num_cities), rep("previous AM", num_cities),
                                       rep("previous PM", num_cities), rep("current AM", num_cities)))

plot_means <- plot_lo %>%
  group_by(day_and_time) %>%
  summarize(mean = mean(mean_error))

ggplot(plot_lo, aes(x = mean_error, fill = day_and_time, col = day_and_time)) + 
  geom_histogram(bins = 25, position = "identity", alpha = 0.5) + 
  scale_fill_manual(values = c("red", "green", "blue", "black"),
                    name = "Day",
                    labels = c("2 Days Previous PM", "Previous Day AM",
                               "Previous Day PM", "Current Day AM")) +
  scale_color_manual(values = c("red", "green", "blue", "black"),
                     name = "Day",
                     labels = c("2 Days Previous PM", "Previous Day AM",
                                "Previous Day PM", "Current Day AM")) +
  geom_vline(data = plot_means, aes(xintercept = mean, color = day_and_time), linetype = "dashed") +
  xlab("Mean Error") +
  ylab("Count") +
  ggtitle("Mean Error When Predicting Low Temperatures on Different Days") +
  theme_minimal()


# create a histogram of mean errors for different high forecast days
plot_hi <- data.frame(mean_error = c(mean_errors$mean_error_hi_2_prev_PM, mean_errors$mean_error_hi_prev_AM,
                                     mean_errors$mean_error_hi_prev_PM, mean_errors$mean_error_hi_current_AM),
                      day_and_time = c(rep("2 previous PM", num_cities), rep("previous AM", num_cities),
                                       rep("previous PM", num_cities), rep("current AM", num_cities)))

plot_means <- plot_hi %>%
  group_by(day_and_time) %>%
  summarize(mean = mean(mean_error))

ggplot(plot_hi, aes(x = mean_error, fill = day_and_time, col = day_and_time)) + 
  geom_histogram(bins = 25, position = "identity", alpha = 0.5) + 
  scale_fill_manual(values = c("red", "green", "blue", "black"),
                    name = "Day",
                    labels = c("2 Days Previous PM", "Previous Day AM",
                               "Previous Day PM", "Current Day AM")) +
  scale_color_manual(values = c("red", "green", "blue", "black"),
                     name = "Day",
                     labels = c("2 Days Previous PM", "Previous Day AM",
                                "Previous Day PM", "Current Day AM")) +
  geom_vline(data = plot_means, aes(xintercept = mean, color = day_and_time), linetype = "dashed") +
  xlab("Mean Error") +
  ylab("Count") +
  ggtitle("Mean Error When Predicting High Temperatures on Different Days") +
  theme_minimal()


# add the lat, long, and climate data to the mean_errors data frame
mean_errors <- find_mean_errors()
cities <- read.csv("data/cities.csv") %>%
  mutate(CITY = str_replace_all(CITY, '_', ' '),
         city_and_state = paste0(CITY, ", ", STATE))
mean_errors <- mean_errors %>%
  merge(cities, by = "city_and_state") %>%
  rename(city = CITY, state = STATE, lat = LAT, lon = LON, climate = CLIMATE) %>%
  filter(state %in% state.abb & state != "AK" & state != "HI")
MainStates <- map_data("state")


# plot mean errors for low temperatures with lat and long coordinates
plot_mean_error_lo <- ggplot() + 
  geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
               color = "black", fill = "white") +
  geom_point(data = mean_errors,
             aes(x = lon, y = lat, col = mean_error_lo), size = 4, alpha = 0.5) +
  scale_color_gradient(low = "green", high = "blue", na.value = NA, name = "Mean Error") +
  geom_label(data = mean_errors %>% filter(abs(mean_error_lo) > 1.7),
    aes(x = lon, y = lat, label = city_and_state), alpha = 0.5, size = 1.5) +
  labs(title = "Mean Errors for Low Temperature Forecasts") +
  theme_classic() +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.92, 0.2),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.2, "cm"))

ggsave("plots/mean_error_lo.png", plot = plot_mean_error_lo,
       width = unit(6, "in"), height = unit(4, "in"))

filter(mean_errors, abs(mean_error_lo) > 1.7) %>% select(city_and_state)


# plot mean errors for high temperatures with lat and long coordinates
plot_mean_error_hi <- ggplot() + 
  geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
               color = "black", fill = "white") +
  geom_point(data = mean_errors,
             aes(x = lon, y = lat, col = mean_error_hi), size = 4, alpha = 0.5) +
  scale_color_gradient(low = "orange", high = "blue", na.value = NA, name = "Mean Error") +
  geom_label(data = mean_errors %>% filter(abs(mean_error_hi) > 1),
             aes(x = lon, y = lat, label = city_and_state), alpha = 0.5, size = 1.5) +
  labs(title = "Mean Errors for High Temperature Forecasts") +
  theme_classic() +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = c(0.92, 0.2),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.2, "cm"))

ggsave("plots/mean_error_hi.png", plot = plot_mean_error_hi,
       width = unit(6, "in"), height = unit(4, "in"))

filter(mean_errors, abs(mean_error_hi) > 1.1) %>% select(city_and_state)


# plot climate classification with lat and long coordinates
ggplot() + 
  geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
               color = "black", fill = "white") +
  geom_point(data = mean_errors,
             aes(x = lon, y = lat, col = climate), size = 8, alpha = 0.9) +
  scale_color_discrete(name = "climate") +
  theme_classic() +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  guides(color = guide_legend(override.aes = list(size = 5)))


# more plots
ggplot(mean_errors, aes(x = lon, y = mean_error_lo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors, aes(x = mean_error_hi, y = lat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors, aes(x = climate, y = mean_error_hi)) +
  geom_point()

ggplot(mean_errors, aes(x = mean_error_hi, y = mean_error_lo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors, aes(x = mean_error_hi, y = mean_error_lo, col = climate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

summary(lm(mean_error_hi ~ climate, data = mean_errors))

summary(lm(mean_error_lo ~ lon, data = mean_errors))

summary(lm(mean_error_lo ~ mean_error_hi, data = mean_errors))