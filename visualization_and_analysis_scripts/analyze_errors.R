library(lubridate)
library(tidyverse)
library(ggpubr)

#'
#' CREATE DATA FRAMES
#'


# create errors data frame
errors <- create_error_df()

# create mean errors data frame
mean_errors <- create_mean_error_df(errors)

# create mean errors data frame with map information
mean_errors_map <- create_mean_error_df_map_info(mean_errors)


#'
#' ANALYSIS AND PLOTS
#'


# create histograms of the mean errors for high and low forecasts
plot_hist_hi_vs_lo(mean_errors, FALSE)

# create histograms of the absolute value mean errors for high and low forecasts
plot_hist_hi_vs_lo(mean_errors, TRUE)

# create histograms of the mean errors for low temperature forecasts on different days
plot_hist_diff_days(mean_errors, TRUE)

# create histograms of the mean errors for high temperature forecasts on different days
plot_hist_diff_days(mean_errors, FALSE)


# create and save maps with mean errors for cities
n <- 8

plot_mean_error_lo <- plot_mean_errors(mean_errors_map, TRUE, FALSE, n, 0)
plot_mean_error_lo_abs <- plot_mean_errors(mean_errors_map, TRUE, TRUE, n, 0)

ggsave("plots/mean_error_lo.png", plot = plot_mean_error_lo,
       width = unit(6, "in"), height = unit(4, "in"))
ggsave("plots/mean_error_lo_abs.png", plot = plot_mean_error_lo_abs,
       width = unit(6, "in"), height = unit(4, "in"))


plot_mean_error_hi <- plot_mean_errors(mean_errors_map, FALSE, FALSE, n, 0)
plot_mean_error_hi_abs <- plot_mean_errors(mean_errors_map, FALSE, TRUE, n, 0)

ggsave("plots/mean_error_hi.png", plot = plot_mean_error_hi,
       width = unit(6, "in"), height = unit(4, "in"))
ggsave("plots/mean_error_hi_abs.png", plot = plot_mean_error_hi_abs,
       width = unit(6, "in"), height = unit(4, "in"))


lo_current_AM <- plot_mean_errors(mean_errors_map, TRUE, FALSE, n, 1)
lo_prev_PM <- plot_mean_errors(mean_errors_map, TRUE, FALSE, n, 2)
lo_prev_AM <- plot_mean_errors(mean_errors_map, TRUE, FALSE, n, 3)
lo_2_prev_PM <- plot_mean_errors(mean_errors_map, TRUE, FALSE, n, 4)

plot_lo_diff_days <- ggarrange(lo_current_AM, lo_prev_PM, lo_prev_AM, lo_2_prev_PM,
                               ncol = 2, nrow = 2)
ggsave("plots/mean_error_lo_diff_days.png", plot = plot_lo_diff_days,
       width = unit(14, "in"), height = unit(10, "in"))


abs_lo_current_AM <- plot_mean_errors(mean_errors_map, TRUE, TRUE, n, 1)
abs_lo_prev_PM <- plot_mean_errors(mean_errors_map, TRUE, TRUE, n, 2)
abs_lo_prev_AM <- plot_mean_errors(mean_errors_map, TRUE, TRUE, n, 3)
abs_lo_2_prev_PM <- plot_mean_errors(mean_errors_map, TRUE, TRUE, n, 4)

plot_abs_lo_diff_days <- ggarrange(abs_lo_current_AM, abs_lo_prev_PM,
                                   abs_lo_prev_AM, abs_lo_2_prev_PM,
                                   ncol = 2, nrow = 2)
ggsave("plots/abs_mean_error_lo_diff_days.png", plot = plot_abs_lo_diff_days,
       width = unit(14, "in"), height = unit(10, "in"))


hi_current_AM <- plot_mean_errors(mean_errors_map, FALSE, FALSE, n, 1)
hi_prev_PM <- plot_mean_errors(mean_errors_map, FALSE, FALSE, n, 2)
hi_prev_AM <- plot_mean_errors(mean_errors_map, FALSE, FALSE, n, 3)
hi_2_prev_PM <- plot_mean_errors(mean_errors_map, FALSE, FALSE, n, 4)

plot_hi_diff_days <- ggarrange(hi_current_AM, hi_prev_PM, hi_prev_AM, hi_2_prev_PM,
                               ncol = 2, nrow = 2)
ggsave("plots/mean_error_hi_diff_days.png", plot = plot_hi_diff_days,
       width = unit(14, "in"), height = unit(10, "in"))


abs_hi_current_AM <- plot_mean_errors(mean_errors_map, FALSE, TRUE, n, 1)
abs_hi_prev_PM <- plot_mean_errors(mean_errors_map, FALSE, TRUE, n, 2)
abs_hi_prev_AM <- plot_mean_errors(mean_errors_map, FALSE, TRUE, n, 3)
abs_hi_2_prev_PM <- plot_mean_errors(mean_errors_map, FALSE, TRUE, n, 4)

plot_abs_hi_diff_days <- ggarrange(abs_hi_current_AM, abs_hi_prev_PM,
                                   abs_hi_prev_AM, abs_hi_2_prev_PM,
                                   ncol = 2, nrow = 2)
ggsave("plots/abs_mean_error_hi_diff_days.png", plot = plot_abs_hi_diff_days,
       width = unit(14, "in"), height = unit(10, "in"))


# perform paired t tests and plot results

#' 
#' paired t test results:
#' 
#' for high temperatures: 
#'   using actual values reported next AM:
#'     error 2 prev PM compared to error current AM: 157/160 significant
#'     error 2 prev PM compared to error prev PM: 103/160 significant
#'     error 2 prev PM compared to error prev AM: 61/160 significant
#'   all significant differences are positive (so errors get smaller over time)
#' 
#' for low temperatures:
#'   using actual values reported next AM:
#'     error 2 prev PM compared to error current AM: 160/160 significant
#'     ***error 2 prev PM compared to error prev PM: 24/160 significant
#'       errors are positive and negative!!!
#'     error 2 prev PM compared to error prev AM: 159/160 significant
#'     
#'  PREV PM IS A PROBLEM!
#' 

summary(errors$error_lo_2_prev_PM - errors$error_lo_current_AM)
summary(errors$error_lo_2_prev_PM - errors$error_lo_prev_PM)
summary(errors$error_lo_2_prev_PM - errors$error_lo_prev_AM)

cities <- read.csv("data/cities.csv") %>%
  mutate(CITY = str_replace_all(CITY, '_', ' '),
         city_and_state = paste0(CITY, ", ", STATE))

errors %>%
  group_by(city_and_state) %>%
  summarize(p = t.test(abs(error_lo_prev_AM), abs(error_lo_prev_PM), paired = TRUE)$p.value,
            diff = mean(abs(error_lo_prev_AM) - abs(error_lo_prev_PM), na.rm = TRUE)) %>%
  filter(p < 0.05) %>%
  merge(cities, by = "city_and_state") %>%
  rename(city = CITY, state = STATE, lat = LAT, lon = LON, climate = CLIMATE) %>%
  filter(state %in% state.abb & state != "AK" & state != "HI") %>%
  select(city_and_state, p, diff) %>%
  summarize(better = sum(diff > 0),
            worse = sum(diff < 0))

ggsave("plots/p_vals_lo_sig.png", plot = plot_p_vals(errors_p_map, TRUE, TRUE),
       width = unit(12, "in"), height = unit(4, "in"))

ggsave("plots/p_vals_hi_sig.png", plot = plot_p_vals(errors_p_map, FALSE, TRUE),
       width = unit(12, "in"), height = unit(4, "in"))


# more plots and some basic models
num_cities <- length(mean_errors_map$city_and_state)
mean_errors_combine <- data.frame(city_and_state = rep(mean_errors_map$city_and_state, 2),
                                  lat = rep(mean_errors_map$lat, 2),
                                  lon = rep(mean_errors_map$lon, 2),
                                  mean_error = c(mean_errors_map$mean_error_lo, mean_errors_map$mean_error_hi),
                                  hi_lo = c(rep("lo", num_cities), rep("hi", num_cities)))

ggplot(mean_errors_combine, aes(x = lon, y = mean_error, col = hi_lo)) +
  geom_point(size = 2) +
  labs(title = "Mean Error ~ Longitude", x = "longitude", y = "mean error") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors_combine, aes(x = lon, y = abs(mean_error), col = hi_lo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors_combine, aes(x = lat, y = mean_error, col = hi_lo)) +
  geom_point(size = 2) +
  labs(title = "Mean Error ~ Latitude", x = "latitute", y = "mean error") + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors_combine, aes(x = lat, y = abs(mean_error), col = hi_lo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors, aes(x = lon, y = mean_error_lo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors, aes(x = mean_error_hi, y = mean_error_lo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(mean_errors, aes(x = mean_error_hi, y = mean_error_lo, col = climate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


cor(mean_errors_map$lon, mean_errors_map$mean_error_lo)
summary(lm(mean_error_lo ~ lon, data = mean_errors_map))

cor(mean_errors_map$lat, mean_errors_map$mean_error_hi)
summary(lm(mean_error_hi ~ climate + lat, data = mean_errors))

cor(mean_errors$mean_error_lo, mean_errors$mean_error_hi)
summary(lm(mean_error_lo ~ mean_error_hi, data = mean_errors))