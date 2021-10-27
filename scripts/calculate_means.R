library(lubridate)
library(tidyverse)


data <- read.csv("scripts/email_data_reorganized.csv")
data$date <- parse_date_time(data$date, "%Y-%m-%d")


mean_diffs <- data %>%
  mutate(diff_lo_2_prev_PM = actual_lo_next_AM - forecast_lo_2_prev_PM,
         diff_hi_2_prev_PM = actual_hi_next_AM - forecast_hi_2_prev_PM,
         diff_lo_prev_AM = actual_lo_next_AM - forecast_lo_prev_AM,
         diff_hi_prev_AM = actual_hi_next_AM - forecast_hi_prev_AM,
         diff_lo_prev_PM = actual_lo_next_AM - forecast_lo_prev_PM,
         diff_hi_prev_PM = actual_hi_next_AM - forecast_hi_prev_PM,
         diff_lo_current_AM = actual_lo_next_AM - forecast_lo_current_AM,
         diff_hi_current_AM = actual_hi_next_AM - forecast_hi_current_AM) %>%
  select(date, city,
         diff_lo_2_prev_PM, diff_hi_2_prev_PM,
         diff_lo_prev_AM, diff_hi_prev_AM,
         diff_lo_prev_PM, diff_hi_prev_PM,
         diff_lo_current_AM, diff_hi_current_AM) %>%
  group_by(city) %>%
  summarize(mean_diff_lo_2_prev_PM = mean(diff_lo_2_prev_PM, na.rm = TRUE),
            mean_diff_hi_2_prev_PM = mean(diff_hi_2_prev_PM, na.rm = TRUE),
            mean_diff_lo_prev_AM = mean(diff_lo_prev_AM, na.rm = TRUE),
            mean_diff_hi_prev_AM = mean(diff_hi_prev_AM, na.rm = TRUE),
            mean_diff_lo_prev_PM = mean(diff_lo_prev_PM, na.rm = TRUE),
            mean_diff_hi_prev_PM = mean(diff_hi_prev_PM, na.rm = TRUE),
            mean_diff_lo_current_AM = mean(diff_lo_current_AM, na.rm = TRUE),
            mean_diff_hi_current_AM = mean(diff_hi_current_AM, na.rm = TRUE))

mean_diffs$mean_diff_lo <- apply(mean_diffs[,c(2, 4, 6, 8)], 1, mean)
mean_diffs$mean_diff_hi <- apply(mean_diffs[,c(3, 5, 7, 9)], 1, mean)


summary(mean_diffs)


hist(mean_diffs$mean_diff_lo)
hist(mean_diffs$mean_diff_hi)

c(quantile(mean_diffs$mean_diff_lo, 0.025), quantile(mean_diffs$mean_diff_lo, 0.975))
c(quantile(mean_diffs$mean_diff_hi, 0.025), quantile(mean_diffs$mean_diff_hi, 0.975))


plot_hi_lo <- data.frame(mean_diff = c(mean_diffs$mean_diff_lo, mean_diffs$mean_diff_hi),
                               hi_or_lo = c(rep("lo", 164), rep("hi", 164)))
plot_means <- plot_hi_lo %>%
  group_by(hi_or_lo) %>%
  summarize(mean = mean(mean_diff))

ggplot(plot_hi_lo, aes(x = mean_diff, fill = hi_or_lo, col = hi_or_lo)) + 
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


plot_hi_lo$mean_diff <- abs(plot_hi_lo$mean_diff)
plot_means <- plot_hi_lo %>%
  group_by(hi_or_lo) %>%
  summarize(mean = mean(mean_diff))

ggplot(plot_hi_lo, aes(x = mean_diff, fill = hi_or_lo, col = hi_or_lo)) + 
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


hist(mean_diffs$mean_diff_lo_2_prev_PM)
hist(mean_diffs$mean_diff_lo_prev_AM)
hist(mean_diffs$mean_diff_lo_prev_PM)
hist(mean_diffs$mean_diff_lo_current_AM)

c(quantile(mean_diffs$mean_diff_lo_2_prev_PM, 0.025), quantile(mean_diffs$mean_diff_lo_2_prev_PM, 0.975))
c(quantile(mean_diffs$mean_diff_lo_prev_AM, 0.025), quantile(mean_diffs$mean_diff_lo_prev_AM, 0.975))
c(quantile(mean_diffs$mean_diff_lo_prev_PM, 0.025), quantile(mean_diffs$mean_diff_lo_prev_PM, 0.975))
c(quantile(mean_diffs$mean_diff_lo_current_AM, 0.025), quantile(mean_diffs$mean_diff_lo_current_AM, 0.975))

plot_lo <- data.frame(mean_diff = c(mean_diffs$mean_diff_lo_2_prev_PM, mean_diffs$mean_diff_lo_prev_AM,
                                    mean_diffs$mean_diff_lo_prev_PM, mean_diffs$mean_diff_lo_current_AM),
                      day_and_time = c(rep("2 previous PM", 164), rep("previous AM", 164),
                                       rep("previous PM", 164), rep("current AM", 164)))
plot_means <- plot_lo %>%
  group_by(day_and_time) %>%
  summarize(mean = mean(mean_diff))

ggplot(plot_lo, aes(x = mean_diff, fill = day_and_time, col = day_and_time)) + 
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


hist(mean_diffs$mean_diff_hi_2_prev_PM)
hist(mean_diffs$mean_diff_hi_prev_AM)
hist(mean_diffs$mean_diff_hi_prev_PM)
hist(mean_diffs$mean_diff_hi_current_AM)


c(quantile(mean_diffs$mean_diff_hi_2_prev_PM, 0.025), quantile(mean_diffs$mean_diff_hi_2_prev_PM, 0.975))
c(quantile(mean_diffs$mean_diff_hi_prev_AM, 0.025), quantile(mean_diffs$mean_diff_hi_prev_AM, 0.975))
c(quantile(mean_diffs$mean_diff_hi_prev_PM, 0.025), quantile(mean_diffs$mean_diff_hi_prev_PM, 0.975))
c(quantile(mean_diffs$mean_diff_hi_current_AM, 0.025), quantile(mean_diffs$mean_diff_hi_current_AM, 0.975))


plot_hi <- data.frame(mean_diff = c(mean_diffs$mean_diff_hi_2_prev_PM, mean_diffs$mean_diff_hi_prev_AM,
                                    mean_diffs$mean_diff_hi_prev_PM, mean_diffs$mean_diff_hi_current_AM),
                      day_and_time = c(rep("2 previous PM", 164), rep("previous AM", 164),
                                       rep("previous PM", 164), rep("current AM", 164)))
plot_means <- plot_hi %>%
  group_by(day_and_time) %>%
  summarize(mean = mean(mean_diff))

ggplot(plot_hi, aes(x = mean_diff, fill = day_and_time, col = day_and_time)) + 
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