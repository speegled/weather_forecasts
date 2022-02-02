library(lubridate)
library(tidyverse)
library(ggpubr)


#'
#' FUNCTION DEFINITIONS
#'

# function that returns the error data frame
create_error_df <- function() {
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
    # mutate(error_lo_2_prev_PM = actual_lo_current_PM - forecast_lo_2_prev_PM,
    #        error_hi_2_prev_PM = actual_hi_current_PM - forecast_hi_2_prev_PM,
    #        error_lo_prev_AM = actual_lo_current_PM - forecast_lo_prev_AM,
    #        error_hi_prev_AM = actual_hi_current_PM - forecast_hi_prev_AM,
    #        error_lo_prev_PM = actual_lo_current_PM - forecast_lo_prev_PM,
    #        error_hi_prev_PM = actual_hi_current_PM - forecast_hi_prev_PM,
    #        error_lo_current_AM = actual_lo_current_PM - forecast_lo_current_AM,
    #        error_hi_current_AM = actual_hi_current_PM - forecast_hi_current_AM) %>%
    select(date, city, 
           error_lo_2_prev_PM, error_hi_2_prev_PM,
           error_lo_prev_AM, error_hi_prev_AM,
           error_lo_prev_PM, error_hi_prev_PM,
           error_lo_current_AM, error_hi_current_AM, state)
  errors$city_and_state <- paste0(errors$city, ", ", errors$state)
  
  return(errors)
}


# function that returns the mean error data frame
create_mean_error_df <- function(errors) {
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


# function that adds the lat, long, and climate data to the mean_errors data frame
create_mean_error_df_map_info <- function(mean_errors) {
  cities <- read.csv("data/cities.csv") %>%
    mutate(CITY = str_replace_all(CITY, '_', ' '),
           city_and_state = paste0(CITY, ", ", STATE))
  mean_errors <- mean_errors %>%
    merge(cities, by = "city_and_state") %>%
    rename(city = CITY, state = STATE, lat = LAT, lon = LON, climate = CLIMATE) %>%
    filter(state %in% state.abb & state != "AK" & state != "HI")
  return(mean_errors)
}


# function that returns a data frame with paired t test p values
create_error_df_p_map_info <- function(errors) {
  cities_p <- errors %>%
    group_by(city_and_state) %>%
    summarize(p_lo_current_AM = t.test(abs(error_lo_2_prev_PM), abs(error_lo_current_AM), paired = TRUE)$p.value,
              p_hi_current_AM = t.test(abs(error_hi_2_prev_PM), abs(error_hi_current_AM), paired = TRUE)$p.value,
              mean_diff_lo_current_AM = mean(abs(error_lo_2_prev_PM) - abs(error_lo_current_AM), na.rm = TRUE),
              mean_diff_hi_current_AM = mean(abs(error_hi_2_prev_PM) - abs(error_hi_current_AM), na.rm = TRUE),
              p_lo_prev_PM = t.test(abs(error_lo_2_prev_PM), abs(error_lo_prev_PM), paired = TRUE)$p.value,
              p_hi_prev_PM = t.test(abs(error_hi_2_prev_PM), abs(error_hi_prev_PM), paired = TRUE)$p.value,
              mean_diff_lo_prev_PM = mean(abs(error_lo_2_prev_PM) - abs(error_lo_prev_PM), na.rm = TRUE),
              mean_diff_hi_prev_PM = mean(abs(error_hi_2_prev_PM) - abs(error_hi_prev_PM), na.rm = TRUE),
              p_lo_prev_AM = t.test(abs(error_lo_2_prev_PM), abs(error_lo_prev_AM), paired = TRUE)$p.value,
              p_hi_prev_AM = t.test(abs(error_hi_2_prev_PM), abs(error_hi_prev_AM), paired = TRUE)$p.value,
              mean_diff_lo_prev_AM = mean(abs(error_lo_2_prev_PM) - abs(error_lo_prev_AM), na.rm = TRUE),
              mean_diff_hi_prev_AM = mean(abs(error_hi_2_prev_PM) - abs(error_hi_prev_AM), na.rm = TRUE))
  
  cities <- read.csv("data/cities.csv") %>%
    mutate(CITY = str_replace_all(CITY, '_', ' '),
           city_and_state = paste0(CITY, ", ", STATE))
  cities_p <- cities_p %>%
    merge(cities, by = "city_and_state") %>%
    rename(city = CITY, state = STATE, lat = LAT, lon = LON, climate = CLIMATE) %>%
    filter(state %in% state.abb & state != "AK" & state != "HI")
  
  return(cities_p)
}


# function that returns histograms of mean errors for high and low temperature forecasts
plot_hist_hi_vs_low <- function(mean_errors, abs) {
  num_cities <- length(mean_errors$city_and_state)
  
  plot_hi_lo <- data.frame(mean_error = c(mean_errors$mean_error_lo,
                                          mean_errors$mean_error_hi),
                           hi_or_lo = c(rep("lo", num_cities), rep("hi", num_cities)))
  if (abs) {
    plot_hi_lo$mean_error <- abs(plot_hi_lo$mean_error)
    title <- "Absolute Value Mean Error When Predicting High and Low Temperatures"
    xlab <- "Absolute Value Mean Error"
  }
  else {
    title <- "Mean Error When Predicting High and Low Temperatures"
    xlab <- "Mean Error"
  }
  plot_means <- plot_hi_lo %>%
    group_by(hi_or_lo) %>%
    summarize(mean = mean(mean_error))
  
  plot <- ggplot(plot_hi_lo, aes(x = mean_error, fill = hi_or_lo, col = hi_or_lo)) + 
    geom_histogram(bins = 25, position = "identity", alpha = 0.5) + 
    scale_fill_manual(values = c("red", "blue"),
                      name = "High or Low",
                      labels = c("High", "Low")) +
    scale_color_manual(values = c("red", "blue"),
                       name = "High or Low",
                       labels = c("High", "Low")) +
    geom_vline(data = plot_means,
               aes(xintercept = mean, color = hi_or_lo),
               linetype = "dashed") +
    xlab(xlab) +
    ylab("Count") +
    ggtitle(title) +
    theme_minimal()
  
  return(plot)
}


# function that returns histograms of mean errors for high and low temperature forecasts on different days
plot_hist_diff_days <- function(mean_errors, lo) {
  num_cities <- length(mean_errors$city_and_state)
  
  if (lo) {
    plot_data <- data.frame(mean_error = c(mean_errors$mean_error_lo_2_prev_PM,
                                           mean_errors$mean_error_lo_prev_AM,
                                           mean_errors$mean_error_lo_prev_PM,
                                           mean_errors$mean_error_lo_current_AM),
                            day_and_time = c(rep("2 previous PM", num_cities),
                                             rep("previous AM", num_cities),
                                             rep("previous PM", num_cities),
                                             rep("current AM", num_cities)))
    title <- "Mean Error When Predicting Low Temperatures on Different Days"
  }
  else {
    plot_data <- data.frame(mean_error = c(mean_errors$mean_error_hi_2_prev_PM,
                                           mean_errors$mean_error_hi_prev_AM,
                                           mean_errors$mean_error_hi_prev_PM,
                                           mean_errors$mean_error_hi_current_AM),
                            day_and_time = c(rep("2 previous PM", num_cities),
                                             rep("previous AM", num_cities),
                                             rep("previous PM", num_cities),
                                             rep("current AM", num_cities)))
    title <- "Mean Error When Predicting High Temperatures on Different Days"
  }
  plot_means <- plot_data %>%
    group_by(day_and_time) %>%
    summarize(mean = mean(mean_error))
  
  plot <- ggplot(plot_data, aes(x = mean_error, fill = day_and_time, col = day_and_time)) + 
    geom_histogram(bins = 25, position = "identity", alpha = 0.5) + 
    scale_fill_manual(values = c("red", "green", "blue", "black"),
                      name = "Day",
                      labels = c("2 Days Previous PM", "Previous Day AM",
                                 "Previous Day PM", "Current Day AM")) +
    scale_color_manual(values = c("red", "green", "blue", "black"),
                       name = "Day",
                       labels = c("2 Days Previous PM", "Previous Day AM",
                                  "Previous Day PM", "Current Day AM")) +
    geom_vline(data = plot_means,
               aes(xintercept = mean, color = day_and_time),
               linetype = "dashed") +
    xlab("Mean Error") +
    ylab("Count") +
    ggtitle(title) +
    theme_minimal()
  
  return(plot)
}


#'
#' function that returns a map with mean errors for cities
#'   day = 0: overall mean forecast error for forecasts on all days
#'   day = 1: mean forecast error for current day AM forecasts 
#'   day = 2: mean forecast error for previous day PM forecasts
#'   day = 3: mean forecast error for previous day AM forecasts 
#'   day = 4: mean forecast error for 2 days previous PM forecasts
#' 
plot_mean_errors <- function(mean_errors, lo, abs, n, day) {
  MainStates <- map_data("state")
  
  if (lo) {
    colors <- c("green", "blue")
    if (day == 0) {
      title <- "Mean Errors for Low Temperature Forecasts All Days"
      data <- mean_errors %>% select(lon, lat, mean_error_lo, city_and_state) %>%
        rename(mean_error = mean_error_lo)
    }
    else if (day == 1) {
      title <- "Mean Errors for Low Temperature Forecasts Current Day AM"
      data <- mean_errors %>% select(lon, lat, mean_error_lo_current_AM, city_and_state) %>%
        rename(mean_error = mean_error_lo_current_AM)
    }
    else if (day == 2) {
      title <- "Mean Errors for Low Temperature Forecasts Previous Day PM"
      data <- mean_errors %>% select(lon, lat, mean_error_lo_prev_PM, city_and_state) %>%
        rename(mean_error = mean_error_lo_prev_PM)
    }
    else if (day == 3) {
      title <- "Mean Errors for Low Temperature Forecasts Previous Day AM"
      data <- mean_errors %>% select(lon, lat, mean_error_lo_prev_AM, city_and_state) %>%
        rename(mean_error = mean_error_lo_prev_AM)
    }
    else if (day == 4) {
      title <- "Mean Errors for Low Temperature Forecasts 2 Days Previous PM"
      data <- mean_errors %>% select(lon, lat, mean_error_lo_2_prev_PM, city_and_state) %>%
        rename(mean_error = mean_error_lo_2_prev_PM)
    }
    if (abs) {
      data$mean_error <- abs(data$mean_error)
      title <- paste0("Absolute Value ", title)
    }
  }
  else {
    colors <- c("orange", "blue")
    if (day == 0) {
      data <- mean_errors %>% select(lon, lat, mean_error_hi, city_and_state) %>%
        rename(mean_error = mean_error_hi)
      title <- "Mean Errors for High Temperature Forecasts All Days"
    }
    else if (day == 1) {
      data <- mean_errors %>% select(lon, lat, mean_error_hi_current_AM, city_and_state) %>%
        rename(mean_error = mean_error_hi_current_AM)
      title <- "Mean Errors for High Temperature Forecasts Current Day AM"
    }
    else if (day == 2) {
      data <- mean_errors %>% select(lon, lat, mean_error_hi_prev_PM, city_and_state) %>%
        rename(mean_error = mean_error_hi_prev_PM)
      title <- "Mean Errors for High Temperature Forecasts Previous Day PM"
    }
    else if (day == 3) {
      data <- mean_errors %>% select(lon, lat, mean_error_hi_prev_AM, city_and_state) %>%
        rename(mean_error = mean_error_hi_prev_AM)
      title <- "Mean Errors for High Temperature Forecasts Previous Day AM"
    }
    else if (day == 4) {
      data <- mean_errors %>% select(lon, lat, mean_error_hi_2_prev_PM, city_and_state) %>%
        rename(mean_error = mean_error_hi_2_prev_PM)
      title <- "Mean Errors for High Temperature Forecasts 2 Days Previous PM"
    }
    if (abs) {
      data$mean_error <- abs(data$mean_error)
      title <- paste0("Absolute Value ", title)
    }
  }
  
  plot <- ggplot() + 
    geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
                 color = "black", fill = "white") +
    geom_point(data = data,
               aes(x = lon, y = lat, col = mean_error, size = lon), alpha = 0.4) +
    scale_color_gradient(low = colors[1], high = colors[2], na.value = NA, name = "Mean Error") +
    geom_label(data = (data %>% arrange((abs(mean_error))))[1:n,],
               aes(x = lon, y = lat, label = city_and_state), alpha = 0.5, size = 1.5, col = "black") +
    geom_label(data = (data %>% arrange(desc(abs(mean_error))))[1:n,],
               aes(x = lon, y = lat, label = city_and_state), alpha = 0.5, size = 1.5, col = "red") +
    labs(title = title) +
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
          legend.key.size = unit(0.2, "cm")) +
    scale_size(range = c(20, 10), guide = "none")
  
  return(plot)
}


# function that plots paired t test p values on a map
plot_p_vals <- function(cities_p, lo, sig) {
  MainStates <- map_data("state")
  
  if (lo) {
    cities_p$p_current_AM <- cities_p$p_lo_current_AM
    cities_p$p_prev_PM <- cities_p$p_lo_prev_PM
    colors <- c("springgreen", "blue")
  }
  else {
    cities_p$p_current_AM <- cities_p$p_hi_current_AM
    cities_p$p_prev_PM <- cities_p$p_hi_prev_PM
    colors <- c("gold", "red")
  }
  
  if (sig) {
    cities_p_current <- filter(cities_p, p_current_AM < 0.05)
  }
  else {
    cities_p_current <- cities_p
  }
  plot_current <- ggplot() + 
    geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
                 color = "black", fill = "white") +
    geom_point(data = cities_p_current,
               aes(x = lon, y = lat, col = p_current_AM), size = 8, alpha = 0.5) +
    scale_color_gradient(low = colors[1], high = colors[2]) +
    theme_classic() +
    labs(title = "Error From 2 Prev PM Compared to Error From Current AM") +
    theme(axis.line = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  if (sig) {
    cities_p_prev <- filter(cities_p, p_prev_PM < 0.05)
  }
  else {
    cities_p_prev <- cities_p
  }
  plot_prev_PM <- ggplot() + 
    geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
                 color = "black", fill = "white") +
    geom_point(data = cities_p_prev,
               aes(x = lon, y = lat, col = p_prev_PM), size = 8, alpha = 0.5) +
    scale_color_gradient(low = colors[1], high = colors[2]) +
    theme_classic() +
    labs(title = "Error From 2 Prev PM Compared to Error From Prev PM") +
    theme(axis.line = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  plot <- ggarrange(plot_current, plot_prev_PM, ncol = 2, nrow = 1)
  
  return(plot)
}