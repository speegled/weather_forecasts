library(tidyverse)
library(lubridate)
library(hms)

df <- read.csv("email_data.csv")

df$date_and_time <- parse_date_time(df$date_and_time, "%Y-%m-%d %H:%M:%S")
df <- df %>% mutate(date = date(date_and_time),
                    time = as_hms(date_and_time)) %>%
  mutate(am_or_pm = case_when(time < hms("12:00:00") ~ "AM",
                              TRUE ~ "PM"))

new_df <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(new_df) <- c("city", "date", "forecast_lo_2_prev_PM", "forecast_hi_2_prev_PM",
                      "forecast_lo_prev_AM", "forecast_hi_prev_AM", "forecast_lo_prev_PM",
                      "forecast_hi_prev_PM", "forecast_lo_current_AM", "forecast_hi_current_AM",
                      "actual_lo_current_PM", "actual_hi_current_PM", "actual_lo_next_AM",
                      "actual_hi_next_AM")


#' 6 relevant temperatures for each day:
#' 1. forecast two days previous PM
#' 2. forecast previous day AM
#' 3. forecast previous day PM
#' 4. forecast current day AM
#' 5. actual current day PM
#' 6. actual next day AM

#' for each day we need:
#' 1. two days previous PM
#' 2. previous day AM, PM
#' 3. current day AM, PM
#' 4. next day AM


organize_data <- function(params) {
  current_city <- params[1][1,1]
  current_date <- params[2][1,1]
  
  data <- df %>% filter(city == current_city,
                        date == current_date | date == current_date - 1 |
                          date == current_date - 2 & am_or_pm == "PM" |
                          date == current_date + 1 & am_or_pm == "AM") %>%
    select(city, date_and_time, date, am_or_pm, previous_lo, previous_hi,
           today_lo, today_hi, tomorrow_lo, tomorrow_hi) %>%
    arrange(date_and_time)

  data_df <- data.frame(city = as.character(city), date = as.character(date),
                        forecast_lo_2_prev_PM =  data[1, "tomorrow_lo"],
                        forecast_hi_2_prev_PM =  data[1, "tomorrow_hi"],
                        forecast_lo_prev_AM = data[2, "tomorrow_lo"],
                        forecast_hi_prev_AM = data[2, "tomorrow_hi"],
                        forecast_lo_prev_PM = data[3, "today_lo"],
                        forecast_hi_prev_PM = data[3, "today_hi"],
                        forecast_lo_current_AM = data[4, "today_lo"],
                        forecast_hi_current_AM = data[4, "today_hi"],
                        actual_lo_current_PM = data[5, "previous_lo"],
                        actual_hi_current_PM = data[5, "previous_hi"],
                        actual_lo_next_AM = data[6, "previous_lo"], 
                        actual_hi_next_AM = data[6, "previous_hi"])
  
  return(data_df)
}

cities_dates <- df %>% select(city, date)
params <- cities_dates[1,]
organize_data(cities_dates[1,])