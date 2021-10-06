#' there are 12 temperatures corresponding to each day in the data:
#' 1. forecast (hi and lo) from two days previous PM
#' 2. forecast (hi and lo) from previous day AM
#' 3. forecast (hi and lo) from previous day PM
#' 4. forecast (hi and lo) from current day AM
#' 5. actual (hi and lo) from current day PM
#' 6. actual (hi and lo) from next day AM

#' so, for each day in the data, we need the rows corresponding to:
#' 1. two days previous PM
#' 2. previous day AM and PM
#' 3. current day AM and PM
#' 4. next day AM


library(tidyverse)
library(lubridate)

# read in the email data
df <- read.csv("scripts/email_data.csv")

# format date and time variable and add variables for hour and AM/PM
df$date_and_time <- parse_date_time(df$date_and_time, "%Y-%m-%d %H:%M:%S")
df <- df %>% mutate(date = date(date_and_time),
                    hour = hour(date_and_time))
df <- df %>% mutate(am_or_pm = case_when(hour < 12 ~ "AM",
                                         TRUE ~ "PM"))

organize_data <- function(param) {
  # split the parameter string to get the city and date
  split <- str_split_fixed(param, "---", 2)
  current_city <- str_trim(split[1])
  current_date <- date(parse_date_time(str_trim(split[2]), "%Y-%m-%d"))
  
  # create data frame with data for 2 days previous, previous day, current day, next day
  data <- df %>% filter(city == current_city,
                        date == current_date | date == current_date - 1 |
                          date == current_date - 2 & am_or_pm == "PM" |
                          date == current_date + 1 & am_or_pm == "AM") %>%
    select(city, date_and_time, date, am_or_pm, previous_lo, previous_hi,
           today_lo, today_hi, tomorrow_lo, tomorrow_hi)
  
  # create data frame with data for 2 days previous
  data_2_previous <- data %>% filter(city == current_city,
                                     date == current_date - 2 & am_or_pm == "PM") %>%
    select(city, date_and_time, date, am_or_pm, tomorrow_lo, tomorrow_hi)

  # create data frame with data for previous day 
  data_previous <- data %>% filter(city == current_city,
                                   date == current_date - 1) %>%
    select(city, date_and_time, date, am_or_pm, tomorrow_lo, tomorrow_hi, today_lo, today_hi)

  # create data frame with data for current day
  data_current <- data %>% filter(city == current_city,
                                  date == current_date) %>%
    select(city, date_and_time, date, am_or_pm, today_lo, today_hi, previous_lo, previous_hi)

  # create data frame with data for next day
  data_next <- data %>% filter(city == current_city,
                               date == current_date + 1 & am_or_pm == "AM") %>%
    select(city, date_and_time, date, am_or_pm, previous_lo, previous_hi)
  
  # create a new data frame with the data for the given city and date
  data_df <- data.frame(date = current_date, city = current_city, 
                        forecast_lo_2_prev_PM =  data_2_previous[1, "tomorrow_lo"],
                        forecast_hi_2_prev_PM =  data_2_previous[1, "tomorrow_hi"],
                        forecast_lo_prev_AM = data_previous[1, "tomorrow_lo"],
                        forecast_hi_prev_AM = data_previous[1, "tomorrow_hi"],
                        forecast_lo_prev_PM = data_previous[2, "today_lo"],
                        forecast_hi_prev_PM = data_previous[2, "today_hi"],
                        forecast_lo_current_AM = data_current[1, "today_lo"],
                        forecast_hi_current_AM = data_current[1, "today_hi"],
                        actual_lo_current_PM = data_current[2, "previous_lo"],
                        actual_hi_current_PM = data_current[2, "previous_hi"],
                        actual_lo_next_AM = data_next[1, "previous_lo"], 
                        actual_hi_next_AM = data_next[1, "previous_hi"])
  
  # return data frame (1 row)
  return(data_df)
}

# create vector of parameter strings (format: "<city> --- <date>") to feed into map_df()
params <- unique((df %>% select(city, date) %>% 
  mutate(param = paste(city, "---", date)))$param)

# create reorganized data frame
new_df <- map_df(params, organize_data)
colnames(new_df) <- c("date", "city", "forecast_lo_2_prev_PM", "forecast_hi_2_prev_PM",
                      "forecast_lo_prev_AM", "forecast_hi_prev_AM", "forecast_lo_prev_PM",
                      "forecast_hi_prev_PM", "forecast_lo_current_AM", "forecast_hi_current_AM",
                      "actual_lo_current_PM", "actual_hi_current_PM", "actual_lo_next_AM",
                      "actual_hi_next_AM")
new_df <- new_df %>% arrange(date)
write.csv(new_df, file = "scripts/email_data_reorganized.csv", row.names = FALSE)