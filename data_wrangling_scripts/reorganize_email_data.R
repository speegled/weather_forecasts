library(tidyverse)
library(lubridate)
library(dtplyr)
library(data.table)


# read in email data
df <- read.csv("data/email_data.csv")

# REMOVE LATER!
df <- filter(df, city == "ST LOUIS")

# format date and time variable and add variables for time and AM or PM
df$date_and_time <- ymd_hms(df$date_and_time)
df <- df %>% mutate(date = date(date_and_time),
                    time_s = second(date_and_time) + 60 * minute(date_and_time) + 60**2 * hour(date_and_time))
df <- df %>% mutate(am_or_pm = case_when(hour(date_and_time) < 12 ~ "AM",
                                         TRUE ~ "PM"))

# format city variable (replace spaces with underscores)
df$city <- str_replace(df$city, " ", "_")

# remove duplicate (date, am_or_pm) pairs
df <- df %>% group_by(city, date, am_or_pm) %>%
  filter(time_s > 60**2) %>%
  filter(time_s == min(time_s))
df <- select(df, -time_s)

# create sequences of all dates and all cities in email data
seq_dates <- seq(min(df$date_and_time), max(df$date_and_time), by = 43200)
n_dates <- length(seq_dates)
seq_cities <- unique(df$city)
n_cities <- length(seq_cities)

# create data frame with all possible (city, date, am_or_pm) combinations
# fill in the temperature, outlook, and precipitation values with NA
df_all_dates <- data.frame(date_and_time = rep(seq_dates, each = n_cities),
                           city = rep(seq_cities, n_dates),
                           previous_lo = rep(NA, n_dates * n_cities),
                           previous_hi = rep(NA, n_dates * n_cities),
                           previous_precip = rep(NA, n_dates * n_cities),
                           today_lo = rep(NA, n_dates * n_cities),
                           today_hi = rep(NA, n_dates * n_cities),
                           today_outlook = rep(NA, n_dates * n_cities),
                           tomorrow_lo = rep(NA, n_dates * n_cities),
                           tomorrow_hi = rep(NA, n_dates * n_cities),
                           tomorrow_outlook = rep(NA, n_dates * n_cities),
                           date = ymd(rep(format(seq_dates, "%Y-%m-%d"), each = n_cities)),
                           am_or_pm = rep(c(rep("AM", n_cities), rep("PM", n_cities)), n_dates / 2))

# add rows of NAs for missing (city, date, am_or_pm) combinations in df
df$city_date_am_or_pm <- paste(df$city, df$date, df$am_or_pm)
df_all_dates$city_date_am_or_pm <- paste(df_all_dates$city,
                                         df_all_dates$date, 
                                         df_all_dates$am_or_pm)
`%notin%` <- Negate(`%in%`)
df_missing_dates <- filter(df_all_dates, city_date_am_or_pm %notin% df$city_date_am_or_pm)
df <- rbind(df, df_missing_dates) %>% select(-city_date_am_or_pm) %>% arrange(date_and_time)

# find the dates in the email data that are not already in the reorganized data frame
all_dates <- unique(ymd(df$date))
email_data_reorganized <- read.csv("data/email_data_reorganized.csv")
email_data_reorganized$date <- ymd(email_data_reorganized$date)
email_data_reorganized <- filter(email_data_reorganized, year(date) == 2021)
existing_dates <- unique(email_data_reorganized$date)
new_dates <- ymd(setdiff(as.character(all_dates), as.character(existing_dates)))

organize_data <- function(param) {
  # split the parameter string to get the city and date
  split <- str_split_fixed(param, "---", 2)
  current_city <- str_trim(split[1])
  current_date <- date(parse_date_time(str_trim(split[2]), "%Y-%m-%d"))
  
  df <- lazy_dt(df)
  
  # create data frame with data for 2 days previous, previous day, current day, next day
  data <- df %>% filter(city == current_city,
                        date == current_date | date == current_date - 1 |
                          date == current_date - 2 & am_or_pm == "PM" | 
                          date == current_date + 1 & am_or_pm == "AM") %>%
    select(city, date_and_time, date, am_or_pm,
           previous_lo, previous_hi, previous_precip,
           today_lo, today_hi, today_outlook, 
           tomorrow_lo, tomorrow_hi, tomorrow_outlook) %>% as.data.frame()
  
  data <- lazy_dt(data)
  
  # create data frame with data for 2 days previous
  data_2_previous <- data %>% filter(city == current_city, date == current_date - 2) %>%
    select(city, date_and_time, date, am_or_pm,
           tomorrow_lo, tomorrow_hi, tomorrow_outlook) %>% as.data.frame()
  
  # create data frame with data for previous day 
  data_previous <- data %>% filter(city == current_city, date == current_date - 1) %>%
    select(city, date_and_time, date, am_or_pm,
           tomorrow_lo, tomorrow_hi, tomorrow_outlook, 
           today_lo, today_hi, today_outlook) %>% as.data.frame()
  
  # create data frame with data for current day
  data_current <- data %>% filter(city == current_city, date == current_date) %>%
    select(city, date_and_time, date, am_or_pm,
           today_lo, today_hi, today_outlook,
           previous_lo, previous_hi, previous_precip) %>% as.data.frame()
  
  # create data frame with data for next day
  data_next <- data %>% filter(city == current_city, date == current_date + 1) %>%
    select(city, date_and_time, date, am_or_pm,
           previous_lo, previous_hi, previous_precip) %>% as.data.frame()
  
  # create a new data frame with the data for the given city and date
  data_df <- data.frame(date = current_date, city = current_city, 
                        forecast_hi_2_prev_PM =  data_2_previous[1, "tomorrow_hi"],
                        forecast_lo_prev_AM = data_previous[1, "tomorrow_lo"],
                        forecast_hi_prev_AM = data_previous[1, "tomorrow_hi"],
                        forecast_lo_prev_PM = data_previous[2, "tomorrow_lo"],
                        forecast_hi_prev_PM = data_previous[2, "today_hi"],
                        forecast_lo_current_AM = data_current[1, "today_lo"],
                        forecast_hi_current_AM = data_current[1, "today_hi"],
                        forecast_lo_current_PM = data_current[2, "today_lo"],
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
colnames(new_df) <- c("date", "city", "forecast_hi_2_prev_PM", "forecast_lo_prev_AM", 
                      "forecast_hi_prev_AM", "forecast_lo_prev_PM", "forecast_hi_prev_PM", 
                      "forecast_lo_current_AM", "forecast_hi_current_AM", "forecast_lo_current_PM", 
                      "actual_lo_current_PM", "actual_hi_current_PM", "actual_lo_next_AM", "actual_hi_next_AM")
new_df <- new_df %>% arrange(date)
write.csv(new_df, file = "data/email_data_reorganized.csv", row.names = FALSE)


# separate state abbreviation from city names if existing and add for those not there
state_abreviations <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID", "IL","IN","KS","KY","LA","MA","MD","ME","MH","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY", "OH","OK","OR","PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
cities <-  read.csv("data/cities.csv", header = TRUE) %>% select(CITY, STATE) %>%
  mutate(city = str_replace_all(CITY, '_', ' ')) %>%
  select(city, state = STATE)

df <- read.csv("data/email_data_reorganized.csv") # import data written above
df$city <- as.character(df$city)
df <- df %>%
  mutate(last_two = substr(city, nchar(city) - 1, nchar(city)),
         without_last_two = substr(city, 1, nchar(city) - 3),
         middle_chr = substr(city, nchar(city) - 2, nchar(city) - 2),
         new_city = ifelse((last_two %in% state_abreviations) & (middle_chr == " "), without_last_two, city)) %>%
  left_join(cities, by = c("city")) %>%
  mutate(new_state = ifelse((last_two %in% state_abreviations) & (middle_chr == " "), last_two, 
                            ifelse(! is.na(state), as.character(state), NA)) ) %>%
  select(-city,-state, -last_two, -middle_chr, -without_last_two) %>% # remove old and temp variables
  select(date, city = new_city, state = new_state, everything()) # change new_city, new_state to city, state and reorder cols

# format city variable (replace spaces with underscores)
df$city <- str_replace(df$city, " ", "_")

# overwrite with city name altered and state included
write.csv(df, file = "data/email_data_reorganized.csv", row.names = FALSE)