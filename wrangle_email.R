#' This script transforms the email data into a proper R DataFrame
#' Created by Clayton Strauch on 14 September 2021

library(stringr)
library(lubridate)
library(purrr)

find_hi_lo <- function(temps){
  if (temps[1] != "MM" && temps[2] != "MM") {
    hi <- max(as.integer(temps[1]), as.integer(temps[2]))
    lo <- min(as.integer(temps[1]), as.integer(temps[2]))
  }
  else {
    hi <- NA
    lo <- NA
  }
  return(c(hi, lo))
}

check_negative <- function(temps) {
  if (str_detect(temps[1], "B")) {
    temps[1] <- paste("-", str_replace(temps[1], "B", ""), sep = "")
  }
  if (str_detect(temps[2], "B")) {
    temps[2] <- paste("-", str_replace(temps[2], "B", ""), sep = "")
  }
  return(temps)
}

extract_data_from_email <- function(email_file) {
  email <- readLines(email_file)
  
  # initiate empty vector and boolean variable
  v <-  c()
  b <- FALSE
  
  # initialize data frame with 11 columns
  df <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(df) = c("date_and_time", "city",
                   "previous_lo", "previous_hi", "previous_precip",
                   "today_lo", "today_hi", "today_outlook",
                   "tomorrow_lo", "tomorrow_hi", "tomorrow_outlook")
  
  # keep lines that are between "SELECTED CITIES" and
  # "$$" or "NATIONAL TEMPERATURE EXTREMES" then repeat for entire email
  for (line in email){
    if (grepl("SELECTED CITIES", line, ignore.case=FALSE, fixed=TRUE)){
      b <-  TRUE
    }
    
    if (b == TRUE && line != "") {
      # do not include lines that contain these values
      if ( !(grepl("TELECOMMUNICATION|PRECIPITATION|INDICATE|TEMPERATURE", line) ||
             grepl("$$", line, fixed=TRUE) ||
             grepl("NATIONAL TEMPERATURE EXTREMES", line, ignore.case = FALSE, fixed = TRUE))) {
        v <- append(v, line)
      }
    }
    
    if (grepl("$$", line, ignore.case=FALSE, fixed=TRUE) || 
        grepl("NATIONAL TEMPERATURE EXTREMES", line, ignore.case=FALSE, fixed=TRUE) ){
      b <-  FALSE
    }
  }
  
  # initialize date and time variable
  date_and_time <- ""

  # format information and then add it to data frame
  for (line in v) {
    if (str_detect(line, "\\.\\.|FORECAST|WEA|PCPN")) { }
    else if (str_detect(line, " EDT | EST ")) {
      split <- str_split_fixed(line, "EDT|EST", n = 2)
      time <- split[1]
      split_time <- str_split_fixed(time, " ", n = 2)
      if (str_length(split_time[1]) == 3) {
        temp <- paste(substring(split_time[1], 1, 1), ":", substring(split_time[1], 2, 3), sep = "")
      }
      if (str_length(split_time[1]) == 4) {
        temp <- paste(substring(split_time[1], 1, 2), ":", substring(split_time[1], 3, 4), sep = "")
      }
      time <- paste(temp, split_time[2])
      date_and_time <- paste(str_split_fixed(str_trim(split[2]), " ", n = 2)[2], time)
    }
    else {
      # split once on "MM" (missing value) or first temperature to retrieve city
      first_mm_or_num <- str_locate(line, "B[0-9]|MM|[0-9]")[1, 1]
      city <- str_trim(substr(line, 1, first_mm_or_num - 1))
      remaining <- substr(line, first_mm_or_num, nchar(line))
      
      # split twice on whitespace to retrieve previous hi and lo
      split <- str_split_fixed(remaining, "\\s+", n = 3)
      if (split[1] != "MM" && split[2] != "MM") {
        split <- check_negative(split)
        previous_hi <- find_hi_lo(split)[1]
        previous_lo <- find_hi_lo(split)[2]
      }
      else {
        previous_hi <- NA
        previous_lo <- NA
      }
      remaining <- split[3]
      
      # split once on whitespace to retrieve previous precipitation
      split <- str_split_fixed(remaining, "\\s+", n = 2)
      if (str_detect(split[1], "[0-9]")) {
        previous_precip <- split[1]
        remaining <- split[2]
      }
      else {
        previous_precip <- NA
        if (split[1] == "MM") { remaining <- split[2] }
        else { previous_precip <- 0 }
      }
      
      # split once on whitespace to retrieve today outlook
      # split once on whitespace to retrieve today hi and lo
      # split once on whitespace to retrieve tomorrow outlook
      # split once on whitespace to retrieve tomorrow hi and lo
      split <- str_split_fixed(remaining, "\\s+", 5)
      
      if (split[1] == "MISG") { today_outlook = NA }
      else { today_outlook <- split[1]}
      
      today_temps <- check_negative(str_split_fixed(split[2], "/", 2))
      today_hi <- find_hi_lo(today_temps)[1]
      today_lo <- find_hi_lo(today_temps)[2]
      
      if (split[3] == "MISG") { tomorrow_outlook = NA }
      else { tomorrow_outlook <- split[3]}
      
      tomorrow_temps <- check_negative(str_split_fixed(split[4], "/", 2))
      tomorrow_hi <- find_hi_lo(tomorrow_temps)[1]
      tomorrow_lo <- find_hi_lo(tomorrow_temps)[2]
      
      # add new row to data frame
      row <- c(date_and_time, city, previous_lo, previous_hi, previous_precip,
               today_lo, today_hi,  today_outlook,
               tomorrow_lo, tomorrow_hi, tomorrow_outlook)
      df[nrow(df) + 1, ] <- row
    }
  }
  
  # format data frame
  df$date_and_time <- parse_date_time(df$date_and_time, "%b %d %Y %H:%M %p")
  df$city <- as.factor(df$city)
  df$previous_lo <- as.integer(df$previous_lo)
  df$previous_hi <- as.integer(df$previous_hi)
  df$previous_precip <- as.numeric(df$previous_precip)
  df$today_lo <- as.integer(df$today_lo)
  df$today_hi <- as.integer(df$today_hi)
  df$today_outlook <- as.factor(df$today_outlook)
  df$tomorrow_lo <- as.integer(df$tomorrow_lo)
  df$tomorrow_hi <- as.integer(df$tomorrow_hi)
  df$tomorrow_outlook <- as.factor(df$tomorrow_outlook)
  
  # return data frame
  return(df)
}

files <- list.files(path = "data", pattern = "*.eml", full.names = TRUE, recursive = FALSE)
df <- map_df(files, extract_data_from_email)
write.csv(df, file = "email_data.csv")