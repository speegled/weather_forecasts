#' This script transforms the email data into a proper R DataFrame
#' Created by Clayton Strauch on 14 September 2021

library(stringr)

# import data for testing
email = readLines("data/wx-natnl Digest Fri, 02 Apr 2021 (1_2).eml[1].eml")

extract_data_from_email <- function(email) {
  # initiate empty vector and boolean variable
  v <-  c()
  b <- FALSE
  
  # initialize data frame
  df <- data.frame(matrix(ncol = 12, nrow = 0))
  colnames(df) = c("date", "time", "city",
                   "previous_lo", "previous_hi", "previous_precip",
                   "today_lo", "today_hi", "today_outlook",
                   "tomorrow_lo", "tomorrow_hi", "tomorrow_outlook")
  
  # keep lines that are between "SELECTED CITIES" and "$$" then repeat for entire email
  for (line in email){
    if (grepl("SELECTED CITIES", line, ignore.case=FALSE, fixed=TRUE)){
      b <-  TRUE 
    }
    
    if (b == TRUE && line != "") {
      # do not include lines that contain these values
      if ( !(grepl("TELECOMMUNICATION|PRECIPITATION|INDICATE|TEMPERATURE", line) || grepl("$$", line, fixed=TRUE))){
        v <- append(v, line)
      }
    }
    
    if (grepl("$$", line, ignore.case=FALSE, fixed=TRUE)){
      b <-  FALSE
    }
  }
  
  # initialize time and date variables
  time <- ""
  date <- ""
  
  # format information and then add it to data frame
  for (line in v) {
    if (str_detect(line, "\\.\\.|FORECAST|CITY")) { }
    else if (str_detect(line, "EDT")) {
      split <- str_split_fixed(v[2], "EDT", n = 2)
      time <- split[1]
      date <- str_split_fixed(str_trim(split[2]), " ", n = 2)[2]
    }
    else {
      # split once on "MM" or first number to retrieve city
      first_mm_or_num <- str_locate(line, "[0-9]|MM")[1, 1]
      city <- str_trim(substr(line, 1, first_mm_or_num - 1))
      remaining <- substr(line, first_mm_or_num, nchar(line))
      
      # split twice on whitespace to retrieve previous hi and lo
      # doesn't consider case where only one of previous hi, previous lo is missing
      # doesn't consider below zero case
      split <- str_split_fixed(remaining, "\\s+", n = 3)
      if (split[1] != "MM" && split[2] != "MM") {
        previous_hi <- max(as.integer(split[1]), as.integer(split[2]))
        previous_lo <- min(as.integer(split[1]), as.integer(split[2]))
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
      # missing values not considered here
      # doesn't consider below zero case
      split <- str_split_fixed(remaining, "\\s+", 5)
      today_outlook <- split[1]
      today_temps <- str_split_fixed(split[2], "/", 2)
      today_hi <- max(as.integer(today_temps[1]), as.integer(today_temps[2]))
      today_lo <- min(as.integer(today_temps[1]), as.integer(today_temps[2]))
      tomorrow_outlook <- split[3]
      tomorrow_temps <- str_split_fixed(split[4], "/", 2)
      tomorrow_hi <- max(as.integer(tomorrow_temps[1]), as.integer(tomorrow_temps[2]))
      tomorrow_lo <- min(as.integer(tomorrow_temps[1]), as.integer(tomorrow_temps[2]))
      
      # add new row to data frame
      row <- c(date, time, city, previous_lo, previous_hi, previous_precip,
               today_lo, today_hi,  today_outlook,
               tomorrow_lo, tomorrow_hi, tomorrow_outlook)
      df[nrow(df) + 1, ] <- row
    }
  }
  
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

df <- extract_data_from_email(email)