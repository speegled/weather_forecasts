library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
a = read.csv("Scraper/Data/koppen.csv")
df = data.frame(matrix(nrow = 0, ncol = 10))
colNames = c(
  "date",
  "city",
  "state",
  "forecast_lo_2_prev_PM",
  "forecast_hi_2_prev_PM",
  "forecast_lo_prev_AM",
  "forecast_hi_prev_AM",
  "forecast_lo_prev_PM",
  "forecast_hi_prev_PM",
  "forecast_lo_current_AM",
  "forecast_hi_current_AM",
  "actual_lo_current_PM",
  "actual_hi_current_PM",
  "actual_lo_next_AM",
  "actual_hi_next_AM",
  "forecast_lo_next_AM",
  "forecast_hi_next_AM",
  "forecast_lo_next_PM",
  "forecast_hi_next_",
  "forecast_lo_2_next_AM",
  "forecast_hi_2_next_AM"
  
)
names(df) =  c(
  "date",
  "time",
  "city",
  "state",
  "forecast_lo_2_prev_PM",
  "forecast_hi_2_prev_PM",
  "forecast_lo_prev_AM",
  "forecast_hi_prev_AM" ,
  "forecast_lo_prev_PM" ,
  "forecast_hi_prev_PM" ,
  "forecast_lo_current_AM",
  "forecast_hi_current_AM",
  "forecast_lo_current_PM",
  "forecast_hi_curent_PM",
  "actual_lo_current_PM",
  "actual_hi_current_PM",
  "actual_lo_next_AM",
  "actual_hi_next_AM",
  "forecast_lo_next_AM",
  "forecast_hi_next_AM",
  "forecast_lo_next_PM",
  "forecast_hi_next_PM",
  "forecast_lo_2_next_AM",
  "forecast_hi_2_next_AM",
  "forecast_lo_2_next_PM",
  "forecast_hi_2_next_PM",
)
func = function(fcast) {
  dttm = parse_date_time(as.character(date()),
                         "%a %b %d %H:%M:%S %Y")
  dt = date(dttm)
  tm = paste(hour(dttm), minute(dttm), second(dttm), sep = ":")
  df_temp = tryCatch({
    fcast = strsplit(fcast, "\n")[[1]]
    fcast = str_replace_all(fcast, pattern = "[\r\n]", replacement = " ")
    fcast = fcast[fcast != ""]
    fcast = fcast[fcast != " "]
    
    
    while (fcast[1] != "Today" & fcast[1] !=  "Tonight") {
      fcast = fcast[-1]
    }
    if (fcast[1] == "Today")
    {
      df_temp = data.frame(
        "date" = dt,
        "time" = tm,
        "city" = a[i, 1],
        "state" = a[i, 2],
        "forecast_lo_2_prev_PM" = NA,
        "forecast_hi_2_prev_PM" = NA,
        "forecast_lo_prev_AM" = NA,
        "forecast_hi_prev_AM" = NA,
        "forecast_lo_prev_PM" = NA,
        "forecast_hi_prev_PM" = NA,
        "forecast_lo_current_AM" = NA,
        "forecast_hi_current_AM" = str_extract(fcast[2], "[[:digit:]]+"),
        #today high ^
        "forecast_lo_current_PM" = str_extract(fcast[4], "[[:digit:]]+"),
        #today lo
        "forecast_hi_curent_PM" = NA,
        "actual_lo_current_PM" = NA,
        "actual_hi_current_PM" = NA,
        "actual_lo_next_AM" = NA,
        "actual_hi_next_AM" = NA,
        "forecast_lo_next_AM" = NA,
        "forecast_hi_next_AM" = str_extract(fcast[6], "[[:digit:]]+"),
        #tom hi,
        "forecast_lo_next_PM" = str_extract(fcast[8], "[[:digit:]]+"),
        # tom lo
        "forecast_hi_next_PM" = NA,
        "forecast_lo_2_next_AM" = NA,
        "forecast_hi_2_next_AM" = str_extract(fcast[10], "[[:digit:]]+"),
        # day aft hi
        "forecast_lo_2_next_PM" = str_extract(fcast[12], "[[:digit:]]+"),
        # day aft lo
        "forecast_hi_2_next_PM" = NA
      )
    } else{
      df_temp = data.frame(
        "date" = dt,
        "time" = tm,
        "city" = a[i, 1],
        "state" = a[i, 2],
        "forecast_lo_2_prev_PM" = NA,
        "forecast_hi_2_prev_PM" = NA,
        "forecast_lo_prev_AM" = NA,
        "forecast_hi_prev_AM" = NA,
        "forecast_lo_prev_PM" = NA,
        "forecast_hi_prev_PM" = NA,
        "forecast_lo_current_AM" = NA,
        "forecast_hi_current_AM" = NA,
        #today high
        "forecast_lo_current_PM" = str_extract(fcast[2], "[[:digit:]]+"),
        #today lo
        "forecast_hi_curent_PM" = NA,
        "actual_lo_current_PM" = NA,
        "actual_hi_current_PM" = NA,
        "actual_lo_next_AM" = NA,
        "actual_hi_next_AM" = NA,
        "forecast_lo_next_AM" = NA,
        "forecast_hi_next_AM" = str_extract(fcast[4], "[[:digit:]]+"),
        #tom hi,
        "forecast_lo_next_PM" = str_extract(fcast[6], "[[:digit:]]+"),
        # tom lo
        "forecast_hi_next_PM" = NA,
        "forecast_lo_2_next_AM" = NA,
        "forecast_hi_2_next_AM" = str_extract(fcast[8], "[[:digit:]]+"),
        # day aft hi
        "forecast_lo_2_next_PM" = str_extract(fcast[10], "[[:digit:]]+"),
        # day aft lo
        "forecast_hi_2_next_PM" = NA
      )
    
    }
    
  },
  error = function(cond) {
    message(paste("Could not fetch", a[i, 1], a[i, 2], sep = " "))
    message(paste(cond), "\n")
    message(paste("forecast:: ", fcast, sep = "---"))
    df_temp = data.frame(
      "date" = dt,
      "time" = tm,
      "city" = a[i, 1],
      "state" = a[i, 2],
      "forecast_lo_2_prev_PM" = NA,
      "forecast_hi_2_prev_PM" = NA,
      "forecast_lo_prev_AM" = NA,
      "forecast_hi_prev_AM" = NA,
      "forecast_lo_prev_PM" = NA,
      "forecast_hi_prev_PM" = NA,
      "forecast_lo_current_AM" = NA,
      "forecast_hi_current_AM" = NA,  #today high
      "forecast_lo_current_PM" = NA,      #today lo
      "forecast_hi_curent_PM" = NA,
      "actual_lo_current_PM" = NA,
      "actual_hi_current_PM" = NA,
      "actual_lo_next_AM" = NA,
      "actual_hi_next_AM" = NA,
      "forecast_lo_next_AM" = NA,
      "forecast_hi_next_AM" = NA,      #tom hi,
      "forecast_lo_next_PM" = NA,      # tom lo
      "forecast_hi_next_PM" = NA,
      "forecast_lo_2_next_AM" = NA,
      "forecast_hi_2_next_AM" = NA,      # day aft hi
      "forecast_lo_2_next_PM" = NA,      # day aft lo
      "forecast_hi_2_next_PM" = NA
    )
  })
df_temp
}

for (i in 1:128) {
  url <- 'https://www.weather.gov/'
  weather <- html_session(url)
  # Sys.sleep(10)
  pgform <- html_form(weather)
  # Sys.sleep(10)
  form.filled <- pgform
  # Sys.sleep(10)
  form.filled[[2]] <-
    pgform[[2]] %>% set_values("inputstring" = paste(a[i, 1], a[i, 2], sep = ", "))
  # Sys.sleep(10)
  session <- submit_form(session = weather, form = form.filled[[2]])
  session
  
  session2 = read_html(session)
  s2 = session2 %>% html_elements("h1") %>% html_text()
  s2 = s2[1]
  if (s2 == "More than one location matched your submission") {
    s3 = read_html(session)
    session = follow_link(session,
                          s3 %>% html_element("table") %>% html_element("a") %>% html_text())
  }
  forecast <-
    html_node(session, css = "#seven-day-forecast-list") %>%
    html_text()
  
  df_temp = func(forecast)
  
  df = rbind(df, df_temp)
  
}



write.table(
  df,
  "Scraper/data/weather_data.csv" ,
  append = TRUE,
  sep = "," ,
  col.names = !file.exists("Scraper/data/weather_data.csv"),
  row.names = FALSE
)
