library(rvest)
library(dplyr)
library(stringr)
a = read.csv("koppen.csv")
df = data.frame(matrix(nrow = 0, ncol = 9))
names(df) = c("City",
              "State",
              "Date",
              "D0_Hi",
              "D0_Lo",
              "D1_Hi",
              "D1_Lo",
              "D2_Hi",
              "D2_Lo")

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
  # s4 = session2 %>% html_nodes(".panel-title") %>% html_text2()
  # gsub("[\\(\\)]", "", regmatches(s3, gregexpr("\\(.*?\\)", s3))[[1]])
  # Sys.sleep(10)
  forecast <-
    html_node(session, css = "#seven-day-forecast-list") %>%
    html_text()
  
  df_temp = func(forecast)

  df = rbind(df, df_temp)

}

func = function(fcast) {
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
        "City" = a[i, 1],
        "State" = a[i, 2],
        "date" = date(),
        "D0_Hi" = str_extract(fcast[2], "[[:digit:]]+"),
        "D0_Lo" = str_extract(fcast[4], "[[:digit:]]+"),
        "D1_Hi" = str_extract(fcast[6], "[[:digit:]]+"),
        "D1_Lo" = str_extract(fcast[8], "[[:digit:]]+"),
        "D2_Hi" = str_extract(fcast[10], "[[:digit:]]+"),
        "D2_Lo" = str_extract(fcast[12], "[[:digit:]]+")
      )
    } else{
      df_temp = data.frame(
        "City" = a[i, 1],
        "State" = a[i, 2],
        "date" = date(),
        "D0_Hi" = NA,
        "D0_Lo" = str_extract(fcast[2], "[[:digit:]]+"),
        "D1_Hi" = str_extract(fcast[4], "[[:digit:]]+"),
        "D1_Lo" = str_extract(fcast[6], "[[:digit:]]+"),
        "D2_Hi" = str_extract(fcast[8], "[[:digit:]]+"),
        "D2_Lo" = str_extract(fcast[10], "[[:digit:]]+")
      )
    }
    
  },
  error = function(cond){
    message(paste("Could not fetch", a[i, 1], a[i, 2], sep = " "))
    message(paste(cond),"\n")
    message(paste("forecast:: ", fcast, sep = "---"))
    df_temp = data.frame(
      "City" = a[i, 1],
      "State" = a[i, 2],
      "date" = date(),
      "D0_Hi" = NA,
      "D0_Lo" = NA,
      "D1_Hi" = NA,
      "D1_Lo" = NA,
      "D2_Hi" = NA,
      "D2_Lo" = NA
    )
  })
  df_temp
}

write.table(df, "weather_data.csv" , append = TRUE, sep = "," , col.names = !file.exists("weather_data.csv"), row.names = FALSE)


