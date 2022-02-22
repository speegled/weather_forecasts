source("visualization_and_analysis_scripts/analyze_errors.R") # loads all functions from analyze errors
# this code is not meaningful. Mostly code to analyze the data
errDf = create_error_df()
errDf <-
  errDf %>%
  filter(error_lo_2_prev_PM < 25,
         error_lo_2_prev_PM > -25,
         city == "CHICAGO") %>%
  select(date, error_hi_prev_AM)

plot(errDf$date, errDf$error_hi_prev_AM)

plot(y)
plot(decompose(y))
forecast::findfrequency(y)

y = ts(errDf$error_lo_2_prev_PM)
(y)

data <- read.csv("data/email_data_reorganized.csv")
data <- data %>%  filter(city == "CHICAGO")

plot(forecast_lo_2_prev_PM ~ date, data = data)
y2 = data$forecast_lo_2_prev_PM[!is.na(data$forecast_lo_2_prev_PM)]
acf(ts(y2))
aics = c()
for (i in seq(1, 20, by = 3)) {
  for (j in  seq(1, 20, by = 3)) {
    AR <- arima(y, order = c(i, 0, j))
    ts.plot(y)
    AR_fit <- y - residuals(AR)
    points(AR_fit,
           type = "l",
           col = 2,
           lty = 2)
    aics = c(aics, AIC(AR))
    print(paste(AIC(AR), i, j))
  }
}








acf(ts(errDf[, 2]))

AR <- arima(y, order = c(9, 0, 9))
ts.plot(y)
AR_fit <- y - residuals(AR)
points(AR_fit,
       type = "l",
       col = 2,
       lty = 2)
aics = c(aics, AIC(AR))
print(paste(AIC(AR), 20, 20))

yy = diff(y)
plot(errDf$date, y)

plot(errDf$date[-1], yy)



# chicago
errDf = create_error_df()
errDf <-
  errDf %>%
  filter(city == "CHICAGO",!is.na(error_hi_prev_AM)) %>%
  select(date, error_hi_prev_AM)

# errDf[,1] = 1:length(err)

# y = errDf[1:200,2]
# x = errDf[1:200,1]

y = errDf[, 2]
x = errDf[, 1]

plot(y ~ x)
AIC(arima(y, order = c(1, 0, 0)))
AIC(arima(y, order = c(2, 0, 0)))
AIC(arima(y, order = c(4, 0, 0)))

acf(y)

mod <- lm(y ~ x)

mod_gls <- nlme::gls(y ~ x, correlation = nlme::corARMA(p = 2))
summary(mod_gls)

plot(y ~ x, type = "l")
points(errDf[201:260, 1],
       predict(mod_gls,
               newdata = data.frame(y = errDf[201:260, 2], x = errDf[201:260, 1])),
       col = 2)
points(errDf[201:260, 1],
       errDf[201:260, 2],
       col = 3)

plot(errDf[201:260, 1],
     predict(mod_gls,
             newdata = data.frame(y = errDf[201:260, 2], x = errDf[201:260, 1])),
     col = 2)
#----------------------
#Checking if scrpaer data and email data are same

library(lubridate)
df <- read.csv("data/weather_data.csv")
df$Time <- lubridate::hms(df$Time)
df$Date <- lubridate::date(df$Date)

df <- df %>% filter(City == "KANSAS CITY",
                    hour(Time) < 12) %>%
  select(D1_Lo, Date)


data <- read.csv("data/email_data.csv")
data$date = lubridate::date(data$date_and_time)
data <- data %>%  filter(city == "KANSAS CITY",
                         lubridate::hour(data$date_and_time) < 12) %>%
  select(tomorrow_lo, date)
head(df)
head(data)
common <- intersect(df$Date, data$date)
df[common, ] # give you common rows in data frame 1
data[common, ] # give you common rows in data frame 2

df2 = inner_join(df, data, by = c("Date" = "date"))

colors <-
  c("Email" = "blue",
    "Scraper" = "red")
ggplot(df2) +
  geom_line(aes(x = Date, y = D1_Lo, color = "Email"), alpha = .8) +
  geom_line(aes(x = Date, y = tomorrow_lo, color = "Scraper"), alpha = .8) +
  scale_color_manual(values = colors) +
  labs(y = "Temp (F)", x = "Date", colour = "Source") + theme(
    panel.grid.major = element_blank(),
    text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.background  = element_blank(),
    panel.border = element_rect(fill = NA),
    panel.spacing = unit(0, "mm"),
    legend.key = element_rect(fill = NA),
    legend.position = c(0.01, 0.01),
    legend.justification = c("left", "bottom")
  )


scale_color_manual(
  name = "source",
  labels = c("Email", "Scraper"),
  values = c("blue", "red")
)

# ---------------------------

library(tidyverse)
library(lubridate)
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
mid_pt = mean(c(min(cities$LAT), max(cities$LAT)))

func <- function(city_inp) {
  errDf = create_error_df()
  errDf <-
    errDf %>%
    filter(city == city_inp,!is.na(error_hi_2_prev_PM)) %>%
    select(date, error_hi_prev_AM)
  
  
  # acf(errDf$error_hi_prev_AM)
  
  y = errDf[, 2]
  x = errDf[, 1]
  
  # plot(y ~ x)
  
  # acf(y)
  
  # mod <- lm(y ~ x)
  
  dates = seq.Date(date("2021-01-31"), date("2021-11-01"), by = 7)
  
  wt <- data.frame(matrix(ncol = 4, nrow = 0))
  names(wt) = c("start", "avgTemp","city", "east_west")
  wt
  
  for (i in 1:(length(dates) - 1)) {
    t = abs(errDf[errDf$date >= dates[i] & errDf$date < dates[i + 1], 2])
    t = mean(t, na.rm = T)
    city_inp_lon = cities[as.character(cities$CITY) == as.character(city_inp),]$LAT
    city_inp_lon
    if(length(city_inp_lon) > 0 && city_inp_lon>(mid_pt +3)){
      coast = "South"
    }else if(length(city_inp_lon) > 0 && city_inp_lon<(mid_pt-3)){
      coast = "North"
    }else{
      coast = "other"
    }
    wt = rbind(wt, data.frame(start = dates[i], avgTemp = t, city = city_inp, east_west = coast))
    
  }
  wt$avgTemp <- normalize(wt$avgTemp)
  wt
}

cities <- read.csv("data/cities.csv")

# df = func("CHICAGO")
# df = rbind(df, func("KANSAS CITY"))
unq_cities = unique(errDf$city)
df = func("ABILENE")
for(i in 2:length(unq_cities)){
  df = rbind(df, func(unq_cities[i]))
}
df <- df %>% filter(east_west!= "other")
ggplot()+
  # geom_point(data = df, aes(x = start, y = avgTemp, color = city))+
  geom_line(stat = "smooth", data = df, aes(x = start, y = avgTemp, group = city, color = east_west), alpha = .4)+
  theme(        panel.grid.major = element_blank(),
        text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.background  = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.spacing = unit(0, "mm"))+
    
  coord_cartesian(ylim = c(0,1),expand = F)

ggplot()+
  # geom_point(data = df, aes(x = start, y = avgTemp, color = city))+
  geom_smooth( data = df, aes(x = start, y = avgTemp, group = east_west, color = east_west), alpha = .4, se = T)+
  theme(        panel.grid.major = element_blank(),
                text = element_text(size = 12),
                panel.grid = element_blank(),
                panel.background  = element_blank(),
                panel.border = element_rect(fill = NA),
                panel.spacing = unit(0, "mm"))+
  
  coord_cartesian(expand = F)
  
data.frame(df %>% group_by(city,east_west) %>% summarise())

ggplot()+
  geom_point(data = df, aes(x = start, y = avgTemp, color = "red"))+
  geom_smooth(data = df, aes(x = start, y = avgTemp, color = "red"))+
  
  geom_point(data = df2, aes(x = start, y = avgTemp, color = "blue"))+
  geom_smooth(data = df2, aes(x = start, y = avgTemp, color = "blue"))
  # geom_point(aes(x = df2$start[13], y=1))
  





min(cities$LON)
max(cities$LON)

demo = 1
city_inp_lon = 2
if(length(city_inp_lon>0)){
  demo = 2
}
demo






a = rnorm(100)
b = rnorm(100,5)


a = df %>% filter(month(start) == 4, day(start) == 4, east_west == "North") %>% select(avgTemp)
b = df %>% filter(month(start) == 4, day(start) == 4, east_west == "South")  %>% select(avgTemp)
var.test(unlist(a), unlist(b))

a = df %>% filter(month(start) == 9, day(start) == 26, east_west == "North") %>% select(avgTemp)
b = df %>% filter(month(start) == 9, day(start) == 26, east_west == "South")  %>% select(avgTemp)
var.test(unlist(a), unlist(b))

