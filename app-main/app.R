##' This Shiny App will generate (interactive) maps of various types
##' Initially created 26 October 2021 by Clayton Strauch

library(shiny)
library(tidyverse)
library(ggplot2)
library(maps)
library(lubridate)
library(usmap)
library(ggthemes)
library(tibble)
library(viridis)
library(rgeos)
library(mapproj)
library(reshape2)
library(akima)

ui <- fluidPage(
  titlePanel(" Weather Project"),
  p(), 
  sidebarLayout(
    sidebarPanel(
      p("Weather is hard to predict. Why? Where? *write later*"),
    width = 3),
    mainPanel( tabsetPanel(
      tabPanel("Mean Temp. Error.",
          p(),
          sidebarLayout(
            sidebarPanel(
              selectInput(inputId = "mean_error_hi_or_lo", label = "High or Low Temperature", 
                choices = c("High", "Low", "Both"), multiple = FALSE),
              sliderInput(inputId = "mean_error_num_bins", label = "Number of Bins",
                          min = 5, max=30, value = 20, round = TRUE),
              checkboxInput(inputId = "mean_error_abs", label = strong("Absolute Value?"), value=FALSE),
            ),
            mainPanel(
              plotOutput(outputId = "mean_errors")
            )
          )
      ),
      tabPanel("Cities Map",
               p(),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "bool_state_borders", label = "Include State Borders?", 
                               choices = c("No", "Yes"), multiple = FALSE)
                 ),
                 mainPanel(
                   plotOutput(outputId = "main_map")
                 )
               )
      ),
      tabPanel("Mean Error Map Approximation",
               p(),
               sidebarLayout(
                 sidebarPanel(
                   radioButtons(inputId = "mean_error_map_hi_or_lo", label = "High or Low Temperature", 
                               choices = c("High"="hi", "Low"="lo")),
                   radioButtons(inputId = "mean_error_map_forecast_time", label = "Time of Forecast", 
                                choices = c("Mean of all Forecasts"="",
                                            "PM 2 Days Previous"="_2_prev_PM",
                                            "AM 1 Day Previous"="_prev_AM",
                                            "PM 1 Day Previous"="_prev_PM",
                                            "AM Current Day"="_current_AM")),
                   sliderInput(inputId = "mean_error_map_dot_size", label = "Point Size",
                               min = 0, max=30, value = 15, round = TRUE),
                   sliderInput(inputId = "mean_error_map_dot_alpha", label = "Point Transparency",
                               min = 0, max=1, value = 0.5, round = TRUE),
                   checkboxInput(inputId = "mean_error_map_abs", label = strong("Absolute Value?"), value=FALSE),
                   checkboxInput(inputId = "mean_error_map_show_sig_cities", label = strong("Show Significant City Labels?"), value=FALSE),
                   sliderInput(inputId = "mean_error_map_show_sig_cities_font_size", label = "City Label Size",
                               min = 0.5, max=6.5, value = 2, round = FALSE)
                 ),
                 mainPanel(
                   plotOutput(outputId = "mean_error_map")
                 )
               )),
      tabPanel("Continuous Map",
               p(),
               sidebarLayout(
                 sidebarPanel(
                   radioButtons(inputId = "cont_map_variable", label = "Variable to Plot", 
                                choices = c("Current Day Low Temperature"="D0_Lo",
                                            "Current Day High Temperature"="D0_Lo",
                                            "1 Day Previous Low Temperature"="D1_Lo",
                                            "1 Day Previous High Temperature"="D1_Hi"))
                 ),
                 mainPanel(
                   plotOutput(outputId = "cont_map")
                 )
               )
               )
      
    ))
      
    )
)

server <- function(input, output) {
  #data used in multiple shiny plots/objects/etc.
  cities <- read.csv("../data/cities.csv")
  data <- read.csv("../data/email_data_reorganized.csv")
  data$date <- parse_date_time(data$date, "%Y-%m-%d")
  mean_errors <- data %>%
    mutate(error_lo_2_prev_PM = actual_lo_next_AM - forecast_lo_2_prev_PM,
           error_hi_2_prev_PM = actual_hi_next_AM - forecast_hi_2_prev_PM,
           error_lo_prev_AM = actual_lo_next_AM - forecast_lo_prev_AM,
           error_hi_prev_AM = actual_hi_next_AM - forecast_hi_prev_AM,
           error_lo_prev_PM = actual_lo_next_AM - forecast_lo_prev_PM,
           error_hi_prev_PM = actual_hi_next_AM - forecast_hi_prev_PM,
           error_lo_current_AM = actual_lo_next_AM - forecast_lo_current_AM,
           error_hi_current_AM = actual_hi_next_AM - forecast_hi_current_AM) %>%
    select(date, city,
           error_lo_2_prev_PM, error_hi_2_prev_PM,
           error_lo_prev_AM, error_hi_prev_AM,
           error_lo_prev_PM, error_hi_prev_PM,
           error_lo_current_AM, error_hi_current_AM) %>%
    group_by(city) %>%
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
  
  #start making objects in the app
  output$main_map <- renderPlot({
    if( input$bool_state_borders == "No"){
      ggplot() +
        borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue') +
        geom_point(data = cities, mapping = aes(x=LON, y=LAT), color = 'black')
    }
    else{
      ggplot() +
      borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue') +
      borders('state') +
      geom_point(data = cities, mapping = aes(x=LON, y=LAT), color = 'black')
    }
  }, width=850, height=600)
  output$mean_errors <- renderPlot({
    #Histograms of Mean High Error and Mean Low Error
    if(input$mean_error_hi_or_lo == "High"){
      if(input$mean_error_abs==FALSE){
        hist(mean_errors$mean_error_hi, main="Mean Error of Predicted and Observed High Temperatures", breaks = input$mean_error_num_bins, xlab="Temperature Error", col="indianred1")
      }
      else{
        hist(abs(mean_errors$mean_error_hi), main="Mean Error of Predicted and Observed High Temperatures", breaks = input$mean_error_num_bins, xlab="Temperature Error", col="indianred1")
      }
    }
    else if(input$mean_error_hi_or_lo == "Low"){
      if(input$mean_error_abs==FALSE){
        hist(mean_errors$mean_error_lo, main="Mean Error of Predicted and Observed Low Temperatures",  breaks = input$mean_error_num_bins, xlab="Temperature Error", col="slateblue1")
      }
      else{
        hist(abs(mean_errors$mean_error_lo), main="Mean Error of Predicted and Observed Low Temperatures",  breaks = input$mean_error_num_bins, xlab="Temperature Error", col="slateblue1")
      }
    }
    #Layered ggplot histogram for both low and high errors
    else{
      #construct additional components
      c(quantile(mean_errors$mean_error_lo, 0.025), quantile(mean_errors$mean_error_lo, 0.975))
      c(quantile(mean_errors$mean_error_hi, 0.025), quantile(mean_errors$mean_error_hi, 0.975))
      num_cities <- length(mean_errors$city)
      plot_hi_lo <- data.frame(mean_error = c(mean_errors$mean_error_lo, mean_errors$mean_error_hi),
                               hi_or_lo = c(rep("lo", num_cities), rep("hi", num_cities)))
      if( input$mean_error_abs == TRUE){
        plot_hi_lo$mean_error <- abs(plot_hi_lo$mean_error)
      }
      plot_means <- plot_hi_lo %>%
        group_by(hi_or_lo) %>%
        summarize(mean = mean(mean_error))
      
      ggplot(plot_hi_lo, aes(x = mean_error, fill = hi_or_lo, col = hi_or_lo)) + 
        geom_histogram(bins = input$mean_error_num_bins, position = "identity", alpha = 0.5) + 
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
    }
    
  }, width = 750, height=550)
  output$mean_error_map <- renderPlot({
    mean_error_map_hi_lo_var <- input$mean_error_map_hi_or_lo
    if (input$mean_error_map_hi_or_lo=="hi"){
      mean_error_map_low_color = "orange"
    } else{
      mean_error_map_low_color = "green"
    }
    cities <- filter(cities, CITY != "DOVER") %>%
      select(city=CITY, lat=LAT, lon=LON, climate=CLIMATE)
    not_cont <- c("ANCHORAGE", "HONOLULU", "SAN JUAN PR", "JUNEAU", "ST THOMAS VI")
    mean_errors <- mean_errors %>%
      inner_join(cities, by="city") %>%
      filter(!(city %in% not_cont)) %>%
      mutate(data_to_plot = (!!as.symbol(paste("mean_error_", input$mean_error_map_hi_or_lo, input$mean_error_map_forecast_time, sep="")))) %>%
      mutate(data_to_plot = case_when(
        input$mean_error_map_abs == TRUE ~ abs(data_to_plot), 
        input$mean_error_map_abs == FALSE ~ data_to_plot))
      
    MainStates <- map_data("state") 
    
    #mean_error_map_col_interest = paste("mean_error_", input$mean_error_map_hi_or_lo, input$mean_error_map_forecast_time, sep="")
    #Whether to show sig city labels or not
    if (input$mean_error_map_show_sig_cities == FALSE){
      ggplot() + 
        geom_polygon(data = MainStates, aes(x = long, y = lat, group = group), color = "black", fill = "white") +
        geom_point(data = mean_errors,
                   aes(x = lon, y = lat, col = data_to_plot), size = input$mean_error_map_dot_size, alpha = input$mean_error_map_dot_alpha) +
        scale_color_gradient(low = mean_error_map_low_color, high = "blue", na.value = NA, name = "Mean Error") +
        labs(title = paste("Mean Errors for", input$mean_error_map_hi_or_lo,"Temperature Forecasts"), subtitle = input$mean_error_map_forecast_time) +
        theme_classic()
    } else {
      ggplot() + 
        geom_polygon(data = MainStates, aes(x = long, y = lat, group = group), color = "black", fill = "white") +
        geom_point(data = mean_errors,
                   aes(x = lon, y = lat, col = data_to_plot), size = input$mean_error_map_dot_size, alpha = input$mean_error_map_dot_alpha) +
        scale_color_gradient(low = mean_error_map_low_color, high = "blue", na.value = NA, name = "Mean Error") +
        geom_label(data = mean_errors %>% filter(abs(data_to_plot) > 1.7),
                   aes(x = lon, y = lat, label = city), alpha = 0.5, size = input$mean_error_map_show_sig_cities_font_size) +
        labs(title = paste("Mean Errors for", input$mean_error_map_hi_or_lo,"Temperature Forecasts"), subtitle = input$mean_error_map_forecast_time) +
        theme_classic()
    }
     
    
    
    
    
  }, width = 850, height=600)
  output$cont_map <- renderPlot({
    
    state <- map_data("state")
    county <- map_data("county")
    usa <- map_data("usa")
    
    # Ensure data is in same format. 
    # Change any temp cols (ex: D0_Hi) to whatever you need
    # can add NA for rest cols
    df = read.csv("../data/vis_data/vis_dat.csv")
    
    #filter out alaska and hawaii
    df = df %>% 
      filter(Lon > -150 & Lon < -50 & Lat > 20 & Lat< 50)
    df = df[!is.na(df$D2_Hi),]
    df = df[!is.na(df$Lat),]
    
    df2 = data.frame(City = "MAPLE FALLS", State = "WA", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 49, Lon = -121.929)
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 49, Lon = -95.34))
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 48.380147429239486, Lon = -124.9))
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 40.438564089182776, Lon = -124.40398315271037))
    
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 40.43262875150735, Lon = -124.33235376063053))
    #40.43262875150735, -124.33235376063053
    # df = rbind(df,c)
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 25.847255760595104, Lon = -97.38835720353877))
    #25.847255760595104, -97.38835720353877
    #47.42954034281501, -69.17932665630178
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 47.42954034281501, Lon = -69.17932665630178))
    #44.91469429949023, -67.00336321956611
    df2 = rbind(df2,  data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 44.91469429949023, Lon = -67.00336321956611))
    #45.30851220725746, -69.41196906976818
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 45.30851220725746, Lon = -69.41196906976818))
    #24.545979687846238, -81.8117847934193
    df2 = rbind(df2,  data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 24.545979687846238, Lon = -81.8117847934193))
    #34.574393505327905, -120.6099823317243
    df2 = rbind(df2,  data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 34.574393505327905, Lon = -120.6099823317243))
    #25.19597229396031, -80.3543262332071
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 25.19597229396031, Lon = -80.3543262332071))
    #49.69460365530822, -96.24450216840253
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 49.69460365530822, Lon = -96.24450216840253))
    #38.89722951749534, -123.81178034374763
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 38.89722951749534, Lon = -123.81178034374763))
    
    #35.63378635217967, -121.44675908586248
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 35.63378635217967, Lon = -121.44675908586248))
    #44.957550713263544, -66.33976766540263
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 44.957550713263544, Lon = -66.33976766540263))
    #47.5129437283398, -68.66783546613333
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 47.5129437283398, Lon = -68.66783546613333))
    #46.502993009536745, -67.38148086183992
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 47.5129437283398, Lon = -68.66783546613333))
    #47.09141122442806, -67.64492994268902
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 47.09141122442806, Lon = -67.64492994268902))
    #47.463275411222156, -67.95813653088872
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 47.463275411222156, Lon = -67.95813653088872))
    #25.53092315659166, -97.51700722865732
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 25.53092315659166, Lon = -97.51700722865732))
    #25.035318275866842, -79.233573332187
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 25.035318275866842, Lon = -79.233573332187))
    #36.380984263414966, -123.31739932533833
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 36.380984263414966, Lon = -123.31739932533833))
    #32.639983443889776, -117.79713320831848
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 32.639983443889776, Lon = -117.79713320831848))
    #38.04129981092496, -123.73937356991624
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 38.04129981092496, Lon = -123.73937356991624))
    #49.134734581490314, -125.36061954104694
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 49.134734581490314, Lon = -125.36061954104694))  
    #48.95644252119423, -119.20509596072583
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 48.95644252119423, Lon = -119.20509596072583))
    #48.9498268317576, -114.1678425489832
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 48.9498268317576, Lon = -114.1678425489832))
    #47.48662012821261, -115.46745400082271
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 47.48662012821261, Lon = -115.46745400082271))
    #48.84798789593003, -104.04055482861055
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 48.84798789593003, Lon = -104.04055482861055))
    #46.04231581626713, -104.19715811300537
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 46.04231581626713, Lon = -104.19715811300537))
    #44.05100687578249, -107.7990336540859
    df2 = rbind(df2, data.frame(City = "WILLIAMS", State = "MN", date = date(), D0_Hi = 49, D0_Lo = 30, D1_Hi = 35, D1_Lo = 25, D2_Hi = 38, D2_Lo = 30, Lat = 44.05100687578249, Lon = -107.7990336540859))
    
    # get closest city for each city in df2. df1 is the cities scraped
    func <- function(df1, df2){
      p1 = SpatialPoints(df1[,c("Lat", "Lon")])
      p2 = SpatialPoints(df2[,c("Lat", "Lon")])
      min_ind = apply(gDistance(p1, p2, byid=TRUE), 1, which.min)
      df2[,4:9] = df1[min_ind, 4:9]
      df2
    }
    
    
    for( i in 1:nrow(df2)){
      df = rbind(df, func(df, data.frame(df2[i,])))
    }
    
    #' this part removes excess gradient over water not too important for changing with other data
    #bottom half
    
    usa <- map_data("usa")
    
    pol1 = Polygon(usa[,c("long","lat")])
    p1 <- SpatialPoints(usa[,c("long","lat")])
    
    a = c(38, -70.58713866470633)
    b = c(38, -124.73458425264569)
    c = c(24.5, -124.73458425264569)
    d = c(24.5, -72.58713866470633)
    
    temp = data.frame(x = c(a[2],b[2],c[2],d[2]), y = c(a[1],b[1],c[1],d[1]))
    temp = rbind(temp,temp[1,])
    pol2 = Polygon(temp[,c("x","y")])
    p2 <- SpatialPoints(temp[,c("x","y")])
    
    p1 <- SpatialPolygons(list(Polygons(list(pol1), "p1")))
    p2 <- SpatialPolygons(list(Polygons(list(pol2), "p2")))
    res <- gDifference(  gBuffer(p2, byid=TRUE, width=0),gBuffer(p1, byid=TRUE, width=0))
    a=res@polygons
    dat = as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)
    dat2 = as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)

    usa <- map_data("usa")
    pol1 = Polygon(usa[,c("long","lat")])
    p1 <- SpatialPoints(usa[,c("long","lat")])
    
    a = c(49.48709157805013, -126.0292411544105)
    b = c(49.372767456697055, -97.20111573305981)
    c = c(35.01791506999891, -96.9813892040004)
    d = c(34.36753597096922, -124.97455433158129)
    temp = data.frame(x = c(a[2],b[2],c[2],d[2]), y = c(a[1],b[1],c[1],d[1]))
    temp = rbind(temp,temp[1,])
    pol2 = Polygon(temp[,c("x","y")])
    p2 <- SpatialPoints(temp[,c("x","y")])
    
    p1 <- SpatialPolygons(list(Polygons(list(pol1), "p1")))
    p2 <- SpatialPolygons(list(Polygons(list(pol2), "p2")))
    res <- gDifference(  gBuffer(p2, byid=TRUE, width=0),gBuffer(p1, byid=TRUE, width=0))
    a=res@polygons
    
    dat2 = as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)

    dat3 = as.data.frame(a[1][[1]]@Polygons[2][[1]]@coords)

    usa2 = usa[usa$order>4950 & usa$order<5050,]
    
    # lake michigan
    usa <- map_data("usa")
    
    pol1 = Polygon(usa2[,c("long","lat")])
    p1 <- SpatialPoints(usa2[,c("long","lat")])
    
    a = c(44.19517160437166, -88.03181425992327)
    b = c(44.17748599794071, -84.77623582388959)
    c = c(41.056774105291225, -84.97354360789164)
    d = c(41.1311228986116, -89.28965138293627)
    temp = data.frame(x = c(a[2],b[2],c[2],d[2]), y = c(a[1],b[1],c[1],d[1]))
    temp = rbind(temp,temp[1,])
    pol2 = Polygon(temp[,c("x","y")])
    p2 <- SpatialPoints(temp[,c("x","y")])
    
    p1 <- SpatialPolygons(list(Polygons(list(pol1), "p1")))
    p2 <- SpatialPolygons(list(Polygons(list(pol2), "p2")))
    res <- gIntersection(  gBuffer(p1, byid=TRUE, width=0),gBuffer(p2, byid=TRUE, width=0))
    # res <- gDifference(  gBuffer(p2, byid=T, width=0),gBuffer(p1, byid=T, width=0))
    a=res@polygons
    
    dat4 = as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)

    usa3 = usa[usa$order>4400 & usa$order<4750,]
    
    # lake erie
    pol1 = Polygon(usa3[,c("long","lat")])
    p1 <- SpatialPoints(usa3[,c("long","lat")])
    
    a = c(43.528281172791125, -81.87143003643087)
    b = c(43.723199406399075, -78.89256679347217)
    c = c(41.411998034650495, -78.24462197355422)
    d = c(41.39703243933011, -84.08310879021084)
    temp = data.frame(x = c(a[2],b[2],c[2],d[2]), y = c(a[1],b[1],c[1],d[1]))
    temp = rbind(temp,temp[1,])
    pol2 = Polygon(temp[,c("x","y")])
    p2 <- SpatialPoints(temp[,c("x","y")])
    
    p1 <- SpatialPolygons(list(Polygons(list(pol1), "p1")))
    p2 <- SpatialPolygons(list(Polygons(list(pol2), "p2")))
    res <- gIntersection(  gBuffer(p1, byid=TRUE, width=0),gBuffer(p2, byid=TRUE, width=0))
    # res <- gDifference(  gBuffer(p2, byid=T, width=0),gBuffer(p1, byid=T, width=0))
    a=res@polygons
    
    dat9 = as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)
    # 41.63111,-87.34742	
    
    func2 <- function(a,b,c,d){
      usa <- map_data("usa")
      
      pol1 = Polygon(usa[,c("long","lat")])
      p1 <- SpatialPoints(usa[,c("long","lat")])
      
      temp = data.frame(x = c(a[2],b[2],c[2],d[2]), y = c(a[1],b[1],c[1],d[1]))
      temp = rbind(temp,temp[1,])
      pol2 = Polygon(temp[,c("x","y")])
      p2 <- SpatialPoints(temp[,c("x","y")])
      
      p1 <- SpatialPolygons(list(Polygons(list(pol1), "p1")))
      p2 <- SpatialPolygons(list(Polygons(list(pol2), "p2")))
      res <- gDifference(  gBuffer(p2, byid=TRUE, width=0),gBuffer(p1, byid=TRUE, width=0))
      # res <- gDifference(  gBuffer(p2, byid=T, width=0),gBuffer(p1, byid=T, width=0))
      a=res@polygons
      
      dat4 = as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)
      dat4
      a
    }
    # mroe michigan
    a = c(49.8482220461071, -97.47405904039569)
    b = c(49.08579769843062, -75.6525557263955)
    c = c(40.84810428427789, -75.18327608523421)
    d = c(41.6858225737601, -94.42374137284726)
    a = func2(a,b,c,d)
    dat5=as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)
    
    # NE
    a = c(41.246257873632096, -79.64143297087567)
    b = c(40.08947370526088, -68.25584014764978)
    c = c(37.15714268482833, -68.78377974395625)
    d = c(36.73521516453001, -83.39567010016603)
    a = func2(a,b,c,d)
    dat6=as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)
    
    a = c(49.32343518715469, -76.47379539303692)
    b = c(49.67143801942952, -64.80046431914974)
    c = c(38.27080937015975, -66.26696319777878)
    d = c(40.40288869611612, -78.64421373340791)
    a = func2(a,b,c,d)
    dat7=as.data.frame(a[1][[1]]@Polygons[2][[1]]@coords)
    dat12=as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)

    a = c(43.21797785315606, -76.7594230482479)
    b = c(40.94788604781764, -73.82577204501558)
    c = c(48.70403470082143, -66.58205863585866)
    d = c(45.03637007559224, -67.87033261211045)
    #a = func2(a,b,c,d)
    #dat10=as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)

    usa4 = usa[usa$region == "long island",]
    
    #ny
    pol1 = Polygon(usa4[,c("long","lat")])
    p1 <- SpatialPoints(usa4[,c("long","lat")])
    
    a = c(41.001751527727585, -74.04773493362443)
    b = c(41.143187833020214, -72.67103648349567)
    c = c(40.08815292379516, -72.58119433974234)
    d = c(40.46715040724515, -74.34632789397413)
    temp = data.frame(x = c(a[2],b[2],c[2],d[2]), y = c(a[1],b[1],c[1],d[1]))
    temp = rbind(temp,temp[1,])
    pol2 = Polygon(temp[,c("x","y")])
    p2 <- SpatialPoints(temp[,c("x","y")])
    
    p1 <- SpatialPolygons(list(Polygons(list(pol1), "p1")))
    p2 <- SpatialPolygons(list(Polygons(list(pol2), "p2")))
    res <- gDifference(  gBuffer(p2, byid=TRUE, width=0),gBuffer(p1, byid=TRUE, width=0))
    # res <- gDifference(  gBuffer(p2, byid=T, width=0),gBuffer(p1, byid=T, width=0))
    a=res@polygons
    
    dat11 = as.data.frame(a[1][[1]]@Polygons[1][[1]]@coords)
    # 41.63111,-87.34742
    
    can = map_data("world", c("canada"))
    mex = map_data("world", c("mexico"))
    state <- map_data("state")
    county <- map_data("county")
    usa <- map_data("usa")
    
    #' Creates Interpolation Heres
    #' Change z param to whatever col you're plotting
    #' #' THIS IS WHERE THE SHINY MAGIC WILL HAPPEN
    if (input$cont_map_variable == "D0_Lo"){
      fld <- with(df, interp(x = Lon, y = Lat, z = D0_Lo, duplicate = "mean" ))
    } else if (input$cont_map_variable == "D0_Hi"){
      fld <- with(df, interp(x = Lon, y = Lat, z = D0_Hi, duplicate = "mean" ))
    } else if (input$cont_map_variable == "D1_Lo"){
      fld <- with(df, interp(x = Lon, y = Lat, z = D1_Lo, duplicate = "mean" ))
    } else if (input$cont_map_variable == "D1_Hi"){
      fld <- with(df, interp(x = Lon, y = Lat, z = D1_Hi, duplicate = "mean" ))
    } 
    
    df2 <- melt(fld$z, na.rm = TRUE)
    names(df2) <- c("x", "y", "temp")
    df2$Lon <- fld$x[df2$x]
    df2$Lat <- fld$y[df2$y]
    
    
    ggplot() +
      geom_raster(data = df2, aes(x = Lon, y = Lat, fill = temp), interpolate = T) +
      
      geom_map(data=state, map=state,
               aes(long, lat, map_id=region),
               color="black", fill=NA, size=0.1)+ 
      geom_polygon(data = dat2, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat3, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat4, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat5, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat6, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat7, aes(x=x,y=y), fill = "lightblue") +
      # geom_polygon(data = dat8, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat9, aes(x=x,y=y), fill = "lightblue") +
      #geom_polygon(data = dat10, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat11, aes(x=x,y=y), fill = "lightblue") +
      geom_polygon(data = dat12, aes(x=x,y=y), fill = "lightblue") +
      
      geom_polygon(data = dat, aes(x=x,y=y), fill = "lightblue") +
      geom_map(data=usa, map=usa,
               aes(x=long,y= lat, map_id=region),
               color="black", fill=NA, size=.25)+
      
      geom_map(data=can, map=can,
               aes(long, lat, map_id=region),
               color="black", fill="white", size=.25)+
      geom_map(data=mex, map=mex,
               aes(long, lat, map_id=region),
               color="black", fill="white", size=.25)+
      scale_fill_continuous(name = "Temp (F)",low = "yellow", high = "red") +
      
      xlab("Longitude") +
      ylab("Latitude") +
      # ylim(c(24,51))+
      #   xlim(c(-145,-50))+
      ylim(c(24,51))+
      xlim(c(-130,-57))+
      
      theme(panel.background = element_rect(fill = 'lightblue'),
            legend.background = element_rect(fill = NA),
            legend.key = element_rect(fill = NA),
            legend.position = c(0.99, .01),
            legend.justification = c("right", "bottom") ,
            panel.grid.major = element_blank(),
            panel.grid = element_blank())
    
  }, width=850, height=600)
}

shinyApp(ui = ui, server = server)