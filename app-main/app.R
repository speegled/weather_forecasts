##' This Shiny App will generate (interactive) maps of various types
##' Initially created 26 October 2021 by Clayton Strauch

library(shiny)
library(tidyverse)
library(ggplot2)
library(maps)
library(lubridate)

ui <- fluidPage(
  titlePanel("Weather Project"),
  sidebarLayout(
    sidebarPanel(
      p("Weather is hard to predict. Why? Where? *write later*"),
    width = 3),
    mainPanel( tabsetPanel(
      tabPanel("Cities Map",
          p(),
          sidebarLayout(
            sidebarPanel(
              selectInput(inputId = "bool_state_borders", label = "Include State Bordes?", 
                  choices = c("No", "Yes"), multiple = FALSE)
            ),
            mainPanel(
              plotOutput(outputId = "main_map")
            )
          )
      ),
      tabPanel("Mean Temp. Diff.",
          p(),
          sidebarLayout(
            sidebarPanel(
              selectInput(inputId = "mean_diff_hi_or_lo", label = "High or Low Temperature", 
                choices = c("High", "Low", "Both"), multiple = FALSE),
              sliderInput(inputId = "mean_diff_num_bins", label = "Number of Bins",
                          min = 5, max=30, value = 20, round = TRUE),
              checkboxInput(inputId = "mean_diff_abs", label = "Absolute Value?", value=FALSE),
            ),
            mainPanel(
              plotOutput(outputId = "mean_diffs")
            )
          )
      ),
      tabPanel("Mean Errors Map",
               p(),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "mean_diff_map_hi_or_lo", label = "High or Low Temperature", 
                               choices = c("High", "Low"), multiple = FALSE),
                   sliderInput(inputId = "mean_diff_map_dot_size", label = "Point Size",
                               min = 0, max=30, value = 5, round = TRUE),
                   sliderInput(inputId = "mean_diff_map_dot_alpha", label = "Point Transparency",
                               min = 0, max=1, value = 0.5, round = TRUE),
                 ),
                 mainPanel(
                   plotOutput(outputId = "mean_error_map")
                 )
               ))
      
    ))
      
    )
)

server <- function(input, output) {
  #data used in multiple shiny plots/objects/etc.
  cities <- read.csv("../cities.csv")
  data <- read.csv("../scripts/email_data_reorganized.csv")
  data$date <- parse_date_time(data$date, "%Y-%m-%d")
  mean_diffs <- data %>%
    mutate(diff_lo_2_prev_PM = actual_lo_next_AM - forecast_lo_2_prev_PM,
           diff_hi_2_prev_PM = actual_hi_next_AM - forecast_hi_2_prev_PM,
           diff_lo_prev_AM = actual_lo_next_AM - forecast_lo_prev_AM,
           diff_hi_prev_AM = actual_hi_next_AM - forecast_hi_prev_AM,
           diff_lo_prev_PM = actual_lo_next_AM - forecast_lo_prev_PM,
           diff_hi_prev_PM = actual_hi_next_AM - forecast_hi_prev_PM,
           diff_lo_current_AM = actual_lo_next_AM - forecast_lo_current_AM,
           diff_hi_current_AM = actual_hi_next_AM - forecast_hi_current_AM) %>%
    select(date, city,
           diff_lo_2_prev_PM, diff_hi_2_prev_PM,
           diff_lo_prev_AM, diff_hi_prev_AM,
           diff_lo_prev_PM, diff_hi_prev_PM,
           diff_lo_current_AM, diff_hi_current_AM) %>%
    group_by(city) %>%
    summarize(mean_diff_lo_2_prev_PM = mean(diff_lo_2_prev_PM, na.rm = TRUE),
              mean_diff_hi_2_prev_PM = mean(diff_hi_2_prev_PM, na.rm = TRUE),
              mean_diff_lo_prev_AM = mean(diff_lo_prev_AM, na.rm = TRUE),
              mean_diff_hi_prev_AM = mean(diff_hi_prev_AM, na.rm = TRUE),
              mean_diff_lo_prev_PM = mean(diff_lo_prev_PM, na.rm = TRUE),
              mean_diff_hi_prev_PM = mean(diff_hi_prev_PM, na.rm = TRUE),
              mean_diff_lo_current_AM = mean(diff_lo_current_AM, na.rm = TRUE),
              mean_diff_hi_current_AM = mean(diff_hi_current_AM, na.rm = TRUE))
  mean_diffs$mean_error_lo <- apply(mean_diffs[,c(2, 4, 6, 8)], 1, mean)
  mean_diffs$mean_error_hi <- apply(mean_diffs[,c(3, 5, 7, 9)], 1, mean)
  
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
  }, width=650, height=500)
  output$mean_diffs <- renderPlot({
    #Histograms of Mean High Difference and Mean Low Difference
    if(input$mean_diff_hi_or_lo == "High"){
      if(input$mean_diff_abs==FALSE){
        hist(mean_diffs$mean_diff_hi, main="Mean Difference of Predicted and Observed High Temperatures", breaks = input$mean_diff_num_bins, xlab="Temperature Difference", col="indianred1")
      }
      else{
        hist(abs(mean_diffs$mean_diff_hi), main="Mean Difference of Predicted and Observed High Temperatures", breaks = input$mean_diff_num_bins, xlab="Temperature Difference", col="indianred1")
      }
    }
    else if(input$mean_diff_hi_or_lo == "Low"){
      if(input$mean_diff_abs==FALSE){
        hist(mean_diffs$mean_diff_lo, main="Mean Difference of Predicted and Observed Low Temperatures",  breaks = input$mean_diff_num_bins, xlab="Temperature Difference", col="slateblue1")
      }
      else{
        hist(abs(mean_diffs$mean_diff_lo), main="Mean Difference of Predicted and Observed Low Temperatures",  breaks = input$mean_diff_num_bins, xlab="Temperature Difference", col="slateblue1")
      }
    }
    #Layered ggplot histogram for both low and high difference
    else{
      #construct additional components
      c(quantile(mean_diffs$mean_diff_lo, 0.025), quantile(mean_diffs$mean_diff_lo, 0.975))
      c(quantile(mean_diffs$mean_diff_hi, 0.025), quantile(mean_diffs$mean_diff_hi, 0.975))
      
      plot_hi_lo <- data.frame(mean_diff = c(mean_diffs$mean_diff_lo, mean_diffs$mean_diff_hi),
                               hi_or_lo = c(rep("lo", 164), rep("hi", 164)))
      if( input$mean_diff_abs == TRUE){
        plot_hi_lo$mean_diff <- abs(plot_hi_lo$mean_diff)
      }
      plot_means <- plot_hi_lo %>%
        group_by(hi_or_lo) %>%
        summarize(mean = mean(mean_diff))
      
      ggplot(plot_hi_lo, aes(x = mean_diff, fill = hi_or_lo, col = hi_or_lo)) + 
        geom_histogram(bins = input$mean_diff_num_bins, position = "identity", alpha = 0.5) + 
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
    
  }, width = 650, height=500)
  output$mean_error_map <- renderPlot({
    mean_error_map_hi_lo_var <- input$mean_diff_map_hi_or_lo
   cities <- filter(cities, CITY != "DOVER") %>%
      select(city=CITY, lat=LAT, lon=LON, climate=CLIMATE)
    #temp fix to see if it works
    mean_errors <- mean_diffs %>%
      inner_join(cities, by="city")
    'mean_errors <- mean_diffs %>% mutate(lat = cities$LAT,
                                          lon = cities$LON,
                                          climate = cities$CLIMATE)'
    not_cont <- c("ANCHORAGE", "HONOLULU", "SAN JUAN PR", "JUNEAU", "ST THOMAS VI")
    MainStates <- map_data("state") 
    if(mean_error_map_hi_lo_var == 'High'){
      ggplot() + 
        geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
                     color = "black", fill = "white") +
        geom_point(data = mean_errors %>% filter(!(city %in% not_cont)),
                   aes(x = lon, y = lat, col = mean_error_hi), size = input$mean_diff_map_dot_size, alpha = input$mean_diff_map_dot_alpha) +
        scale_color_gradient(low = "orange", high = "blue", na.value = NA, name = "Mean Error") +
        geom_label(data = mean_errors %>% filter(!(city %in% not_cont), abs(mean_error_hi) > 1.7),
                   aes(x = lon, y = lat, label = city), alpha = 0.5, size = 1.5) +
        labs(title = "Mean Errors for Low Temperature Forecasts") +
        theme_classic() 
    } 
    else{
      ggplot() + 
        geom_polygon(data = MainStates, aes(x = long, y = lat, group = group),
                     color = "black", fill = "white") +
        geom_point(data = mean_errors %>% filter(!(city %in% not_cont)),
                   aes(x = lon, y = lat, col = mean_error_lo), size = input$mean_diff_map_dot_size, alpha = input$mean_diff_map_dot_alpha) +
        scale_color_gradient(low = "green", high = "blue", na.value = NA, name = "Mean Error") +
        geom_label(data = mean_errors %>% filter(!(city %in% not_cont), abs(mean_error_lo) > 1.7),
                   aes(x = lon, y = lat, label = city), alpha = 0.5, size = 1.5) +
        labs(title = "Mean Errors for Low Temperature Forecasts") +
        theme_classic() 
    }
    
  }, width = 650, height=500)
}

shinyApp(ui = ui, server = server)