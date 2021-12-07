##' This Shiny App will generate (interactive) maps of various types
##' Initially created 26 October 2021 by Clayton Strauch

library(shiny)
library(tidyverse)
library(ggplot2)
library(maps)
library(lubridate)

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
      tabPanel("Mean Errors Map",
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
               ))
      
    ))
      
    )
)

server <- function(input, output) {
  #data used in multiple shiny plots/objects/etc.
  cities <- read.csv("../cities.csv")
  data <- read.csv("../scripts/email_data_reorganized.csv")
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
      
      plot_hi_lo <- data.frame(mean_error = c(mean_errors$mean_error_lo, mean_errors$mean_error_hi),
                               hi_or_lo = c(rep("lo", 164), rep("hi", 164)))
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
}

shinyApp(ui = ui, server = server)