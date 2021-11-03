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
      tabPanel("Map",
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
      )
      
    ))
      
    )
)

server <- function(input, output) {
  output$main_map <- renderPlot({
    cities <- read.csv("../cities.csv")
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
    mean_diffs$mean_diff_lo <- apply(mean_diffs[,c(2, 4, 6, 8)], 1, mean)
    mean_diffs$mean_diff_hi <- apply(mean_diffs[,c(3, 5, 7, 9)], 1, mean)
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
}

shinyApp(ui = ui, server = server)