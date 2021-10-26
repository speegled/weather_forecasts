##' This Shiny App will generate (interactive) maps of various types
##' Initially created 26 October 2021 by Clayton Strauch

library(shiny)
library(ggplot2)
library(maps)

ui <- fluidPage(
  titlePanel("Weather Project"),
  selectInput(inputId = "bool_state_borders", label = "Include State Bordes?", 
              choices = c("No", "Yes"), multiple = FALSE),
  plotOutput(outputId = "main_map")
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
}

shinyApp(ui = ui, server = server)