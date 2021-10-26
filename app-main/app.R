library(shiny)
library(ggplot2)
library(maps)

ui <- fluidPage(
  titlePanel("Weather Project"),
  plotOutput(outputId = "main_map")
)

server <- function(input, output) {
  output$main_map <- renderPlot({
    cities <- read.csv("../cities.csv")
    ggplot() +
      borders('world', xlim = c(-125,-65), ylim = c(20, 50), color ='black', fill='lightblue') +
      geom_point(data = cities, mapping = aes(x=LON, y=LAT), color = 'black')
  })
}

shinyApp(ui = ui, server = server)