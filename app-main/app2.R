#' Shiny App 2 -- For Krigging Variations -- Created 20 April 2022 by Clayton Strauch
#' 

library(shiny)
library(tidyverse)
library(sp)
library(gstat)

'state <- map_data("state")

+ geom_map(data=state, map=state,
           aes(long, lat, map_id=region),
           color="black", fill=NA, size=0.1)
'

ui <- fluidPage(
  titlePanel(" Weather Project - Kriging"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "high_or_low", label = "High or Low Temperature?", 
                  choices = c("high", "low")),
      radioButtons(inputId = "hours_before", label = "Forecast Hours Before", 
                   choices = c(12, 24, 36, 48)),
      checkboxInput(inputId = "display_points", label = "Display Data Points?", value = TRUE),
      width = 3
    ),
    mainPanel(
      plotOutput(outputId = "krig_map"),
      width = 9
    )
  )
  
)

server <- function(input, output){
  cities <- read.csv('../data/cities.csv')
  
  grid <- read.csv("../data/model_points.csv") %>%
    select(long=lon, lat)
  coordinates(grid) <- ~lat + long
  
  output$krig_map <- renderPlot({
    df <- read.csv("../data/email_data_expanded.csv") %>%
      filter(high_or_low == input$high_or_low & forecast_hours_before == input$hours_before & !is.na(forecast_temp) & !(state %in% c("AK","HI","VI","PR"))) %>%
      select(date, city, state, observed_temp, forecast_temp) %>%
      mutate(error = abs(forecast_temp - observed_temp)) %>%
      group_by(city, state) %>%
      summarize(mean_error = mean(error, na.rm=T), .groups = "drop") %>%
      inner_join(cities, by=c("city", "state")) %>%
      select(-koppen, -avg_annual_precip)
    
    coordinates(df) <- ~lat + lon
    
    lzn.vgm <- variogram(mean_error ~ 1, df) # calculates sample variogram values 
    
    lzn.fit = fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
    
    lzn.kriged <- krige(mean_error ~ 1, df, grid, model=lzn.fit)
    
    if(input$display_points == TRUE){
      lzn.kriged %>% as.data.frame %>%
      ggplot(aes(x=long, y=lat)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
      scale_fill_gradient(low = "yellow", high="red", name="Mean Temperature Error") +
      theme_bw() + 
      geom_point(data = data.frame(df), aes(x=lon, y=lat))
    }
    else{
      lzn.kriged %>% as.data.frame %>%
      ggplot(aes(x=long, y=lat)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
      scale_fill_gradient(low = "yellow", high="red", name="Mean Temperature Error") +
      theme_bw()
    }
  })  
}


shinyApp(ui = ui, server = server)