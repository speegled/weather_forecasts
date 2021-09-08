library(rvest)

cities <- read.csv("cities.csv")
cities <- data.frame(lapply(cities, as.character), stringsAsFactors=FALSE)

latList <- list()
lonList <- list()

for (j in 1:nrow(cities)){
  term <- paste(cities[j,1], cities[j,2], sep = ", ")
  url <- 'https://www.weather.gov/'  
  weather <- html_session(url)  
  pgform <- html_form(weather)
  form.filled <- pgform
  form.filled[[2]] <- pgform[[2]] %>% set_values("inputstring" = term)
  session <- submit_form(session = weather, form = form.filled[[2]])
  #Sys.sleep(10)
  lat <- session %>% 
    html_node(xpath = '//*[@id="current-conditions"]/div[1]/div/span/text()[1]') %>% 
    html_text()
  lon <- session %>% 
    html_node(xpath = '//*[@id="current-conditions"]/div[1]/div/span/text()[2]') %>% 
    html_text()
  latList[[j]] <- lat
  lonList[[j]] <- lon
}

lats <- unlist(latList)
lons <- unlist(lonList)

cities["Lat"] <- lats
cities["Lon"] <- lons

write.csv(cities, file = "cities.csv", row.names = F)

which(is.na(lats))
which(is.na(lons))

navalues <- which(is.na(lats))
cities[navalues,1]

cities <- read.csv("cities.csv")
cities <- data.frame(lapply(cities, as.character), stringsAsFactors=FALSE)
cities

