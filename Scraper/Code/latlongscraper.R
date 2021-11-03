library(rvest)

filename <- "new_cities.csv"

# read in csv
cities <- read.csv(filename)
cities <- data.frame(lapply(cities, as.character), stringsAsFactors=FALSE)

# scrape latitude and longitude from www.weather.gov
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

# add values to cities csv
cities["LAT"] <- lats
cities["LON"] <- lons

# check for missing values 
which(is.na(cities["LAT"]))
which(is.na(cities["LON"]))

# write to file
write.csv(cities, file = filename, row.names = F)




