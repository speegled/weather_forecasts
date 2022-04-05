library(rvest)

filename <- "data/cities.csv"

# read in csv
cities <- read.csv(filename)
cities <- cities[order(cities$city),]
row.names(cities) <- NULL
cities <- cities[,1:2]
cities <- data.frame(lapply(cities, as.character), stringsAsFactors=FALSE)
nrow(cities)

# scrape latitude and longitude from www.weather.gov
latList <- list()
lonList <- list()
for (j in 1:nrow(cities)){
  term <- paste(cities[j,1], cities[j,2], sep = ", ")
  print(term)
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
write.csv(cities, file = "scraped_cities.csv", row.names = F)

scraped <- read.csv("scraped_cities.csv")
scraped$LAT <- as.character(scraped$LAT)
scraped$LON <- as.character(scraped$LON)
scraped

decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

clean_lat <- c()
clean_lon <- c()
for(x in 1:nrow(scraped)) {
  lat <- scraped[x,"LAT"]
  lon <- scraped[x,"LON"]
  # remove last two characters (degree symbol and direction)
  lat <- substr(lat,1,nchar(lat)-2)
  lon <- substr(lon,1,nchar(lon)-2)
  # convert \n to NA
  if (!is.na(lat) && lat == "\n") {
    lat <- NA
  }
  if (!is.na(lon) && lon == "\n") {
    lon <- NA
  }
  # convert to float
  lat <- as.numeric(lat)
  lon <- as.numeric(lon)
  
  if (!is.na(lat) && decimalplaces(lat) < 2){
    lat <- NA
  }
  if (!is.na(lon) && decimalplaces(lon) < 2){
    lon <- NA
  }
  clean_lat <- c(clean_lat, lat)
  clean_lon <- c(clean_lon, lon)
}
scraped$clean_lat <- clean_lat
scraped$clean_lon <- clean_lon
scraped

scraped[,c("city", "state", "clean_lat", "clean_lon")]
missing_lat <- scraped[which(is.na(clean_lat)),]
missing_lon <- scraped[which(is.na(clean_lon)),]
missing_lat
missing_lon

flagged <- rbind(missing_lat, missing_lon)
flagged <- flagged[!duplicated(flagged),]
flagged <- flagged[order(flagged$city),]
row.names(flagged) <- NULL
flagged

write.csv(flagged, "data/flagged_cities.csv", row.names = F)

flagged <- read.csv("data/flagged_cities.csv")
flagged
  
continental_us_flagged <- flagged[!flagged$state %in% c("AK","HI","PR","VI"),]
continental_us_flagged
nrow(continental_us_flagged)















  