# extract noaa data from tar.gz file structure and merge into one csv to merge into cities data

library(tidyverse)
library(data.table)

#untar into noaa data
#untar("data/us-climate-normals_2006-2020_annualseasonal_multivariate_by-station_c20210609.tar.gz", exdir = "data/noaa_data")


#combine US stations into one dataframe (SKIP MAKING DF SINCE IT"S BEEN MADE AND SAVED)
#
#column names to retain
#retain <- c("STATION"="character","LATITUDE"="character","LONGITUDE"="character","ELEVATION"="character","NAME"="character", "ANN-PRCP-NORMAL"="character")
#noaa_merged <- list.files(path="data/noaa_data", pattern = "^US1", full.names = TRUE) %>% 
#  lapply(fread, header=TRUE, select=retain) %>%  
#  bind_rows()
#write.csv(noaa_merged, file="data/noaa_us1", row.names = FALSE)

#if data has been untarred and merged, read in the csv instead
noaa_merged <- read.csv("data/noaa_us1", header=TRUE)

#highly recommend deleting this "data/noaa_data" directory after merging together bc it's huge after untarring

#read in stations data
stations <- read.table("data/ghcnd-stations.txt", header=FALSE, sep="", fill=TRUE,
                       col.names = c("STATION", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", "V1","V2","V3","V4")) %>%
  filter(grepl("^US", STATION), LATITUDE != "") %>%
  mutate(LATITUDE=as.numeric(LATITUDE),
         LONGITUDE=as.numeric(LONGITUDE),
         ELEVATION=as.numeric(ELEVATION)) %>%
  unite("CITY", starts_with("V"), remove = TRUE) %>%
  select(STATION, LAT=LATITUDE, LONG=LONGITUDE, STATE, CITY)


#combine noaa_merged and stations
noaa <- left_join(noaa_merged, stations, by = "STATION") %>%
  mutate(CITY = sub("_\\d.*", "", CITY), annual_precip=as.numeric(`ANN.PRCP.NORMAL`)) %>%
  select(NAME, CITY, STATE, LATITUDE=LAT, LONGITUDE=LONG, STATION, annual_precip) %>%
  filter(!is.na(annual_precip), !is.na(CITY))

# group city state pairs
noaa_summarized <- noaa %>%
  group_by(CITY, STATE) %>%
  summarize(.groups="keep", avg_annual_precip = mean(annual_precip), n_stations = n(), annual_precip_spread = max(annual_precip)-min(annual_precip), LAT_avg = mean(LATITUDE), LONG_avg = mean(LONGITUDE)) %>%
  ungroup()


# Function to extrapolate from nearby coordinates when city-state doesn't have a match
# Note that this will help if city names don't match by still taking the value of that city in the area
extrap_precip <- function(lat_int, long_int, range) {
  noaa_temp <- .GlobalEnv$noaa %>%
    filter(LATITUDE < lat_int+range & LATITUDE > lat_int-range & LONGITUDE < long_int+range & LONGITUDE > long_int-range)
  p <- mean(noaa_temp$annual_precip, na.rm=TRUE)
  p
}

## COMBINE WITH CITIES
cities <- read.csv('data/cities.csv')
cm <- cities %>%
  left_join(noaa_summarized, by = c('city'='CITY', 'state'='STATE'))

#Add variable and deal with NaN's
cm <- cm %>%
  rowwise() %>%
  mutate(avg_annual_precip = round(ifelse(is.na(avg_annual_precip), extrap_precip(lat, lon, 1.2), avg_annual_precip), digits=3)) %>%
  mutate(avg_annual_precip = ifelse(is.nan(avg_annual_precip), NA, avg_annual_precip))


# Redo cities with new column
cities <- cm %>%
  select(-n_stations, -annual_precip_spread, -LAT_avg, -LONG_avg)

# Overwrite cities.csv
write.csv(cities, file="data/cities.csv", row.names = FALSE)
