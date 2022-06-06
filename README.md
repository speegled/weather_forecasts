# weather_forecasts

### About the Project
The goal of this data science capstone project has been to acquire national weather data to learn which areas of the U.S. struggle with weather prediction and the possible reasons why. Specifically, we focused on the error in high and low temperature forecasting.  

A major component of this project has been the collection and tidying of the data. This is because to our knowledge, there is no large national dataset cleanly containing weather observations alongside predicted values at various time intervals. The process has culminated in two datasets that are continuing to expand as new data is acquired. The first dataset contains the various observed and forecasted weather values while the second dataset contains city-level variables such as elevation and wind speed that will be later used to explain the errors in temperature prediction. Because of the lack of such a dataset in the academic space, the data we have produced and managed is a valuable contribution for further study to be conducted. 

Note that all data pulling, management, and analysis has been conducted in R.

### About the Data
Data from USA National Weather Service 12, 24, and 36 hour weather forecasts for 160 cities, as emailed by the University of Illinois list server. Data starts on January 30, 2021 and is ongoing. Data is stored in `data/email_data_reorganized.csv`.

City level data for the 160 cities plus some other US cities is in `data/cities.csv`.

The functions used to gather city level features are in `data_wrangling_scripts/model`.
* koppen_classification in `koppen.R` extracts the koppen classifications for a dataframe of latitude and longitude points using the kgc package.
* elevation in `elevation.R` also gathers the elevation in meters from a dataframe of latitude and longitude points using the elevatr package.
* distanceToCoast in `distance_to_coast.R` calculates the minimum distance between a shapefile and each latitude and longitude point in the input dataframe. The shapefile used in this case was a 10m coastline shapefile from Natural Earth.
* meanWindSpeed in `wind.R` extracts the mean wind speed for a dataframe of latitude and longitude points from a 10m resolution tif file from the Global Wind Atlas.
* citiesElevationChange in `elevation_change.R` calculates the greatest change in elevation for latitude and longitude points in the input dataframe using the model_points.csv. The function finds the closest n points in the model_points dataframe, computes the elevation change between each and the given latitude and longitude points, and saves the maximum change.
* avg_annual_precipitation was calculated in `data_wrangling_scripts/noaa_extraction.R`. The script pulls data downloaded from NOAA and takes the average annual precipitation in inches for a city or for an average value within a given radius if the city is not present in their data.

### Poster

<image src = "plots/capstone_poster.png">
