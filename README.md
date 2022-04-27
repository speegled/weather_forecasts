# weather_forecasts

Data from USA National Weather Service 12, 24, and 36 hour weather forecasts for 160 cities, as emailed by the University of Illinois list server. Data starts on January 30, 2021 and is ongoing. Data is stored in `data/email_data_reorganized.csv`.

City level data for the 160 cities plus some other US cities is in `data/cities.csv`.

The functions used to gather city level features are in `data_wrangling_scripts/model`.
* koppen_classification in `koppen.R` extracts the koppen classifications for a dataframe of latitude and longitude points using the kgc package.
* elevation in `elevation.R` also gathers the elevation in meters from a dataframe of latitude and longitude points using the elevatr package.
* distanceToCoast in `distance_to_coast.R` calculates the minimum distance between a shapefile and each latitude and longitude point in the input dataframe. The shapefile used in this case was a 10m coastline shapefile from Natural Earth.
* meanWindSpeed in `wind.R` extracts the mean wind speed for a dataframe of latitude and longitude points from a 10m resolution tif file from the Global Wind Atlas.
* citiesElevationChange in `elevation_change.R` calculates the greatest change in elevation for latitude and longitude points in the input dataframe using the model_points.csv. The function finds the closest n points in the model_points dataframe, computes the elevation change between each and the given latitude and longitude points, and saves the maximum change.