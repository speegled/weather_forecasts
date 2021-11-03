# Scraper

This folder contains all the R files and data associated with scraping data. 

The file `weatherScraper.R` scrapes the weather reports for today, tomorrow, and day after twice a day. The work is automated on a remote server. It writes the data to a file called `weather_data.csv`.

The file `wikipediaScraper.R` gathered the koppen classification for each city from wikipedia. The resulting data is stored in `koppen.csv`.