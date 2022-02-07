
verify_cities <- function(email_file_name="data/email_data.csv", cities_file_name="data/cities.csv") {
  # return city names in email_data that are not in cities csv
  # verify that all cities in the emails are in the cities csv
  
  # load email data and cities csv
  email_data <- read.csv(email_file_name)
  cities <- read.csv(cities_file_name)

  # create a list of state abbreviations and INTCNTL
  # these words will be removed from city names when cross checking
  remove_str <- append("INTCNTL", unique(as.character(cities$STATE)))

  email_cities <- c()
  # iterate over each unique city name in email_data
  for(x in unique(email_data$city)){
    
    # split city name
    split <- strsplit(x, " ")[[1]]
    # check if last word in name is a string to remove
    if(split[length(split)] %in% remove_str){
      
      # remove last word from name
      new_name <- paste(split[1:(length(split)-1)], collapse="_")
      # add new name to email_cities
      email_cities <- append(new_name, email_cities)
      next
    }
    # add old name if it does not contain a string to remove, paste with _
    email_cities <- append(paste(split, collapse="_"), email_cities)
  }

  # cities in email_data but not in cities csv
  setdiff(email_cities, cities$CITY)
}

verify_cities()

