#' This script transforms the email data into a proper R DataFrame
#' Created by Clayton Strauch on 14 September 2021


#import data for testing
email = readLines("data/wx-natnl Digest Fri, 02 Apr 2021 (1_2).eml[1].eml")

extract_data_from_email <- function(email) {
  #initiate empty vector and boolean variable
  v <-  c()
  b <- FALSE
  
  #keep lines that are between "SELECTED CITIES" and "$$" then repeat for entire email
  for (line in email){
    if (grepl("SELECTED CITIES", line, ignore.case=FALSE, fixed=TRUE)){
      b <-  TRUE 
    }
    
    if (b == TRUE && line != "") {
      #do not include lines that contain these values
      if ( !(grepl("TELECOMMUNICATION|PRECIPITATION|INDICATE|TEMPERATURE", line) || grepl("$$", line, fixed=TRUE))){
        v <- append(v, line)
      }
    }
    
    if (grepl("$$", line, ignore.case=FALSE, fixed=TRUE)){
      b <-  FALSE
    }
  }

}

