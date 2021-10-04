library(rvest)

a <- read.csv("missingCities.csv")
a
# Sys.sleep(10)
rn  = c("city", "state", "climate")
df <- data.frame(matrix(ncol = 3, nrow = 0))
df
b = c()
for (i in 1:nrow(a)) {
  url <- 'https://www.wikipedia.org/'
  weather <- html_session(url)
  # Sys.sleep(10)
  pgform <- html_form(weather)
  # Sys.sleep(10)
  form.filled <- pgform
  form.filled[[1]] <-
    pgform[[1]] %>% set_values("search" = paste(a[i, 1], a[i, 2], sep = ", "))
  session <- submit_form(session = weather, form = form.filled[[1]])
  # text = html_node(session, xpath = "//*[@id=\"Climate\"]/following-sibling::p") %>%
  # html_text()
  
  sections <-
    html_nodes(session, xpath = '//p[following-sibling::p]') %>%
    html_text()
  sections
  if (sum(grepl("does not exist", sections)) == 1) {
    df = rbind(df, data.frame(
      "city" = a[i, 1],
      "state" =  a[i, 2],
      "climate" = NA
    ))
  }else{
    for (section in sections) {
      if (grepl("Köppen", section)) {
        z = section
        splitString <- strsplit(z, " ")[[1]]
        loc <- grep("Köppen", splitString)
        loc
        sentence = paste(collapse  = ", ", splitString[(loc - 3):(loc + 3)])
        df = rbind(df,
                   data.frame(
                     "city" = a[i, 1],
                     "state" =  a[i, 2],
                     "climate" = sentence
                   ))
        
      }
    }
    if(sum(grepl("Köppen", sections)) == 0){
      df = rbind(df,
                 data.frame(
                   "city" = a[i, 1],
                   "state" =  a[i, 2],
                   "climate" = NA
                 ))
      
    }
  }
}
df

missing_cities$CLIMATE <- df$climate
missing_cities
write.csv(missing_cities, file = "missingCities.csv", row.names = F)
