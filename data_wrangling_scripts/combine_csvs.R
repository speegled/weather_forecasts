library(stringr)

file1 <- "email_cities.csv"
file2 <- "new_cities.csv"

end_file <- "cities.csv"

# read csvs
df1 <- read.csv(file1, header = TRUE)
df2 <- read.csv(file2, header = TRUE)

# append dfs
cities <- rbind(df1, df2)
cities

write.csv(cities, file=end_file, row.names=F)




