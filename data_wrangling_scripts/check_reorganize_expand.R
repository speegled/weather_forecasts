# create errors data frame for reorganized email data
reorganized_errors <- create_error_df()

# read in expanded email data
# format date variable properly
# create error column
expanded <- read.csv("data/email_data_expanded.csv")
expanded$date <- parse_date_time(expanded$date, "%Y-%m-%d")
expanded <- expanded %>% mutate(error = observed_temp - forecast_temp)

all_errors_high_1 <- c(reorganized_errors$error_hi_2_prev_PM,
                       reorganized_errors$error_hi_prev_AM,
                       reorganized_errors$error_hi_prev_PM,
                       reorganized_errors$error_hi_current_AM)

all_errors_low_1 <- c(reorganized_errors$error_lo_prev_AM,
                      reorganized_errors$error_lo_prev_PM,
                      reorganized_errors$error_lo_current_AM,
                      reorganized_errors$error_lo_current_PM)

all_errors_high_2 <- expanded %>%
  filter(high_or_low == "high") %>%
  arrange(-forecast_hours_before) %>% select(error)

all_errors_low_2 <- expanded %>%
  filter(high_or_low == "low") %>%
  arrange(-forecast_hours_before) %>% select(error)

# both sums should be zero
# the errors obtained from the reorganized and expanded email data should be the same 
if (sum(all_errors_high_1 != all_errors_high_2, na.rm = T) == 0) {
  print("The high temperature errors are the same.")
}
if (sum(all_errors_low_1 != all_errors_low_2, na.rm = T) == 0) {
  print("The low temperature errors are the same.")
}
