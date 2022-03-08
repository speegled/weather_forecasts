# data_wrangling_scripts

* * * 

`wrangle_email.R` organizes the original email files into an R data frame.
The format of this data frame is similar to the format of the data in the email files with the irrelevant information removed. 
The data frame is written to the file `email_data.csv` in the `data` folder.

**`email_data.csv` has 11 columns:**

1. `date_and_time`: the date and time (EST) of the observations/forecasts (format: "%Y-%m-%d %H:%M:%S")

2. `city`: the city to which the observations/forecasts apply (some cities contain their corresponding state abbreviations)

3. `previous_lo`: the actual reported low temperature for `previous`

4. `previous_hi`: the actual reported high temperature for `previous`

5. `previous_precip`: the actual reported precipitation for `previous`

6. `today_lo`: the forecast for `today`'s low temperature

7. `today_hi`: the forecast for `today`'s high temperature

8. `today_outlook`: the forecast for `today`'s outlook (factor)

9. `tomorrow_lo`: the forecast for `tomorrow`'s low temperature

10. `tomorrow_hi`: the forecast for `tomorrow`'s high temperature

11. `tomorrow_outlook`: the forecast for `tomorrow`'s outlook (factor)

Also note that

* `previous` = day before email date if AM, day of email date if PM,

* `today` = day of email date if AM, day after email date if PM, and 

* `tomorrow` = day after email date if AM, two days after email date if PM.

* * * 
 
`reorganize_email_data.R` reorganizes the data in `email_data.csv` into a more convenient format.
The reorganized data has a row for each unique (`date`, `city`) pair that includes all of the relevant temperature observations and forecasts for that pair.
This script creates a new R data frame and writes it to the file `email_data_reorganized.csv` in the `data` folder.

**`email_data_reorganized.csv` has 15 columns:**

1. `date`: the date to which the observations/forecasts apply (format: "%Y-%m-%d")`

2. `city`: the city to which the observations/forecasts apply

3. `state`: the state (abbreviation) to which the observations/forecasts apply

4. `forecast_hi_2_prev_PM`: the high temperature forecast for `date` in `city` two days prior to `date` in the PM

5. `forecast_lo_prev_AM`: the low temperature forecast for `date` in `city` one day prior to `date` in the AM

6. `forecast_hi_prev_AM`: the high temperature forecast for `date` in `city` one day prior to `date` in the AM

7. `forecast_lo_prev_PM`: the low temperature forecast for `date` in `city` one day prior to `date` in the PM

8. `forecast_hi_prev_PM`: the high temperature forecast for `date` in `city` one day prior to `date` in the PM

9. `forecast_lo_current_AM`: the low temperature forecast for `date` in `city` on `date` in the AM

10. `forecast_hi_current_AM`: the high temperature forecast for `date` in `city` on `date` in the AM

11. `forecast_lo_current_PM`: the low temperature forecast for `date` in `city` on `date` in the PM

12. `actual_lo_current_PM`: the actual low temperature reported for `date` in `city` on `date` in the PM

13. `actual_hi_current_PM`: the actual high temperature reported for `date` in `city` on `date` in the PM

14. `actual_lo_next_AM`: the actual low temperature reported for `date` in `city` the day after `date` in the AM

15. `actual_hi_next_AM`: the actual high temperature reported for `date` in `city` the day after `date` in the AM

* * * 
 
`expand_email_data.R` expands the data in `email_data.csv`.
The expanded data has eight rows for each unique (`date`, `city`) pair.
This script creates a new R data frame and writes it to the file `email_data_expanded.csv` in the `data` folder.

**`email_data_expanded.csv` has 9 columns:**

1. `date`: the date to which the observations/forecasts apply (format: "%Y-%m-%d")

2. `city`: the city to which the observations/forecasts apply

3. `state`: the state (abbreviation) to which the observations/forecasts apply

4. `high_or_low`: "low" if the forecast corresponds to a low temperature and "high" if the forecast corresponds to a high temperature

5. `forecast_hours_before`: the time of the forecast (one of (12, 24, 36, 48) corresponding to the number of hours before the actual value occurs)

6. `observed_temp`: the actual `low_or_high` temperature reported for `date` in `city`

7. `forecast_temp`: the `high_or_low` temperature forecast for `date` in `city`

8. `observed_precip`: the actual precipitation (in inches) reported for `date` in `city`

9. `forecast_outlook`: the outlook forecast for `date` in `city`

* * * 
 
`check_reorganize_expand.R` checks that `reorganize_email_data.R` and `expand_email_data.R` create data frames with identical temperature information.  It tests whether or not the forecast errors obtained from the temperature values in the data frames created in each file are identical.
