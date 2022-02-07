# scripts

`wrangle_email.R` organizes the original email files into an R data frame.
The format of this data frame is similar to the format of the data in the email files with the irrelevant information removed. 
The data frame is written to the file `email_data.csv`.

`email_data.csv` has 11 columns:

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


`reorganize_email_data.R` reorganizes the data in `email_data.csv` into a more convenient format.
The reorganized data has a row for each unique (`date`, `city`) pair that includes all of the relevant temperature, outlook, and precipitation observations/forecasts for that pair.
This script creates two new R data frames and writes them to the files `email_data_reorganized_no_states.csv` and `email_data_reorganized.csv`.  Unlike `email_data_reorganized_no_states.csv`, `email_data_reorganized.csv` includes the `state` for each `city` and separates these two pieces of information into different columns.

`email_data_reorganized.csv` has 21 columns (`email_data_reorganized_no_states.csv` has 20 columns which include all of the following columns except the `state` column):

1. `date`: the date to which the observations/forecasts apply (format: "%Y-%m-%d")`

2. `city`: the city to which the observations/forecasts apply

3. `state`: the state (abbreviation) to which the observations/forecasts apply

4. `forecast_lo_2_prev_PM`: the low temperature forecast for `date` two days prior to `date` in the PM

5. `forecast_hi_2_prev_PM`: the high temperature forecast for `date` two days prior to `date` in the PM

6. `forecast_out_2_prev_PM`: the outlook forecast for `date` to days prior to `date` in the PM

7. `forecast_lo_prev_AM`: the low temperature forecast for `date` one day prior to `date` in the AM

8. `forecast_hi_prev_AM`: the high temperature forecast for `date` one day prior to `date` in the AM

9. `forecast_out_prev_AM`: the outlook forecast for `date` one day prior to `date` in the AM

10. `forecast_lo_prev_PM`: the low temperature forecast for `date` one day prior to `date` in the PM

11. `forecast_hi_prev_PM`: the high temperature forecast for `date` one day prior to `date` in the PM

12. `forecast_out_prev_PM`: the outlook forecast for `date` one day prior to `date` in the PM

13. `forecast_lo_current_AM`: the low temperature forecast for `date` on `date` in the AM

14. `forecast_hi_current_AM`: the high temperature forecast for `date` on `date` in the AM

15. `forecast_out_current_AM`: the outlook forecast for `date` on `date` in the AM

16. `actual_lo_current_PM`: the actual low temperature reported for `date` on `date` in the PM

17. `actual_hi_current_PM`: the actual high temperature reported for `date` on `date` in the PM

18. `actual_precip_current_PM`: the actual precipitation reported for `date` on `date` in the PM

19. `actual_lo_next_AM`: the actual low temperature reported for `date` the day after `date` in the AM

20. `actual_hi_next_AM`: the actual high temperature reported for `date` the day after `date` in the AM

21. `actual_precip_next_AM`: the actual precipitation reported for `date` the day after `date` in the AM

