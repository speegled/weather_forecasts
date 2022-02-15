# visualization_and_analysis_scripts

* * * 

`functions.R` contains functions that are helpful in error analysis:

1. The function `create_error_df` takes no input and returns the error data frame.

2. The function `create_mean_error_df` takes as input the `errors` data frame and  returns the mean error data frame.

3. The function `create_mean_error_df_map_info` takes as input the `mean_errors` data frame and returns the `mean_errors` data frame with latitude, longitude, and climate data added.

4. The function `plot_hist_hi_vs_lo` takes as input the `mean_errors` data frame and a boolean value `abs` that indicates whether to plot signed or absolute value errors (if `abs` = `TRUE`, then the function will plot absolute value errors.  This function returns histograms of the mean errors for high and low temperature forecasts.

5. The function `plot_hist_diff_days` takes as input the `mean_errors` data frame and a boolean value `lo` that indicates whether to plot high or low temperature errors (if `lo` = `TRUE`, then the function will plot low temperature errors).  This function returns histograms of the mean errors for high and low temperature forecasts on different days.

6. `plot_mean_errors` takes as input the `mean_errors` data frame, a boolean value `lo` (see (4)), a boolean value `abs` (see (5)), an integer `n` that indicates how many cities are labeled (the `n` cities with the smallest absolute value errors and the `n` cities with the largest absolute value errors are labeled), and an integer `day` that indicates which day's errors to plot (see below).  This function returns a map with mean errors for cities.

The `day` variable is an integer in the range [0, 4]:

* `day` = 0: overall mean forecast error for forecasts on all days
    
* `day` = 1: mean forecast error for current day AM forecasts for high temps, mean forecast error for current day PM forecasts for low temps

* `day` = 2: mean forecast error for previous day PM forecasts for high temps, mean forecast error for current day AM forecasts for low temps
 
* `day` = 3: mean forecast error for previous day AM forecasts for high temps, mean forecast error for previous day PM forecasts for low temps
 
* `day` = 4: mean forecast error for 2 days previous PM forecasts for high temps, mean forecast error for previous day AM forecasts for low temps

* * * 

`analyze_errors.R` uses the functions in `functions.R` to analyze the temperature forecast errors.