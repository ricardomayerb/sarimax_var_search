source('./R/utils_av.R')

final_forecast_horizon <- c(2019, 12)
country_name <- "Chile"
arima_res_suffix <- "_dm_s"
use_demetra <- TRUE
use_dm_force_constant <- TRUE

# To run (and save) the arima script
arima_res <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra)

# # To load previously saved arima results
# arima_res <- get_arima_results(country_name = country_name, read_results = TRUE,
#   arima_res_suffix = arima_res_suffix)

