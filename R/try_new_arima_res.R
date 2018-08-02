source('./R/utils_av_ricardo.R')

final_forecast_horizon <- c(2019, 12)
country_name <- "Bolivia"
arima_res_suffix <- "_dm_s"
use_demetra <- TRUE
use_dm_force_constant <- TRUE
h_max <- 8

# To run (and save) the arima script
arima_res <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
  h_max = h_max, set_manual_h = TRUE)

# # To load previously saved arima results
# arima_res <- get_arima_results(country_name = country_name, read_results = TRUE,
#   arima_res_suffix = arima_res_suffix)

