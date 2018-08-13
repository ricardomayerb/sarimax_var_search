source('./R/utils_av_ricardo.R')

final_forecast_horizon <- c(2019, 12)
h_max <- 8
use_manual_h <- TRUE


country_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia",
                   "Ecuador", "Paraguay", "Peru", "Uruguay")


use_demetra <- TRUE
use_dm_force_constant <- TRUE

for (country in country_names) {
  tic()
  print(country)
  country_name <- country

  use_final_stata_variables <- TRUE
  arima_res_suffix <- "_dm_s_fsv"
  arima_res_dm_s_fsv <- get_arima_results(
    country_name = country_name, use_dm_force_constant = use_dm_force_constant,
    arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
    h_max = h_max, set_manual_h = use_manual_h, 
    use_final_stata_variables = use_final_stata_variables)
  
  toc()
}

