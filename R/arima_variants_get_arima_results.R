source('./R/utils_av_ricardo.R')

final_forecast_horizon <- c(2019, 12)
h_max <- 8
use_manual_h <- TRUE

country_name <- "Argentina"

use_demetra <- TRUE
use_dm_force_constant <- TRUE

tic("outer")

tic("demetra+forcing constant")
use_final_stata_variables <- FALSE
arima_res_suffix <- "_dm_s"
arima_res_dm_s <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
  h_max = h_max, set_manual_h = use_manual_h, 
  use_final_stata_variables = use_final_stata_variables)
toc()

tic("demetra+forcing a constant, using only final stata variables")
use_final_stata_variables <- TRUE
arima_res_suffix <- "_dm_s_fsv"
arima_res_dm_s_fsv <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
  h_max = h_max, set_manual_h = use_manual_h, 
  use_final_stata_variables = use_final_stata_variables)
toc()

use_dm_force_constant <- FALSE

use_final_stata_variables <- FALSE
arima_res_suffix <- "_dm_r"

tic("demetra without forcing a constant")
arima_res_dm_s <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
  h_max = h_max, set_manual_h = use_manual_h, 
  use_final_stata_variables = use_final_stata_variables)
toc()

use_final_stata_variables <- TRUE
arima_res_suffix <- "_dm_r_fsv"

tic("demetra without forcing a constant, using only final stata variables")
arima_res_dm_s <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
  h_max = h_max, set_manual_h = use_manual_h, 
  use_final_stata_variables = use_final_stata_variables)
toc()

use_demetra <- FALSE
use_final_stata_variables <- FALSE
arima_res_suffix <- "_auto"

tic("using auto.arima")
arima_res_dm_s <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
  h_max = h_max, set_manual_h = use_manual_h, 
  use_final_stata_variables = use_final_stata_variables)
toc()

use_final_stata_variables <- TRUE
arima_res_suffix <- "_auto_fsv"
tic("using auto.arima, using only final stata variables")
arima_res_dm_s <- get_arima_results(
  country_name = country_name, use_dm_force_constant = use_dm_force_constant,
  arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
  h_max = h_max, set_manual_h = use_manual_h, 
  use_final_stata_variables = use_final_stata_variables)
toc()

toc()



# # To load previously saved arima results
# arima_res <- get_arima_results(country_name = country_name, read_results = TRUE,
#   arima_res_suffix = arima_res_suffix)

