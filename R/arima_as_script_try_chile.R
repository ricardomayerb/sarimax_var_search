source('./R/utils_av_ricardo.R')
library(scales)

arima_res_suffix <- "_auto_slow_cvx_faster"

arima_rds_path = "data/sarimax_objects_"
country_name <- "Chile"

data_path <- paste0("./data/excel/", country_name, ".xlsx")
external_data_path <- "./data/external/external.xlsx"
final_forecast_horizon <- c(2019, 12)
# h_max <- 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16
use_demetra <- FALSE

use_dm_force_constant <- FALSE
is_log_log <- TRUE
lambda_0_in_auto <- FALSE
mean_logical_in_auto <- TRUE 
max_x_lag <- 2

use_final_stata_variables <- TRUE

rgdp_auto_do_stepwise <- FALSE
rgdp_auto_do_approximation <- FALSE

monthly_auto_do_stepwise <- FALSE
monthly_auto_do_approximation <- FALSE


all_arima_data <- ts_data_for_arima(data_path = data_path, 
                                    external_data_path = external_data_path,
                                    all_logs = is_log_log)


this_rgdp_ts <- all_arima_data[["rgdp_ts"]]
this_internal_monthly_ts <- all_arima_data[["monthly_ts"]]
this_external_monthly_ts <- all_arima_data[["external_monthly_ts"]]
lead_q_monthly <- c(all_arima_data[["lead_q"]],
                    all_arima_data[["lead_q_external"]])
lead_m_monthly <- c(all_arima_data[["lead_m"]],
                    all_arima_data[["lead_m_external"]])

# print("this_rgdp_ts")
# print(this_rgdp_ts)
manual_set_h <- TRUE

h_max <- 8
final_forecast_horizon <- c(2019, 12)

if (manual_set_h) {
  h_max <-  h_max
  final_date_dec <- max(time(na.omit(this_rgdp_ts))) + 0.25*h_max
  final_date_ym <- as.yearmon(final_date_dec)
  final_forecast_horizon <- c(year(final_date_ym), month(final_date_ym) + 2)
  print(final_forecast_horizon)
}

if (!manual_set_h) {
  h_max <- get_hmax_q(final_forecast_horizon, current_data = this_rgdp_ts,
                      type = "monthly")
  print(h_max)
  
}


test_length <- h_max

internal_monthly_names <- colnames(this_internal_monthly_ts)
external_monthly_names <- colnames(this_external_monthly_ts)

# if (use_final_stata_variables) {
#   
#   print("using stata final variables")
#   
#   final_stata_variables <- readRDS("./data/final_stata_variables.rds")
#   country_fsv <- final_stata_variables[[country_name]]
#   
#   country_fsv_external <-  country_fsv[country_fsv %in% external_monthly_names]
#   country_fsv_internal <-  country_fsv[country_fsv %in% internal_monthly_names]
#   
#   this_internal_monthly_ts <- this_internal_monthly_ts[, country_fsv_internal]
#   this_external_monthly_ts <- this_external_monthly_ts[, country_fsv_external]
#   
#   internal_monthly_names <- country_fsv_internal
#   external_monthly_names <- country_fsv_external
# 
# }



if (use_demetra) {
  print("demetra")
  do_auto <- FALSE
  demetra_output <- get_demetra_params(data_path)
  demetra_output_external <- get_demetra_params(external_data_path)
  
  demetra_order_internal_external <- c(demetra_output[["monthly_order_list"]],
                                        demetra_output_external[["monthly_order_list"]])
  
  rgdp_order_list <- demetra_output[["rgdp_order_list"]][[1]]
  
  fit_arima_rgdp_list_dem <- fit_arimas(
    y_ts = this_rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
    this_arima_names = "rgdp")
  
  this_rgdp_arima <- fit_arima_rgdp_list_dem
  
  monthly_with_demetra_info <- names(demetra_output[["monthly_order_list"]])
  log_vec <- internal_monthly_names %in% monthly_with_demetra_info
  if (any(!log_vec)) {
    print("At least one of the variables in monthly does not have a DEMETRA line and will not be considered.")
    print("These variables are:")
    print(internal_monthly_names[!log_vec])
    internal_monthly_names <- internal_monthly_names[log_vec]
    this_internal_monthly_ts <- this_internal_monthly_ts[, internal_monthly_names]
  }
  
  
  if (use_dm_force_constant) {
    this_non_external <- "non_external_dm_s" 
    this_external <- "external_dm_s" 
    do_dm_strict <-  FALSE
  } else {
    this_non_external <- "non_external_dm_r" 
    this_external <- "external_dm_r" 
    do_dm_strict <-  TRUE
  }
} else {
  print("auto")
  do_auto <- TRUE
  demetra_order_internal_external <- NULL
  fit_arima_rgdp_list_auto <- fit_arimas(
    y_ts = this_rgdp_ts,
    auto = do_auto,  
    do_stepwise = rgdp_auto_do_step_wise,  
    this_arima_names = "rgdp", 
    my_lambda = NULL,
    do_approximation = rgdp_auto_do_approximation)
  
  this_rgdp_arima <- fit_arima_rgdp_list_auto
  
  gdp_order <- get_order_from_arima(this_rgdp_arima)[[1]]
  rgdp_order <-  gdp_order[c("p", "d", "q")]
  rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
  
  rgdp_order_list <- list(order = rgdp_order, seasonal = rgdp_seasonal,
                          mean_logical = mean_logical_in_auto,
                          log_logical = lambda_0_in_auto)
  
  do_dm_strict <-  TRUE
  this_non_external <- "non_external_auto_r" 
  this_external <- "external_auto_r"
  demetra_output <- NULL
  demetra_output_external <- NULL
}


rgdp_uncond_fc <- forecast(this_rgdp_arima[["rgdp"]], h = h_max)
rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean


tic()
extended_data <- get_extended_monthly_variables(
  do_auto = do_auto,
  use_demetra = use_demetra,
  do_dm_strict = do_dm_strict,
  do_dm_force_constant = use_dm_force_constant,
  monthly_data_ts = this_internal_monthly_ts, 
  monthly_data_external_ts = this_external_monthly_ts,
  order_list = demetra_output,
  order_list_external = demetra_output_external,
  data_path = data_path,
  final_forecast_horizon = final_forecast_horizon, 
  auto_do_stepwise = monthly_auto_do_stepwise, 
  auto_do_approximation = monthly_auto_do_approximation)
toc()


internal_mdata_ext_ts <- extended_data[[this_non_external]][["quarterly_series_ts"]]
external_mdata_ext_ts <- extended_data[[this_external]][["quarterly_series_ts"]]
mdata_ext_ts <- ts.union(internal_mdata_ext_ts, external_mdata_ext_ts)
monthly_names <- c(internal_monthly_names, external_monthly_names)
colnames(mdata_ext_ts) <- monthly_names

internal_mdata_ext_ts_monthly <- extended_data[[this_non_external]][["monthly_series_ts"]]
external_mdata_ext_ts_monthly <- extended_data[[this_external]][["monthly_series_ts"]]
mdata_ext_ts_monthly <- ts.union(internal_mdata_ext_ts_monthly, external_mdata_ext_ts_monthly)
colnames(mdata_ext_ts_monthly) <- monthly_names

if (!use_demetra) {
  all_monthly_auto_arimas <- c(extended_data$fit_arima_m_list_auto, 
                               extended_data$fit_arima_e_list_auto)
  all_monthly_auto_arma <- map(all_monthly_auto_arimas, ~ .[["arma"]])
  auto_arima_monthly_order_list <- map(all_monthly_auto_arma,
                                       ~ list(order = .[c(1,6,2)], 
                                              seasonal = .[c(3,7,4)]))
}

if (use_demetra) {
  x_order_list <- demetra_order_internal_external
}

if (!use_demetra) {
  x_order_list <- auto_arima_monthly_order_list
}



if (use_final_stata_variables) {
  
  print("using stata final variables")
  
  final_stata_variables <- readRDS("./data/final_stata_variables.rds")
  country_fsv <- final_stata_variables[[country_name]]
  
  mdata_ext_ts_monthly <- mdata_ext_ts_monthly[, country_fsv]
  mdata_ext_ts <- mdata_ext_ts[, country_fsv]
  monthly_names <- country_fsv 
  
  x_order_list <- x_order_list[country_fsv]
  lead_q_monthly <- lead_q_monthly[monthly_names] 
  lead_m_monthly <- lead_m_monthly[monthly_names] 

  all_monthly_auto_arimas <- all_monthly_auto_arimas[country_fsv]
  all_monthly_auto_arma <- map(all_monthly_auto_arimas, ~ .[["arma"]])
  auto_arima_monthly_order_list <- map(all_monthly_auto_arma,
                                       ~ list(order = .[c(1,6,2)], 
                                              seasonal = .[c(3,7,4)]))

}



####### ----------- gob   ---

tic()
arimax_and_fcs <- get_arimax_and_fcs(
  y_ts = this_rgdp_ts, 
  xreg_ts = mdata_ext_ts,
  rgdp_arima = this_rgdp_arima,
  max_x_lag = max_x_lag,
  rgdp_order_list = rgdp_order_list,
  h_max = h_max,
  data_is_log_log = is_log_log,
  force.constant = use_dm_force_constant, 
  auto_do_approximation = rgdp_auto_do_approximation, 
  auto_do_stepwise = rgdp_auto_do_stepwise)
toc()




tic()
cv_cond_uncond <- get_cv_obj_cond_uncond(
  y_ts = this_rgdp_ts, 
  xreg_ts_monthly = mdata_ext_ts_monthly,
  xreg_ts = mdata_ext_ts,
  rgdp_arima = this_rgdp_arima,
  max_x_lag = max_x_lag,
  rgdp_order_list = rgdp_order_list,
  x_order_list = x_order_list,
  use_demetra = use_demetra,
  n_cv = number_of_cv, 
  test_length = test_length,
  data_is_log_log = is_log_log, 
  training_length = train_span,
  h_max = h_max,
  force.constant = use_dm_force_constant,
  method = "ML", 
  lead_m_monthly_variables = lead_m_monthly,
  auto_do_approximation = FALSE,
  auto_do_stepwise = TRUE)
toc()





tic()
fcs_aggr_transf <- aggregate_and_transform_fcs(arimax_and_fcs, cv_cond_uncond,
                                   rgdp_ts = this_rgdp_ts,
                                   data_is_log_log = is_log_log, 
                                   rgdp_uncond_fc_mean = rgdp_uncond_fc_mean, 
                                   h_max = h_max,
                                   test_length = test_length)
toc()

arima_res_1 <- fcs_aggr_transf
arima_res_2 <- list(
  mdata_ext_ts = mdata_ext_ts,
  rgdp_ts_in_arima = this_rgdp_ts,
  all_raw_fcs = arimax_and_fcs$all_fcs,
  all_arimax = arimax_and_fcs$all_arimax,
  var_lag_order_season = arimax_and_fcs$var_lag_order_season,
  uncond_fc = rgdp_uncond_fc_mean,
  uncond_yoy_fc = fcs_aggr_transf$rgdp_uncond_yoy_fc_mean)

arima_res_3 <- list(
  arima_cv_allx =  cv_cond_uncond[["cv_allx"]],
  arima_cv_rgdp  = cv_cond_uncond[["cv_rgdp_arima"]],
  arima_cv_allx_yoy  =  cv_cond_uncond[["cv_allx_yoy"]],
  arima_cv_rgdp_yoy  = cv_cond_uncond[["cv_rgdp_arima_yoy"]],
  arima_cv_allx_logdiff  =  cv_cond_uncond[["cv_allx_logdiff"]],
  arima_cv_rgdp_logdiff  = cv_cond_uncond[["cv_rgdp_arima_logdiff"]],
  arima_cv_allx_percent  =  cv_cond_uncond[["cv_allx_percent"]],
  arima_cv_rgdp_percent  = cv_cond_uncond[["cv_rgdp_arima_percent"]]
)

arima_res <- c(arima_res_1, arima_res_2, arima_res_3)


rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
saveRDS(object = arima_res, file = rds_file_name)
  