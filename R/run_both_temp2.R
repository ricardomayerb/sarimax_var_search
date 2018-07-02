source('./R/utils_av.R')
library(scales)

data_path <- "./data/excel/Chile.xlsx"
external_data_path <- "./data/external/external.xlsx"
final_forecast_horizon <- c(2019, 12)
h_max = 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16
use_demetra <- TRUE

if (use_demetra) {
  demetra_output <- get_demetra_params(data_path)
  demetra_output_external <- get_demetra_params(external_data_path)
}


all_arima_data <- ts_data_for_arima(data_path = data_path, 
                                    external_data_path = external_data_path,
                                    all_logs = FALSE)

rgdp_ts <- all_arima_data[["rgdp_ts"]]
monthly_ts <- all_arima_data[["monthly_ts"]]
external_monthly_ts <- all_arima_data[["external_monthly_ts"]]

monthly_names <- colnames(monthly_ts)
external_monthly_names <- colnames(external_monthly_ts)

fit_arima_logrgdp_list_dem <- fit_arimas(
  y_ts = log(rgdp_ts), order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp")

fit_arima_rgdp_list_dem <- fit_arimas(
  y_ts = rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp", my_lambda = 0)

fit_arima_rgdp_list_auto <- fit_arimas(y_ts = rgdp_ts, auto = TRUE,  
                                       this_arima_names = "rgdp", my_lambda = 0,
                                       do_approximation = TRUE)

rgdp_uncond_fc_auto <- forecast(fit_arima_rgdp_list_auto[["rgdp"]], h = h_max)
rgdp_uncond_fc_auto_mean <- rgdp_uncond_fc_auto$mean

logrgdp_uncond_fc <- forecast(fit_arima_logrgdp_list_dem[["rgdp"]], h = h_max)
logrgdp_uncond_fc_mean <- logrgdp_uncond_fc$mean

gdp_and_dates <- get_rgdp_and_dates(data_path)

gdp_order <- get_order_from_arima(fit_arima_rgdp_list_dem)[[1]]
rgdp_order <-  gdp_order[c("p", "d", "q")]
rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]

# default: forecast package's criteria about constants and differencing
# force_constant <-  TRUE

use_dm_force_constant <- TRUE

tic()
extended_data <- get_extended_monthly_variables(monthly_data_ts = monthly_ts, 
                               monthly_data_external_ts = external_monthly_ts,
                               order_list = demetra_output,
                               order_list_external = demetra_output_external,
                               gdp_and_dates = gdp_and_dates,
                               do_dm_strict = FALSE, do_auto = FALSE,
                               do_dm_force_constant = use_dm_force_constant)
toc()

internal_mdata_ext <- extended_data$non_external_dm_s
internal_mdata_ext_ts <- internal_mdata_ext$quarterly_series_ts
internal_monthly_names <- colnames(internal_mdata_ext_ts)

external_mdata_ext <- extended_data[["external_dm_s"]]
external_mdata_ext_ts <- external_mdata_ext$quarterly_series_ts
external_mdata_ext_ts_monthly <- external_mdata_ext$monthly_series_ts

external_mdata_ext_xts <- external_mdata_ext$quarterly_series_xts
external_mdata_ext_xts_monthly <- external_mdata_ext$monthly_series_xts




external_mdata_ext_r <- extended_data[["external_dm_r"]]
external_mdata_ext_ts_r <- external_mdata_ext_r$quarterly_series_ts


mdata_ext_ts <- ts.union(internal_mdata_ext_ts, external_mdata_ext_ts)
monthly_names <- c(internal_monthly_names, external_monthly_names)
colnames(mdata_ext_ts) <- monthly_names

####### ----------- goi   ---

get_cv_of_arimax <- function(y_ts, xreg_ts, y_order, y_seasonal, x_names, 
                 test_length = 8, n_cv = 16, training_length = 16, 
                 method = "ML", max_x_lag = 2, is_log_log = FALSE) {
  
  list_cv_of_arimax <- list()
  
  for (i in 0:max_x_lag) {
    this_cv_arimax <- cv_arimax(y_ts = y_ts, xreg_ts = xreg_ts, , n_cv = n_cv,
                                training_length = train_span, h_max = test_length,
                                y_order = rgdp_order, y_seasonal = y_seasonal, 
                                vec_of_names = x_names, method = method,
                                xreg_lags = 0:i, is_log_log = is_log_log)
    
    list_cv_of_arimax[[i]]  <- this_cv_arimax 
  }
  
  return(list_cv_of_arimax)
  
}

cv_arimax_0_to_2 <- get_cv_of_arimax(y_ts = log(rgdp_ts), xreg_ts = log(mdata_ext_ts), 
                                     y_order = rgdp_order, y_seasonal = rgdp_seasonal,
                                     x_names = monthly_names, is_log_log = TRUE
                                     )

cv0_e_i <- cv_arimax(y_ts = log(rgdp_ts), xreg_ts = log(internal_mdata_ext_ts),  
                     h_max =  h_max, n_cv = number_of_cv,
                     training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = internal_monthly_names,
                     method = "ML", s4xreg = FALSE)

foo <- my_arima_one_x(log(rgdp_ts), xreg_lags = 0:0, y_order = rgdp_order,
                      x_name = "ip_ue", y_seasonal = rgdp_seasonal, 
                      xreg_data = log(mdata_ext_ts))

cv0_e_e <- cv_arimax(y_ts = log(rgdp_ts), xreg_ts = log(external_mdata_ext_ts), 
                     h_max =  h_max, n_cv = number_of_cv,
                     training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, 
                     vec_of_names = external_monthly_names,  method = "CSS-ML")

cv0_e <- list(cv_errors_all_pairs_yx = c(cv0_e_i$cv_errors_all_pairs_yx,
                                         cv0_e_e$cv_errors_all_pairs_yx),
              cv_yoy_errors_all_pairs_yx = c(cv0_e_i$cv_yoy_errors_all_pairs_yx,
                                             cv0_e_e$cv_yoy_errors_all_pairs_yx)
)


cv1_e_i <- cv_arimax(y_ts = log(rgdp_ts), xreg_ts = log(internal_mdata_ext_ts),  h_max = h_max,
                     n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = internal_monthly_names,
                     method = "ML", s4xreg = FALSE, xreg_lags = 0:1)

cv1_e_e <- cv_arimax(y_ts = log(rgdp_ts), xreg_ts = log(external_mdata_ext_ts),  h_max = h_max,
                     n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = external_monthly_names,
                     method = "ML", s4xreg = FALSE, xreg_lags = 0:1)

cv1_e <- list(cv_errors_all_pairs_yx = c(cv1_e_i$cv_errors_all_pairs_yx,
                                         cv1_e_e$cv_errors_all_pairs_yx),
              cv_yoy_errors_all_pairs_yx = c(cv1_e_i$cv_yoy_errors_all_pairs_yx,
                                             cv1_e_e$cv_yoy_errors_all_pairs_yx)
)

# using two-lags xregs (k = 2)
cv2_e_i <- cv_arimax(y_ts = log(rgdp_ts), xreg_ts = log(internal_mdata_ext_ts),  h_max = h_max,
                     n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = internal_monthly_names,
                     method = "ML", s4xreg = FALSE, xreg_lags = 0:2)

cv2_e_e <- cv_arimax(y_ts = log(rgdp_ts), xreg_ts = log(external_mdata_ext_ts),  h_max = h_max,
                     n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                     y_seasonal = rgdp_seasonal, vec_of_names = external_monthly_names,
                     method = "ML", s4xreg = FALSE, xreg_lags = 0:2)

cv2_e <- list(cv_errors_all_pairs_yx = c(cv2_e_i$cv_errors_all_pairs_yx,
                                         cv2_e_e$cv_errors_all_pairs_yx),
              cv_yoy_errors_all_pairs_yx = c(cv2_e_i$cv_yoy_errors_all_pairs_yx,
                                             cv2_e_e$cv_yoy_errors_all_pairs_yx)
)

