source('./R/utils_av.R')
library(scales)

data_path <- "./data/excel/Chile.xlsx"
external_data_path <- "./data/external/external.xlsx"
final_forecast_horizon <- c(2019, 12)
h_max <-  8 # last rgdp data is 2017 Q4
test_length <- h_max
number_of_cv = 8
train_span = 16
use_demetra <- TRUE
is_log_log <- TRUE
max_x_lag <- 2

if (use_demetra) {
  demetra_output <- get_demetra_params(data_path)
  demetra_output_external <- get_demetra_params(external_data_path)
  rgdp_order_list <- demetra_output[["rgdp_order_list"]][[1]]
}

all_arima_data <- ts_data_for_arima(data_path = data_path, 
                                    external_data_path = external_data_path,
                                    all_logs = is_log_log)

this_rgdp_ts <- all_arima_data[["rgdp_ts"]]
this_internal_monthly_ts <- all_arima_data[["monthly_ts"]]
this_external_monthly_ts <- all_arima_data[["external_monthly_ts"]]

internal_monthly_names <- colnames(this_internal_monthly_ts)
external_monthly_names <- colnames(this_external_monthly_ts)


fit_arima_rgdp_list_dem <- fit_arimas(
  y_ts = this_rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp")

this_rgdp_arima <- fit_arima_rgdp_list_dem


rgdp_uncond_fc <- forecast(fit_arima_rgdp_list_dem[["rgdp"]], h = h_max)

rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean

use_dm_force_constant <- TRUE
this_non_external <- "non_external_dm_s" 
this_external <- "external_dm_s" 

tic()
extended_data <- get_extended_monthly_variables(monthly_data_ts = this_internal_monthly_ts, 
                               monthly_data_external_ts = this_external_monthly_ts,
                               order_list = demetra_output,
                               order_list_external = demetra_output_external,
                               do_dm_strict = FALSE, do_auto = FALSE,
                               do_dm_force_constant = use_dm_force_constant,
                               data_path = data_path)
toc()


internal_mdata_ext_ts <- extended_data[[this_non_external]][["quarterly_series_ts"]]
external_mdata_ext_ts <- extended_data[[this_external]][["quarterly_series_ts"]]


mdata_ext_ts <- ts.union(internal_mdata_ext_ts, external_mdata_ext_ts)
monthly_names <- c(internal_monthly_names, external_monthly_names)
colnames(mdata_ext_ts) <- monthly_names

####### ----------- goi   ---

get_cv_of_arimax <- function(y_ts, xreg_ts, y_order, y_seasonal, x_names, 
                 test_length = 8, n_cv = 8, training_length = 16, 
                 method = "ML", max_x_lag = 2, is_log_log = FALSE,
                 y_include_drift = TRUE, rgdp_order_list = NULL) {
  
  # demetra_rgdp_mean_logical <-  rgdp_order_list[["mean_logical"]]

  list_cv_of_arimax <- list()
  
  for (i in 0:max_x_lag) {
    this_cv_arimax <- cv_arimax(y_ts = y_ts, xreg_ts = xreg_ts, n_cv = n_cv,
                                training_length = training_length, h_max = test_length,
                                y_order = y_order, y_seasonal = y_seasonal, 
                                vec_of_names = x_names, method = method,
                                xreg_lags = 0:i, is_log_log = is_log_log, 
                                y_include_drift = y_include_drift)

    list_cv_of_arimax[[i + 1]]  <- this_cv_arimax
  }
  
  return(list_cv_of_arimax)

}



get_cv_obj_cond_uncond <- function(y_ts, xreg_ts, rgdp_arima, max_x_lag, 
                                   rgdp_order_list, n_cv, test_length, 
                                   is_log_log, training_length, h_max) {
  
  gdp_order <- get_order_from_arima(rgdp_arima)[[1]]
  rgdp_order <-  gdp_order[c("p", "d", "q")]
  rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
  demetra_rgdp_mean_logical <- rgdp_order_list[["mean_logical"]]
  monthly_names <- colnames(xreg_ts)
  
  
  
  cv_arimax_0_to_2 <- get_cv_of_arimax(
    y_ts = y_ts, xreg_ts = xreg_ts, y_order = rgdp_order, 
    training_length = training_length, test_length = test_length,
    y_seasonal = rgdp_seasonal, x_names = monthly_names, 
    is_log_log = is_log_log, n_cv = n_cv,
    max_x_lag = max_x_lag, y_include_drift = demetra_rgdp_mean_logical
  )
  
  cv_rgdp_e <- cv_arima(y_ts = y_ts, h_max = h_max, n_cv = n_cv,
                        training_length = training_length, y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal, method = "ML",  
                        y_include_drift = demetra_rgdp_mean_logical)
  
  
  
  cv_rgdp_e_yoy <- cv_rgdp_e[["cv_yoy_errors"]]
  cv_rgdp_e <- cv_rgdp_e[["cv_errors"]]
  cv_rdgp_rmse <- compute_rmse(cv_rgdp_e, h_max = h_max, n_cv = n_cv)
  cv_rdgp_rmse_yoy <- compute_rmse(cv_rgdp_e_yoy, h_max = h_max, n_cv = n_cv)
  cv_rmse_each_h_rgdp <- cv_rdgp_rmse[["same_h_rmse"]] %>% 
    mutate(variable = "rgdp", lag = 0)
  cv_rmse_each_h_rgdp_yoy <- cv_rdgp_rmse_yoy[["same_h_rmse"]] %>% 
    mutate(variable = "rgdp", lag = 0)
  
  
  
  cv_allx_yoy <- map(cv_arimax_0_to_2, "cv_yoy_errors_all_pairs_yx")
  cv_allx <- map(cv_arimax_0_to_2, "cv_errors_all_pairs_yx")
  cv_rmse_list <- map(cv_allx,  ~ map(., compute_rmse, h_max = h_max, n_cv = n_cv))
  cv_rmse_list_yoy <- map(cv_allx_yoy, 
                          ~ map(., compute_rmse, h_max = h_max, n_cv = n_cv))
  cv_rmse_each_h <- map(cv_rmse_list,
                        ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
                          mutate(variable = monthly_names))
  cv_rmse_each_h_yoy <- map(cv_rmse_list_yoy,
                            ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
                              mutate(variable = monthly_names))
  cv_all_x_rmse_each_h <- reduce(cv_rmse_each_h, rbind) %>% 
    mutate(lag =   reduce(
      map(seq(0,length(cv_allx) - 1), rep, length(cv_allx[[1]])),
      c))
  
  
  cv_all_x_rmse_each_h_yoy <- reduce(cv_rmse_each_h_yoy, rbind) %>% 
    mutate(lag = reduce(
      map(seq(0,length(cv_allx) - 1), rep, length(cv_allx[[1]])), c))
  
  return(list(
    cv_allx = cv_allx, 
    cv_allx_yoy = cv_allx_yoy,
    cv_rmse_each_h = cv_rmse_each_h, cv_rmse_each_h_yoy,
    cv_rmse_each_h_rgdp = cv_rmse_each_h_rgdp,
    cv_rmse_each_h_rgdp_yoy = cv_rmse_each_h_rgdp_yoy,
    cv_rgdp_e = cv_rgdp_e,
    cv_rgdp_e_yoy = cv_rgdp_e_yoy,
    cv_all_x_rmse_each_h = cv_all_x_rmse_each_h,
    cv_all_x_rmse_each_h_yoy = cv_all_x_rmse_each_h_yoy)
    )
  
  
}


cv_cond_uncond <- get_cv_obj_cond_uncond(y_ts = this_rgdp_ts, 
                              xreg_ts = mdata_ext_ts,
                              rgdp_arima = this_rgdp_arima,
                              max_x_lag = max_x_lag,
                              rgdp_order_list = rgdp_order_list,
                              n_cv = number_of_cv, 
                              test_length = test_length,
                              is_log_log = is_log_log, 
                              training_length = train_span,
                              h_max = h_max)

get_arimax_and_fcs <- function(y_ts, xreg_ts, rgdp_arima, max_x_lag,
                               rgdp_order_list, h_max) {
  
  gdp_order <- get_order_from_arima(rgdp_arima)[[1]]
  rgdp_order <-  gdp_order[c("p", "d", "q")]
  rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
  demetra_rgdp_mean_logical <- rgdp_order_list[["mean_logical"]]
  monthly_names <- colnames(xreg_ts)
  
  all_arimax_list <- list()
  
  for (i in 0:max_x_lag) {
    this_arimax <- my_arimax(y_ts = y_ts, xreg_ts = xreg_ts,  y_order = rgdp_order, 
                             y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                             xreg_lags = 0:i, y_include_mean = demetra_rgdp_mean_logical)
    
    all_arimax_list[[i + 1]] <- this_arimax
  }
  
  names(all_arimax_list) <- paste0("arimax_", seq(0, length(all_arimax_list) - 1))
  
  all_arimax <- as_tibble(all_arimax_list) %>% 
    mutate(id_fc = monthly_names) %>% 
    gather(key = "type_arimax", value = "arimax", -id_fc) %>% 
    mutate(lag = as.integer(str_remove(type_arimax, "arimax_")), 
           armapar = map(arimax, c("arma")),
           arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
           arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
    )
  
  all_fc_list <- list()
  for (i in 0:max_x_lag) {
    this_fc <- forecast_xreg(all_arimax_list[[i+1]], xreg_ts, h = h_max, 
                             vec_of_names = monthly_names, xreg_lags = 0:i)
    
    all_fc_list[[i + 1]] <- this_fc
  }
  
  names(all_fc_list) <- paste0("fc_", seq(0, length(all_arimax_list) - 1))
  
  
  all_fcs <- as_tibble(all_fc_list)   %>% 
    mutate(id_fc = monthly_names)  %>%
    gather(key = "type_fc", value = "fc", -id_fc) %>% 
    mutate(lag = as.integer(str_remove(type_fc, "fc_")),
           raw_rgdp_fc = map(fc, "mean")) %>% 
    mutate(armapar = map(fc, c("model", "arma")),
           arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
           arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
    ) %>% 
    mutate(data_and_fc = map(raw_rgdp_fc, ~ts(data = c(y_ts, .), frequency = 4,
                                              start = stats::start(y_ts))),
           yoy_data_and_fc = map(data_and_fc, ~ make_yoy_ts(exp(.))),
           yoy_raw_rgdp_fc = map2(yoy_data_and_fc, raw_rgdp_fc,
                                  ~ window(.x, start = stats::start(.y)))
    )
  
  # all_fcs1 <- all_fcs[1,]
  
  var_lag_order_season <- all_fcs %>% 
    dplyr::select(id_fc, lag, arima_order, arima_seasonal) %>% 
    rename(variable = id_fc, lag = lag)
  
  rgdp_var_lag_order_season <- tibble(
    variable = "rgdp", lag = 0, 
    arima_order = list(rgdp_order), arima_seasonal = list(rgdp_seasonal)) 
  
  var_lag_order_season <- rbind(rgdp_var_lag_order_season, var_lag_order_season)
  
  mat_of_raw_fcs <- reduce(all_fcs$raw_rgdp_fc, rbind) 
  
  return(list(
    all_arimax = all_arimax,
    all_fcs = all_fcs,
    var_lag_order_season = var_lag_order_season,
    mat_of_raw_fcs = mat_of_raw_fcs
  ))
}


arimax_and_fcs <- get_arimax_and_fcs(y_ts = this_rgdp_ts, 
                          xreg_ts = mdata_ext_ts,
                          rgdp_arima = this_rgdp_arima,
                          max_x_lag = max_x_lag,
                          rgdp_order_list = rgdp_order_list,
                          h_max = h_max)



aggregate_and_transform_fcs <- function(arimax_and_fcs, cv_cond_uncond,
                                        rgdp_ts) {
  
  mat_of_raw_fcs <- arimax_and_fcs$mat_of_raw_fcs
  cv_all_x_rmse_each_h <- cv_cond_uncond$cv_all_x_rmse_each_h
  cv_rmse_each_h_rgdp <-  cv_cond_uncond[["cv_rmse_each_h_rgdp"]]
  cv_all_x_rmse_each_h_yoy <- cv_cond_uncond$cv_all_x_rmse_each_h_yoy
  cv_rmse_each_h_rgdp_yoy <-  cv_cond_uncond[["cv_rmse_each_h_rgdp_yoy"]]
  
  weigthed_fcs <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                   mat_cv_rmses_from_x = cv_all_x_rmse_each_h,
                                   vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp)
  
  
  weigthed_fcs[is.nan(weigthed_fcs)] <- rgdp_uncond_fc_mean[is.nan(weigthed_fcs)]
  
  
  fcs_using_yoy_weights <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                            mat_cv_rmses_from_x = cv_all_x_rmse_each_h_yoy,
                                            vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_yoy)
  
  fcs_using_yoy_weights[ is.nan(fcs_using_yoy_weights)] <- rgdp_uncond_fc_mean[ is.nan(fcs_using_yoy_weights)]
  
  weigthed_fcs <- ts(weigthed_fcs, 
                     start = stats::start(rgdp_uncond_fc_mean), 
                     frequency = 4)
  
  rgdp_data_and_uncond_fc <- ts(data = c(this_rgdp_ts, rgdp_uncond_fc_mean), 
                                frequency = 4, start = stats::start(this_rgdp_ts))
  
  yoy_rgdp_data_and_uncond_fc <- make_yoy_ts(exp(rgdp_data_and_uncond_fc))
  
  rgdp_uncond_yoy_fc_mean <- window(yoy_rgdp_data_and_uncond_fc,
                                    start = stats::start(rgdp_uncond_fc_mean))
  
  fcs_using_yoy_weights <- ts(fcs_using_yoy_weights, 
                              start = stats::start(rgdp_uncond_fc_mean), 
                              frequency = 4)
  
  
  final_rgdp_and_w_fc <- ts(c(this_rgdp_ts, weigthed_fcs), frequency = 4,
                            start = stats::start(this_rgdp_ts))
  
  final_rgdp_and_yoyw_fc <- ts(c(this_rgdp_ts, fcs_using_yoy_weights), frequency = 4,
                               start = stats::start(this_rgdp_ts))
  
  expo_final_rgdp_and_w_fc <- exp(final_rgdp_and_w_fc)
  expo_final_rgdp_and_yoyw_fc <- exp(final_rgdp_and_yoyw_fc)
  
  yoy_growth_expo_final_rgdp_and_w_fc <- diff(expo_final_rgdp_and_w_fc, lag = 4)/lag.xts(expo_final_rgdp_and_w_fc, k = 4)
  yoy_growth_expo_final_rgdp_and_yoyw_fc <- diff(expo_final_rgdp_and_yoyw_fc, lag = 4)/lag.xts(expo_final_rgdp_and_yoyw_fc, k = 4)
  
  
  
}


foo <- aggregate_and_transform_fcs(arimax_and_fcs, cv_cond_uncond,
                                   rgdp_ts = this_rgdp_ts  )

mat_of_raw_fcs <- arimax_and_fcs$mat_of_raw_fcs
cv_all_x_rmse_each_h <- cv_cond_uncond$cv_all_x_rmse_each_h
cv_rmse_each_h_rgdp <-  cv_cond_uncond[["cv_rmse_each_h_rgdp"]]
cv_all_x_rmse_each_h_yoy <- cv_cond_uncond$cv_all_x_rmse_each_h_yoy
cv_rmse_each_h_rgdp_yoy <-  cv_cond_uncond[["cv_rmse_each_h_rgdp_yoy"]]




weigthed_fcs <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                 mat_cv_rmses_from_x = cv_all_x_rmse_each_h,
                                 vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp)


weigthed_fcs[ is.nan(weigthed_fcs)] <- rgdp_uncond_fc_mean[ is.nan(weigthed_fcs)]


fcs_using_yoy_weights <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                          mat_cv_rmses_from_x = cv_all_x_rmse_each_h_yoy,
                                          vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_yoy)

fcs_using_yoy_weights[ is.nan(fcs_using_yoy_weights)] <- rgdp_uncond_fc_mean[ is.nan(fcs_using_yoy_weights)]

weigthed_fcs <- ts(weigthed_fcs, 
                   start = stats::start(rgdp_uncond_fc_mean), 
                   frequency = 4)

rgdp_data_and_uncond_fc <- ts(data = c(this_rgdp_ts, rgdp_uncond_fc_mean), 
                              frequency = 4, start = stats::start(this_rgdp_ts))

yoy_rgdp_data_and_uncond_fc <- make_yoy_ts(exp(rgdp_data_and_uncond_fc))

rgdp_uncond_yoy_fc_mean <- window(yoy_rgdp_data_and_uncond_fc,
                                  start = stats::start(rgdp_uncond_fc_mean))

fcs_using_yoy_weights <- ts(fcs_using_yoy_weights, 
                            start = stats::start(rgdp_uncond_fc_mean), 
                            frequency = 4)


final_rgdp_and_w_fc <- ts(c(this_rgdp_ts, weigthed_fcs), frequency = 4,
                          start = stats::start(this_rgdp_ts))

final_rgdp_and_yoyw_fc <- ts(c(this_rgdp_ts, fcs_using_yoy_weights), frequency = 4,
                             start = stats::start(this_rgdp_ts))

expo_final_rgdp_and_w_fc <- exp(final_rgdp_and_w_fc)
expo_final_rgdp_and_yoyw_fc <- exp(final_rgdp_and_yoyw_fc)

yoy_growth_expo_final_rgdp_and_w_fc <- diff(expo_final_rgdp_and_w_fc, lag = 4)/lag.xts(expo_final_rgdp_and_w_fc, k = 4)
yoy_growth_expo_final_rgdp_and_yoyw_fc <- diff(expo_final_rgdp_and_yoyw_fc, lag = 4)/lag.xts(expo_final_rgdp_and_yoyw_fc, k = 4)






























# 
# 
# 
# gdp_order <- get_order_from_arima(this_rgdp_arima)[[1]]
# rgdp_order <-  gdp_order[c("p", "d", "q")]
# rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
# demetra_rgdp_mean_logical <- rgdp_order_list[["mean_logical"]]
# 
# 
# tic()
# cv_arimax_0_to_2 <- get_cv_of_arimax(y_ts = this_rgdp_ts, xreg_ts = mdata_ext_ts, 
#                                      y_order = rgdp_order, y_seasonal = rgdp_seasonal,
#                                      x_names = monthly_names, is_log_log = TRUE,
#                                      max_x_lag = 2, y_include_drift = demetra_rgdp_mean_logical
# )
# toc()
# 
# cv_rgdp_e <- cv_arima(y_ts = this_rgdp_ts, h_max = h_max, n_cv = number_of_cv,
#                       training_length = train_span,  y_order = rgdp_order, 
#                       y_seasonal = rgdp_seasonal,
#                       method = "ML",  y_include_drift = demetra_rgdp_mean_logical)
# 
# 
# 
# cv_rgdp_e_yoy <- cv_rgdp_e[["cv_yoy_errors"]]
# cv_rgdp_e <- cv_rgdp_e[["cv_errors"]]
# cv_rdgp_rmse <- compute_rmse(cv_rgdp_e, h_max = h_max, n_cv = number_of_cv)
# cv_rdgp_rmse_yoy <- compute_rmse(cv_rgdp_e_yoy, h_max = h_max, n_cv = number_of_cv)
# cv_rmse_each_h_rgdp <- cv_rdgp_rmse[["same_h_rmse"]] %>% 
#   mutate(variable = "rgdp", lag = 0)
# cv_rmse_each_h_rgdp_yoy <- cv_rdgp_rmse_yoy[["same_h_rmse"]] %>% 
#   mutate(variable = "rgdp", lag = 0)
# 
# 
# 
# cv_allx_yoy <- map(cv_arimax_0_to_2, "cv_yoy_errors_all_pairs_yx")
# cv_allx <- map(cv_arimax_0_to_2, "cv_errors_all_pairs_yx")
# cv_rmse_list <- map(cv_allx,  ~ map(., compute_rmse, h_max = h_max, n_cv = number_of_cv))
# cv_rmse_list_yoy <- map(cv_allx_yoy,  ~ map(., compute_rmse, h_max = h_max, n_cv = number_of_cv))
# cv_rmse_each_h <- map(cv_rmse_list, ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
#                         mutate(variable = monthly_names))
# cv_rmse_each_h_yoy <- map(cv_rmse_list_yoy, ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
#                             mutate(variable = monthly_names))
# cv_all_x_rmse_each_h <- reduce(cv_rmse_each_h, rbind) %>% 
#   mutate(lag =   reduce(map(seq(0,length(cv_allx)-1), rep, length(cv_allx[[1]])), c))
# 
# cv_all_x_rmse_each_h_yoy <- reduce(cv_rmse_each_h_yoy, rbind) %>% 
#   mutate(lag =   reduce(map(seq(0,length(cv_allx)-1), rep, length(cv_allx[[1]])), c))


# all_arimax_list <- list()
# gdp_order <- get_order_from_arima(this_rgdp_arima)[[1]]
# rgdp_order <-  gdp_order[c("p", "d", "q")]
# rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
# demetra_rgdp_mean_logical <- rgdp_order_list[["mean_logical"]]
# monthly_names <- colnames(xreg_ts)
# 
# for (i in 0:max_x_lag) {
#   this_arimax <- my_arimax(y_ts = this_rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
#                            y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
#                            xreg_lags = 0:i, y_include_mean = demetra_rgdp_mean_logical)
#   
#   all_arimax_list[[i + 1]] <- this_arimax
# }
# 
# names(all_arimax_list) <- paste0("arimax_", seq(0, length(all_arimax_list) - 1))
# 
# all_arimax <- as_tibble(all_arimax_list) %>% 
#   mutate(id_fc = monthly_names) %>% 
#   gather(key = "type_arimax", value = "arimax", -id_fc) %>% 
#   mutate(lag = as.integer(str_remove(type_arimax, "arimax_")), 
#          armapar = map(arimax, c("arma")),
#          arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
#          arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
#   )
# 
# all_fc_list <- list()
# for (i in 0:max_x_lag) {
#   this_fc <- forecast_xreg(all_arimax_list[[i+1]], mdata_ext_ts, h = h_max, 
#                            vec_of_names = monthly_names, xreg_lags = 0:i)
#   
#   all_fc_list[[i+1]] <- this_fc
# }
# 
# names(all_fc_list) <- paste0("fc_", seq(0, length(all_arimax_list)-1))
# 
# 
# all_fcs <- as_tibble(all_fc_list)   %>% 
#   mutate(id_fc = monthly_names)  %>%
#   gather(key = "type_fc", value = "fc", -id_fc) %>% 
#   mutate(lag = as.integer(str_remove(type_fc, "fc_")),
#          raw_rgdp_fc = map(fc, "mean")) %>% 
#   mutate(armapar = map(fc, c("model", "arma")),
#          arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
#          arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
#   ) %>% 
#   mutate(data_and_fc = map(raw_rgdp_fc, ~ts(data = c(this_rgdp_ts, .), frequency = 4,
#                                             start = stats::start(this_rgdp_ts))),
#          yoy_data_and_fc = map(data_and_fc, ~ make_yoy_ts(exp(.))),
#          yoy_raw_rgdp_fc = map2(yoy_data_and_fc, raw_rgdp_fc,
#                                 ~ window(.x, start = stats::start(.y)))
#   )
# 
# # all_fcs1 <- all_fcs[1,]
# 
# var_lag_order_season <- all_fcs %>% 
#   dplyr::select(id_fc, lag, arima_order, arima_seasonal) %>% 
#   rename(variable = id_fc, lag = lag)
# 
# rgdp_var_lag_order_season <- tibble(
#   variable = "rgdp", lag = 0, 
#   arima_order = list(rgdp_order), arima_seasonal = list(rgdp_seasonal)) 
# 
# var_lag_order_season <- rbind(rgdp_var_lag_order_season, var_lag_order_season)
# 
# mat_of_raw_fcs <- reduce(all_fcs$raw_rgdp_fc, rbind) 

