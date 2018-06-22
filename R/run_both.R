source('./R/utils_av.R')

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


do_univariate_rgdp <- function(rgdp_data, models = "all", n_offset = 0, 
                               freq = 4, h_max = 8) {
  
  if (n_offset > 0) {
    
    h_max = n_offset
    
    nx <- length(rgdp_data)
    new_nx <- nx - n_offset
    
    test_data <- subset(rgdp_data, start = new_nx + 1)
    dim(test_data) <- c(n_offset, 1)
    
    temp_yoy <- make_yoy_ts(rgdp_data, freq = freq)
    yoy_test_data <- subset(temp_yoy, start = new_nx - freq + 1)
    dim(yoy_test_data) <- c(n_offset, 1)
    
    rgdp_data <- subset(rgdp_data, end = new_nx)
    dim(rgdp_data) <- c(new_nx, 1)
    
  }


  use_demetra <- TRUE
  
  if (use_demetra) {
    demetra_output <- get_demetra_params(data_path)
    demetra_output_external <- get_demetra_params(external_data_path)
  }
  

  fit_arima_logrgdp_list_dem <- fit_arimas(
    y_ts = log(rgdp_data), order_list = demetra_output[["rgdp_order_list"]],
    this_arima_names = "rgdp")[[1]]

  
  fit_arima_logrgdp_auto_slow <- fit_arimas(
    y_ts = log(rgdp_data), include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]

  
  fit_arima_rgdp_auto_slow <- fit_arimas(
    y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
    my_lambda = 0, my_biasadj = FALSE
  )[[1]]

  
  fit_arima_rgdp_auto_slow_badj <- fit_arimas(
    y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
    my_lambda = 0, my_biasadj = TRUE
  )[[1]]

  
  fit_arima_yoyrgdp_auto_slow <- fit_arimas(
    y_ts = make_yoy_ts(rgdp_data, freq = freq), include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]

  
  
  fc_arima_logrgdp_list_dem <- forecast(fit_arima_logrgdp_list_dem, h = h_max)
  fc_arima_logrgdp_auto_slow <- forecast(fit_arima_logrgdp_auto_slow, h = h_max) 
  fc_arima_rgdp_auto_slow <- forecast(fit_arima_rgdp_auto_slow, h = h_max) 
  fc_arima_rgdp_auto_slow_badj <- forecast(fit_arima_rgdp_auto_slow_badj, h = h_max) 
  fc_arima_yoyrgdp_auto_slow <- forecast(fit_arima_yoyrgdp_auto_slow, h = h_max) 

  
  
  if (n_offset > 0) {
    
    acc_arima_rgdp_auto_slow <- accuracy(f = fc_arima_rgdp_auto_slow, x = test_data)
    acc_arima_rgdp_auto_slow_badj <- accuracy(f = fc_arima_rgdp_auto_slow_badj, x = test_data)
    acc_arima_logrgdp_auto_slow <- accuracy(fc_arima_logrgdp_auto_slow, x = test_data)
    acc_arima_logrgdp_list_dem <- accuracy(fc_arima_logrgdp_list_dem, x = test_data)
    acc_arima_yoyrgdp_auto_slow <- accuracy(fc_arima_yoyrgdp_auto_slow, x = test_data)
    
  } else {
    acc_arima_rgdp_auto_slow <- accuracy(f = fc_arima_rgdp_auto_slow)
    acc_arima_rgdp_auto_slow_badj <- accuracy(f = fc_arima_rgdp_auto_slow_badj)
    acc_arima_logrgdp_auto_slow <- accuracy(fc_arima_logrgdp_auto_slow)
    acc_arima_logrgdp_list_dem <- accuracy(fc_arima_logrgdp_list_dem)
    acc_arima_yoyrgdp_auto_slow <- accuracy(fc_arima_yoyrgdp_auto_slow)
  }

  
  acc_all <- cbind(acc_arima_rgdp_auto_slow, acc_arima_rgdp_auto_slow_badj,
                   acc_arima_logrgdp_auto_slow, acc_arima_logrgdp_list_dem,
                   acc_arima_yoyrgdp_auto_slow)
  

  difflog_fc_arima_logrgdp_list_dem <- fc_yoy_from_fc_level(fc_obj = fc_arima_logrgdp_list_dem, dodifflog = TRUE, isloglevel = TRUE)
  difflog_fc_arima_logrgdp_auto_slow <- fc_yoy_from_fc_level(fc_obj = fc_arima_logrgdp_auto_slow, dodifflog = TRUE, isloglevel = TRUE)
  yoy_fc_arima_logrgdp_list_dem <- fc_yoy_from_fc_level(fc_obj = fc_arima_logrgdp_list_dem, isloglevel = TRUE)
  yoy_fc_arima_logrgdp_auto_slow <- fc_yoy_from_fc_level(fc_obj = fc_arima_logrgdp_auto_slow, isloglevel = TRUE)
  yoy_fc_arima_rgdp_auto_slow <- fc_yoy_from_fc_level(fc_arima_rgdp_auto_slow)
  yoy_fc_arima_rgdp_auto_slow_badj <- fc_yoy_from_fc_level(fc_arima_rgdp_auto_slow_badj)

  
  p_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) + 
    autolayer(difflog_fc_arima_logrgdp_list_dem[["yoy_fc"]], series = "dl_logdm") + 
    autolayer(yoy_fc_arima_logrgdp_list_dem[["yoy_fc"]], series = "yoy_logdm") + 
    autolayer(fc_arima_yoyrgdp_auto_slow$mean, series = "direct_yoy") + 
    autolayer(difflog_fc_arima_logrgdp_auto_slow[["yoy_fc"]], series = "dl_logauto") + 
    autolayer(yoy_fc_arima_logrgdp_auto_slow[["yoy_fc"]], series = "yoy_logauto") + 
    autolayer(yoy_fc_arima_rgdp_auto_slow[["yoy_fc"]], series = "yoy_auto") + 
    autolayer(yoy_fc_arima_rgdp_auto_slow_badj[["yoy_fc"]], series = "yoy_auto_badj") + 
    coord_cartesian(xlim = c(2012, 2020))

  
  p_level <- autoplot(rgdp_data) + 
    autolayer(fc_arima_rgdp_auto_slow, PI = FALSE) + 
    autolayer(fc_arima_rgdp_auto_slow_badj, PI = FALSE) + 
    autolayer(exp(fc_arima_logrgdp_auto_slow$mean))  + 
    autolayer(exp(fc_arima_logrgdp_list_dem$mean)) +  
    coord_cartesian(xlim = c(2012, 2020))
  
  if (n_offset > 0) {
    p_yoy <- p_yoy + autolayer(yoy_test_data, series = "data")
    p_level <- p_level + autolayer(test_data, series = "data")
  }

  
  y_ave_logdem_ldiff <- difflog_fc_arima_logrgdp_list_dem[["yearly_average_yoy"]]
  y_ave_logdem <- yoy_fc_arima_logrgdp_list_dem[["yearly_average_yoy"]]
  y_ave_logauto_ldiff <- difflog_fc_arima_logrgdp_auto_slow[["yearly_average_yoy"]]
  y_ave_logauto <- yoy_fc_arima_logrgdp_auto_slow[["yearly_average_yoy"]]
  y_ave_auto <- yoy_fc_arima_rgdp_auto_slow[["yearly_average_yoy"]]
  y_ave_auto_badj <- yoy_fc_arima_rgdp_auto_slow_badj[["yearly_average_yoy"]]
  
  y_ave_all <- cbind(y_ave_logdem_ldiff, y_ave_logdem, y_ave_logauto_ldiff,
                     y_ave_logauto, y_ave_auto, y_ave_auto_badj)
  y_ave_all
  

  y_gt_logdem_ldiff <- difflog_fc_arima_logrgdp_list_dem[["yearly_growth_of_total"]]
  y_gt_logdem <- yoy_fc_arima_logrgdp_list_dem[["yearly_growth_of_total"]]
  y_gt_logauto_ldiff <- difflog_fc_arima_logrgdp_auto_slow[["yearly_growth_of_total"]]
  y_gt_logauto <- yoy_fc_arima_logrgdp_auto_slow[["yearly_growth_of_total"]]
  y_gt_auto <- yoy_fc_arima_rgdp_auto_slow[["yearly_growth_of_total"]]
  y_gt_auto_badj <- yoy_fc_arima_rgdp_auto_slow_badj[["yearly_growth_of_total"]]
  
  y_gt_all <- cbind(y_gt_logdem_ldiff, y_gt_logdem, y_gt_logauto_ldiff,
                    y_gt_logauto, y_gt_auto, y_gt_auto_badj)
  y_gt_all
  
  
  y_total_logdem_ldiff <- difflog_fc_arima_logrgdp_list_dem[["yearly_total"]]
  y_total_logdem <- yoy_fc_arima_logrgdp_list_dem[["yearly_total"]]
  y_total_logauto_ldiff <- difflog_fc_arima_logrgdp_auto_slow[["yearly_total"]]
  y_total_logauto <- yoy_fc_arima_logrgdp_auto_slow[["yearly_total"]]
  y_total_auto <- yoy_fc_arima_rgdp_auto_slow[["yearly_total"]]
  y_total_auto_badj <- yoy_fc_arima_rgdp_auto_slow_badj[["yearly_total"]]
  
  y_total_all <- cbind(y_total_logdem_ldiff, y_total_logdem, y_total_logauto_ldiff,
                       y_total_logauto, y_total_auto, y_total_auto_badj)
  y_total_all

  
  return(list(arima_of_log_y_demetra = fit_arima_logrgdp_list_dem,
              arima_of_log_y_autoarima = fit_arima_logrgdp_auto_slow,
              arima_of_y_autoarima = fit_arima_rgdp_auto_slow,
              arima_of_y_autoarima_badj = fit_arima_rgdp_auto_slow_badj,
              arima_of_yoy_y_autoarima = fit_arima_yoyrgdp_auto_slow,
              yearly_total_y = y_total_all,
              growth_of_yearly_total_y = y_gt_all,
              yearly_average_yoy_growth = y_ave_all,
              accuracy_measures = acc_all,
              plot_y_level = p_level,
              plot_y_yoy =  p_yoy))
  
}

univariate_rgpd_obj <- do_univariate_rgdp(rgdp_ts)

univariate_rgpd_obj_8 <- do_univariate_rgdp(rgdp_ts, n_offset = 8)

# n_offset <- 8
# rgdp_data <- rgdp_ts
# nx <- length(rgdp_data)
# new_nx <- nx - n_offset
# rgdp_data <- subset(rgdp_data, end = new_nx)
# dim(rgdp_data) <- c(new_nx, 1)
# 
# 
# fit_arima_logrgdp_list_dem <- fit_arimas(
#   y_ts = log(rgdp_data), order_list = demetra_output[["rgdp_order_list"]],
#   this_arima_names = "rgdp")[[1]]





#### --------------------------------------



rgdp_uncond_fc <- forecast(fit_arima_rgdp_list_dem[["rgdp"]], h = h_max)
rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean

fit_arima_monthly_list_dem <- fit_arimas(
  y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
  this_arima_names = monthly_names)

fit_arima_external_monthly_list_dem <- fit_arimas(
  y_ts = external_monthly_ts, order_list = demetra_output_external[["monthly_order_list"]],
  this_arima_names = external_monthly_names)

fit_arima_monthly_list_auto <- fit_arimas(
  y_ts = monthly_ts, auto = TRUE, do_stepwise = FALSE, do_approximation = TRUE,
  this_arima_names = monthly_names)

fit_arima_external_monthly_list_auto <- fit_arimas(
  y_ts = external_monthly_ts, auto = TRUE, do_stepwise = FALSE, do_approximation = TRUE,
  this_arima_names = external_monthly_names)




# monthly_param_list = NULL,
# external_monthly_param_list = NULL, do_auto = TRUE, 
# auto_stepwise = FALSE, auto_bias_adj = TRUE,
# do_box_cox = FALSE, auto_lambda = 0,
