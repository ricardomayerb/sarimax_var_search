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


# goo <- tk_tbl(external_monthly_ts)
# goo
# goodf <- as_data_frame(goo)



univariate_analysis <- function(rgdp_data, n_offset = 0, freq = 4, h_max = 8, 
                                tsCV_win = 40, do_auto_lambda = TRUE, 
                                do_demetra = TRUE, do_auto_biasadj = FALSE,
                                do_other_auto = FALSE, do_ets = FALSE) {
  

  
  if (n_offset > 0) {
    
    h_max = n_offset
    
    long_data <- rgdp_data
    
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
  
  yoy_data_ts <- make_yoy_ts(rgdp_data, freq = freq)
  
  
  p_base_lev <- autoplot(rgdp_data) + 
    labs(y = "GDP", x = "", title = "Real GDP forecast") +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  p_base_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) +
    labs(y = "GDP", x = "", title = "YoY growth of GDP") +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  if (n_offset > 0) {
    p_base_lev <- p_base_lev + 
      autolayer(test_data, series = "test data") 
    
    p_base_yoy <- p_base_yoy + 
      autolayer(yoy_test_data, series = "test data") 
  }
  

  
  if (do_demetra) {
    demetra_output <- get_demetra_params(data_path)
    demetra_output_external <- get_demetra_params(external_data_path)
  }
  
  e_list <- list()
  e_nw_list <- list()
  acc_all_training <- vector()
  acc_all_test <- vector()
  y_ave_all <- list()
  y_gt_all <- list()
  y_total_all <- list()
  fit_models <- list()
  
  
  # arima_1 : arima_logrgdp_list_dem, data is in logs, params from demetra
  # arima_2 : arima_logrgdp_auto_slow, data is in logs, auto.arima with stepwise FALSE and approximation FALSE
  # arima_3 : arima_rgdp_auto_slow, data is levels, auto.arima lambda = 0, with stepwise FALSE and approximation FALSE
  # arima_4 : arima_rgdp_auto_slow_badj, arima_3 with bias.adjust = TRUE
  # arima_5 : arima_yoyrgdp_auto_slow, arima_2 but data is in yoy growth
  
  
  if (do_demetra) {
    
    print("Fitting arima suggested by Demetra (model 1)")
    
    fit_arima_1 <- fit_arimas(
      y_ts = log(rgdp_data), order_list = demetra_output[["rgdp_order_list"]],
      this_arima_names = "rgdp")[[1]]
    
    fit_models <- c(fit_models, fit_arima_1)
    
    farima_1 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_1$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_1$arma)[c(3, 7, 4)],
                     include.drift = TRUE), h = h)
    }
    
    e_1 <- tsCV(log(rgdp_data), farima_1, h = 8, window = tsCV_win)
    e_1_nw <- tsCV(log(rgdp_data), farima_1, h = 8)
    e_list <- c(e_list, e_1)
    e_nw_list <- c(e_nw_list, e_1_nw)
    
    
    fc_arima_1 <- forecast(fit_arima_1, h = h_max)
    
    if (n_offset > 0) {
      acc_arima_1 <- accuracy(f = fc_arima_1, x = log(test_data))
      acc_all_training = rbind(acc_all_training, acc_arima_1[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_1[2,])

    } else {
      acc_arima_1 <- accuracy(f = fc_arima_1)
      acc_all_training = rbind(acc_all_training, acc_arima_1[1,])
    }
    
    difflog_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, dodifflog = TRUE, isloglevel = TRUE)
    yoy_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, dodifflog = FALSE, isloglevel = TRUE)
    
    y_ave_logdem_ldiff <- difflog_fc_arima_1[["yearly_average_yoy"]]
    y_ave_logdem <- yoy_fc_arima_1[["yearly_average_yoy"]]
    y_gt_logdem_ldiff <- difflog_fc_arima_1[["yearly_growth_of_total"]]
    y_gt_logdem <- yoy_fc_arima_1[["yearly_growth_of_total"]]
    y_total_logdem_ldiff <- difflog_fc_arima_1[["yearly_total"]]
    y_total_logdem <- yoy_fc_arima_1[["yearly_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_logdem))
    y_gt_all <- c(y_gt_all, list(y_gt_logdem))
    y_total_all <- c(y_total_all, list(y_total_logdem))

    y_tot_dm_2018 <- round(100*as.numeric(y_total_logdem["2018"]), digits = 2)
    y_tot_dm_2019 <- round(100*as.numeric(y_total_logdem["2019"]), digits = 2)
    y_ave_dm_2018 <- round(100*as.numeric(y_ave_logdem["2018"]), digits = 2)
    y_ave_dm_2019 <- round(100*as.numeric(y_ave_logdem["2019"]), digits = 2)
    gt_dm_2018 <- round(100*as.numeric(y_gt_logdem["2018"]), digits = 2)
    gt_dm_2019 <- round(100*as.numeric(y_gt_logdem["2019"]), digits = 2)
    
    p_base_lev <- p_base_lev + 
      autolayer(exp(fc_arima_1$mean), series = "demetra")
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_arima_1$yoy_fc, series = "demetra")
    
  }
  
  if (do_auto_lambda) {
    print("Fitting auto.arima (sw and apprx set to FALSE) to level data with lambda=0 (model 3)")
    
    fit_arima_3 <- fit_arimas(
      y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
      my_lambda = 0, my_biasadj = FALSE
    )[[1]]
    
    fit_models <- c(fit_models, fit_arima_3)
    
    farima_3 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_3$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_3$arma)[c(3, 7, 4)],
                     include.drift = TRUE, lambda = 0), h = h)
    }
    
    e_3 <- tsCV(rgdp_data, farima_3, h = 8, window = tsCV_win)
    e_3_nw <- tsCV(rgdp_data, farima_3, h = 8)
    e_list <- c(e_list, e_3)
    e_nw_list <- c(e_nw_list, e_3_nw)
    
    if (n_offset > 0) {
      acc_arima_3 <- accuracy(f = fc_arima_3, x = test_data)
      acc_all_training = rbind(acc_all_training, acc_arima_3[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_3[2,])
      
    } else {
      acc_arima_3 <- accuracy(f = fc_arima_3)
      acc_all_training = rbind(acc_all_training, acc_arima_3[1,])
    }
    
    
    yoy_fc_arima_3 <- fc_yoy_from_fc_level(fc_arima_3)
    
    y_ave_auto <- yoy_fc_arima_3[["yearly_average_yoy"]]
    y_gt_auto <- yoy_fc_arima_3[["yearly_growth_of_total"]]
    y_total_auto <- yoy_fc_arima_3[["yearly_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_auto))
    y_gt_all <- c(y_gt_all, list(y_gt_auto))
    y_total_all <- c(y_total_all, list(y_total_auto))

    gt_auto_2018 <- round(100*as.numeric(y_gt_auto["2018"]), digits = 2)
    gt_auto_2019 <- round(100*as.numeric(y_gt_auto["2019"]), digits = 2)
    y_tot_auto_2018 <- round(100*as.numeric(y_total_auto["2018"]), digits = 2)
    y_tot_auto_2019 <- round(100*as.numeric(y_total_auto["2019"]), digits = 2)
    y_ave_auto_2018 <- round(100*as.numeric(y_ave_auto["2018"]), digits = 2)
    y_ave_auto_2019 <- round(100*as.numeric(y_ave_auto["2019"]), digits = 2)
    
    p_base_lev <- p_base_lev + 
      autolayer(fc_arima_3, series = "auto", PI = FALSE)
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_arima_3$yoy_fc, series = "auto")
    
  }
  
  if (do_other_auto) {
    print("Fitting auto.arima to log data (sw and apprx set to FALSE) (model 2)")
    fit_arima_2 <- fit_arimas(
      y_ts = log(rgdp_data), include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]
    
    print("Fitting auto.arima (sw and apprx set to FALSE) to level data with lambda=0, bias-adjusted (model 4)")
    fit_arima_4 <- fit_arimas(
      y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
      my_lambda = 0, my_biasadj = TRUE
    )[[1]]
    
    print("Fitting auto.arima to YoY growth data (sw and apprx set to FALSE) (model 5)")
    fit_arima_5 <- fit_arimas(
      y_ts = make_yoy_ts(rgdp_data, freq = freq), include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]
    
    fit_models <- c(fit_models, fit_arima_2, fit_arima_4, fit_arima_5)
    
    farima_2 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_2$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_2$arma)[c(3, 7, 4)],
                     include.drift = TRUE), h = h)
    }
    
    farima_4 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_4$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_4$arma)[c(3, 7, 4)],
                     include.drift = TRUE, lambda = 0, biasadj = TRUE), h = h)
    }
    
    farima_5 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_5$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_5$arma)[c(3, 7, 4)],
                     include.drift = TRUE), h = h)
    }
    
    e_2 <- tsCV(log(rgdp_data), farima_2, h = 8, window = tsCV_win)
    e_4 <- tsCV(rgdp_data, farima_4, h = 8, window = tsCV_win)
    e_5 <- tsCV(yoy_data_ts, farima_5, h = 8, window = tsCV_win)
    
    e_2_nw <- tsCV(log(rgdp_data), farima_2, h = 8)
    e_4_nw <- tsCV(rgdp_data, farima_4, h = 8)
    e_5_nw <- tsCV(yoy_data_ts, farima_5, h = 8)
    
    e_list <- c(e_list, e_2, e_4, e_5)
    e_nw_list <- c(e_nw_list, e_2_nw, e_4_nw, e_5_nw)
    
    fc_arima_2 <- forecast(fit_arima_2, h = h_max) 
    fc_arima_4 <- forecast(fit_arima_4, h = h_max, lambda = 0, biasadj = TRUE) 
    fc_arima_5 <- forecast(fit_arima_5, h = h_max) 
    
    if (n_offset > 0) {
      acc_arima_2 <- accuracy(f = fc_arima_2, x = log(test_data))
      acc_arima_4 <- accuracy(f = fc_arima_4, x = test_data)
      acc_arima_5 <- accuracy(f = fc_arima_5, x = yoy_test_data)
      
      acc_all_training = rbind(acc_all_training, acc_arima_2[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_2[2,])
      acc_all_training = rbind(acc_all_training, acc_arima_4[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_4[2,])
      acc_all_training = rbind(acc_all_training, acc_arima_5[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_5[2,])
      
    } else {
      acc_arima_2 <- accuracy(f = fc_arima_2)
      acc_arima_4 <- accuracy(f = fc_arima_4)
      acc_arima_5 <- accuracy(f = fc_arima_5)
      
      acc_all_training = rbind(acc_all_training, acc_arima_2[1,])
      acc_all_training = rbind(acc_all_training, acc_arima_4[1,])
      acc_all_training = rbind(acc_all_training, acc_arima_5[1,])
    }
    

    difflog_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, dodifflog = TRUE, isloglevel = TRUE)
    yoy_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, isloglevel = TRUE)
    yoy_fc_arima_4 <- fc_yoy_from_fc_level(fc_arima_4)
    
    y_ave_logauto_ldiff <- difflog_fc_arima_2[["yearly_average_yoy"]]
    y_ave_logauto <- yoy_fc_arima_2[["yearly_average_yoy"]]
    y_ave_auto_badj <- yoy_fc_arima_4[["yearly_average_yoy"]]
    
    y_gt_logauto_ldiff <- difflog_fc_arima_2[["yearly_growth_of_total"]]
    y_gt_logauto <- yoy_fc_arima_2[["yearly_growth_of_total"]]
    y_gt_auto_badj <- yoy_fc_arima_4[["yearly_growth_of_total"]]
    
    y_total_logauto_ldiff <- difflog_fc_arima_2[["yearly_total"]]
    y_total_logauto <- yoy_fc_arima_2[["yearly_total"]]
    y_total_auto_badj <- yoy_fc_arima_4[["yearly_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_logauto), list(y_ave_auto_badj))
    y_gt_all <- c(y_gt_all, list(y_gt_logauto), list(y_gt_auto_badj))
    y_total_all <- c(y_total_all, list(y_total_logauto), list(y_total_auto_badj))
    
    p_base_lev <- p_base_lev + 
      autolayer(exp(fc_arima_2$mean), series = "auto of log") + 
      autolayer(fc_arima_4, series = "auto with badj")
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_arima_2$yoy_fc, series = "auto of log") + 
      autolayer(yoy_fc_arima_4$yoy_fc, series = "auto with badj") + 
      autolayer(fc_arima_5, series = "auto of yoy", PI = FALSE)
  }
  
  if (do_ets) {
    print("Fitting auto ETS to level data (model 6)")
    fit_ets_6 <- ets(rgdp_data)
    
    print("Fitting auto ETS to level data, with lambda=0 and bias-adjusted (model 7)")
    fit_ets_7 <- ets(rgdp_data, lambda = 0, biasadj = TRUE)
    
    fit_models <- c(fit_models, fit_ets_6, fit_ets_7)
    
    fets_6 <- function(x, h) {
      forecast(ets(x), h = h)
    }
    
    fets_7 <- function(x, h) {
      forecast(ets(x, lambda = 0, biasadj = TRUE), h = h)
    }
  
    e_6 <- tsCV(rgdp_ts, fets_6, h = 8, window = tsCV_win)
    e_7 <- tsCV(rgdp_ts, fets_7, h = 8, window = tsCV_win)
    e_6nw <- tsCV(rgdp_ts, fets_6, h = 8)
    e_7nw <- tsCV(rgdp_ts, fets_7, h = 8)
    
    e_list <- c(e_list, e_6, e_7)
    e_nw_list <- c(e_nw_list, e_6_nw, e_7_nw)
    
    fc_ets_6 <- forecast(fit_ets_6, h = h_max) 
    fc_ets_7 <- forecast(fit_ets_7, h = h_max) 
    
    if (n_offset > 0) {
      acc_ets_6 <- accuracy(f = fc_ets_6, x = test_data)
      acc_ets_7 <- accuracy(f = fc_ets_7, x = test_data)
      
      acc_all_training = rbind(acc_all_training, acc_ets_6[1,])
      acc_all_test = rbind(acc_all_test, acc_ets_6[2,])
      acc_all_training = rbind(acc_all_training, acc_ets_7[1,])
      acc_all_test = rbind(acc_all_test, acc_ets_7[2,])
      
    } else {
      acc_ets_6 <- accuracy(f = fc_ets_6)
      acc_ets_7 <- accuracy(f = fc_ets_7)
      
      acc_all_training = rbind(acc_all_training, acc_ets_6[1,])
      acc_all_training = rbind(acc_all_training, acc_ets_7[1,])
    }
    
    yoy_fc_ets_6 <- fc_yoy_from_fc_level(fc_ets_6)
    yoy_fc_ets_7 <- fc_yoy_from_fc_level(fc_ets_7)
    
    y_ave_ets <- yoy_fc_ets_6[["yearly_average_yoy"]]
    y_ave_ets_badj <- yoy_fc_ets_7[["yearly_average_yoy"]]
    
    y_total_ets <- yoy_fc_ets_6[["yearly_total"]]
    y_total_ets_badj <- yoy_fc_ets_7[["yearly_total"]]
    
    y_gt_ets <- yoy_fc_ets_6[["yearly_growth_of_total"]]
    y_gt_ets_badj <- yoy_fc_ets_7[["yearly_growth_of_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_ets), list(y_ave_ets_badj))
    y_gt_all <- c(y_gt_all, list(y_gt_ets), list(y_gt_ets_badj))
    y_total_all <- c(y_total_all, list(y_total_ets), list(y_total_ets_badj))
    
    p_base_lev <- p_base_lev + 
      autolayer(fc_ets_6, series = "ets", PI = FALSE) + 
      autolayer(fc_ets_7, series = "ets lambda0", PI = FALSE)
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_ets_6$yoy_fc, series = "ets") + 
      autolayer(yoy_fc_ets_7$yoy_fc, series = "ets lambda0")
    
  }
  
  # print(paste("Computing CV errors (tsCV)) for each model, with and without a rolling window of", tsCV_win))
  
  cv_rmse <- map(e_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
  cv_mae <- map(e_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)
  
  cv_rmse_nw <- map(e_nw_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
  cv_mae_nw <- map(e_nw_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)  

  return(list(fit_models,
              yearly_total_y = y_total_all,
              growth_of_yearly_total_y = y_gt_all,
              yearly_average_yoy_growth = y_ave_all,
              accuracy_measures_training_set = acc_all_training,
              accuracy_measures_test_set = acc_all_test,
              plot_levels = p_base_lev,
              plot_yoy = p_base_yoy,
              cv_rmse = cv_rmse,
              cv_rmse_no_rolling_window = cv_rmse_nw,
              cv_mae = cv_mae,
              cv_mae_no_rolling_window = cv_mae_nw)
         )
  
}

tic()
univariate_rgpd_obj <- univariate_analysis(rgdp_ts, do_demetra = TRUE, do_auto_lambda = FALSE)
toc()







#### -------- foo -------

rgdp_data <- rgdp_ts
n_offset <- 8
freq <-  4
h_max <- 8
tsCV_win <- 40
do_auto_lambda = TRUE
do_demetra = TRUE
do_auto_biasadj = TRUE
do_other_auto = TRUE
do_ets = TRUE


if (n_offset > 0) {
  
  h_max = n_offset
  
  long_data <- rgdp_data
  
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

yoy_data_ts <- make_yoy_ts(rgdp_data, freq = freq)


p_base_lev <- autoplot(rgdp_data) + 
  labs(y = "GDP", x = "", title = "Real GDP forecast") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p_base_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) +
  labs(y = "GDP", x = "", title = "YoY growth of GDP") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

if (n_offset > 0) {
  p_base_lev <- p_base_lev + 
    autolayer(test_data, series = "test data") 
  
  p_base_yoy <- p_base_yoy + 
    autolayer(yoy_test_data, series = "test data") 
}



if (do_demetra) {
  demetra_output <- get_demetra_params(data_path)
  demetra_output_external <- get_demetra_params(external_data_path)
}

e_list <- NULL
e_names <-  c()
e_nw_list <- NULL
e_nw_names <-  c()
acc_all_training <- vector()
acc_all_test <- vector()
y_ave_all <- list()
y_gt_all <- list()
y_total_all <- list()
fit_models <- list()


# arima_1 : arima_logrgdp_list_dem, data is in logs, params from demetra
# arima_2 : arima_logrgdp_auto_slow, data is in logs, auto.arima with stepwise FALSE and approximation FALSE
# arima_3 : arima_rgdp_auto_slow, data is levels, auto.arima lambda = 0, with stepwise FALSE and approximation FALSE
# arima_4 : arima_rgdp_auto_slow_badj, arima_3 with bias.adjust = TRUE
# arima_5 : arima_yoyrgdp_auto_slow, arima_2 but data is in yoy growth


if (do_demetra) {
  
  print("Fitting arima suggested by Demetra (model 1)")
  
  fit_arima_1 <- fit_arimas(
    y_ts = log(rgdp_data), order_list = demetra_output[["rgdp_order_list"]],
    this_arima_names = "rgdp")[[1]]
  
  fit_models <- c(fit_models, fit_arima_1)
  
  farima_1 <- function(x, h) {
    forecast(Arima(x, order = (fit_arima_1$arma)[c(1, 6, 2)],
                   seasonal = (fit_arima_1$arma)[c(3, 7, 4)],
                   include.drift = TRUE), h = h)
  }
  
  e_1 <- tsCV(log(rgdp_data), farima_1, h = 8, window = tsCV_win)
  e_1_nw <- tsCV(log(rgdp_data), farima_1, h = 8)
  
  if(is.null(e_list)) {
    e_list <- list(e_1)
  } else {
    e_list[[length(e_list) + 1]] <-  e_1
  }
  
  if(is.null(e_nw_list)) {
    e_nw_list <- list(e_1_nw)
  } else {
    e_nw_list[[length(e_nw_list) + 1]] <-  e_1_nw
  }
  
  e_names <- c(e_names, "dm")
  names(e_list) <- e_names
  e_nw_names <- c(e_nw_names, "dm")
  names(e_nw_list) <- e_nw_names
  
  fc_arima_1 <- forecast(fit_arima_1, h = h_max)
  
  if (n_offset > 0) {
    acc_arima_1 <- accuracy(f = fc_arima_1, x = log(test_data))
    acc_all_training = rbind(acc_all_training, acc_arima_1[1,])
    acc_all_test = rbind(acc_all_test, acc_arima_1[2,])
    
  } else {
    acc_arima_1 <- accuracy(f = fc_arima_1)
    acc_all_training = rbind(acc_all_training, acc_arima_1[1,])
  }
  
  difflog_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, dodifflog = TRUE, isloglevel = TRUE)
  yoy_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, dodifflog = FALSE, isloglevel = TRUE)
  
  y_ave_logdem_ldiff <- difflog_fc_arima_1[["yearly_average_yoy"]]
  y_ave_logdem <- yoy_fc_arima_1[["yearly_average_yoy"]]
  y_gt_logdem_ldiff <- difflog_fc_arima_1[["yearly_growth_of_total"]]
  y_gt_logdem <- yoy_fc_arima_1[["yearly_growth_of_total"]]
  y_total_logdem_ldiff <- difflog_fc_arima_1[["yearly_total"]]
  y_total_logdem <- yoy_fc_arima_1[["yearly_total"]]
  
  y_ave_all <- c(y_ave_all, list(y_ave_logdem))
  y_gt_all <- c(y_gt_all, list(y_gt_logdem))
  y_total_all <- c(y_total_all, list(y_total_logdem))
  
  y_tot_dm_2018 <- round(100*as.numeric(y_total_logdem["2018"]), digits = 2)
  y_tot_dm_2019 <- round(100*as.numeric(y_total_logdem["2019"]), digits = 2)
  y_ave_dm_2018 <- round(100*as.numeric(y_ave_logdem["2018"]), digits = 2)
  y_ave_dm_2019 <- round(100*as.numeric(y_ave_logdem["2019"]), digits = 2)
  gt_dm_2018 <- round(100*as.numeric(y_gt_logdem["2018"]), digits = 2)
  gt_dm_2019 <- round(100*as.numeric(y_gt_logdem["2019"]), digits = 2)
  
  p_base_lev <- p_base_lev + 
    autolayer(exp(fc_arima_1$mean), series = "demetra")
  
  p_base_yoy <-  p_base_yoy + 
    autolayer(yoy_fc_arima_1$yoy_fc, series = "demetra")
  
}

if (do_auto_lambda) {
  print("Fitting auto.arima (sw and apprx set to FALSE) to level data with lambda=0 (model 3)")
  
  fit_arima_3 <- fit_arimas(
    y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
    my_lambda = 0, my_biasadj = FALSE
  )[[1]]
  
  fit_models <- c(fit_models, fit_arima_3)
  
  farima_3 <- function(x, h) {
    forecast(Arima(x, order = (fit_arima_3$arma)[c(1, 6, 2)],
                   seasonal = (fit_arima_3$arma)[c(3, 7, 4)],
                   include.drift = TRUE, lambda = 0), h = h)
  }
  
  e_3 <- tsCV(rgdp_data, farima_3, h = 8, window = tsCV_win)
  e_3_nw <- tsCV(rgdp_data, farima_3, h = 8)
  
  if(is.null(e_list)) {
    e_list <- list(e_3)
  } else {
    e_list[[length(e_list) + 1]] <-  e_3
  }
  
  if(is.null(e_nw_list)) {
    e_nw_list <- list(e_3_nw)
  } else {
    e_nw_list[[length(e_nw_list) + 1]] <-  e_3_nw
  }
  
  e_names <- c(e_names, "au")
  names(e_list) <- e_names
  e_nw_names <- c(e_nw_names, "au")
  names(e_nw_list) <- e_nw_names
  
  fc_arima_3 <- forecast(fit_arima_3, h = h_max)
  
  if (n_offset > 0) {
    acc_arima_3 <- accuracy(f = fc_arima_3, x = test_data)
    acc_all_training = rbind(acc_all_training, acc_arima_3[1,])
    acc_all_test = rbind(acc_all_test, acc_arima_3[2,])
    
  } else {
    acc_arima_3 <- accuracy(f = fc_arima_3)
    acc_all_training = rbind(acc_all_training, acc_arima_3[1,])
  }
  
  
  yoy_fc_arima_3 <- fc_yoy_from_fc_level(fc_arima_3)
  
  y_ave_auto <- yoy_fc_arima_3[["yearly_average_yoy"]]
  y_gt_auto <- yoy_fc_arima_3[["yearly_growth_of_total"]]
  y_total_auto <- yoy_fc_arima_3[["yearly_total"]]
  
  y_ave_all <- c(y_ave_all, list(y_ave_auto))
  y_gt_all <- c(y_gt_all, list(y_gt_auto))
  y_total_all <- c(y_total_all, list(y_total_auto))
  
  gt_auto_2018 <- round(100*as.numeric(y_gt_auto["2018"]), digits = 2)
  gt_auto_2019 <- round(100*as.numeric(y_gt_auto["2019"]), digits = 2)
  y_tot_auto_2018 <- round(100*as.numeric(y_total_auto["2018"]), digits = 2)
  y_tot_auto_2019 <- round(100*as.numeric(y_total_auto["2019"]), digits = 2)
  y_ave_auto_2018 <- round(100*as.numeric(y_ave_auto["2018"]), digits = 2)
  y_ave_auto_2019 <- round(100*as.numeric(y_ave_auto["2019"]), digits = 2)
  
  p_base_lev <- p_base_lev + 
    autolayer(fc_arima_3, series = "auto", PI = FALSE)
  
  p_base_yoy <-  p_base_yoy + 
    autolayer(yoy_fc_arima_3$yoy_fc, series = "auto")
  
}

if (do_other_auto) {
  print("Fitting auto.arima to log data (sw and apprx set to FALSE) (model 2)")
  fit_arima_2 <- fit_arimas(
    y_ts = log(rgdp_data), include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]
  
  print("Fitting auto.arima (sw and apprx set to FALSE) to level data with lambda=0, bias-adjusted (model 4)")
  fit_arima_4 <- fit_arimas(
    y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
    my_lambda = 0, my_biasadj = TRUE
  )[[1]]
  
  print("Fitting auto.arima to YoY growth data (sw and apprx set to FALSE) (model 5)")
  fit_arima_5 <- fit_arimas(
    y_ts = make_yoy_ts(rgdp_data, freq = freq), include.constant = TRUE, auto = TRUE, 
    do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]
  
  fit_models <- c(fit_models, fit_arima_2, fit_arima_4, fit_arima_5)
  
  farima_2 <- function(x, h) {
    forecast(Arima(x, order = (fit_arima_2$arma)[c(1, 6, 2)],
                   seasonal = (fit_arima_2$arma)[c(3, 7, 4)],
                   include.drift = TRUE), h = h)
  }
  
  farima_4 <- function(x, h) {
    forecast(Arima(x, order = (fit_arima_4$arma)[c(1, 6, 2)],
                   seasonal = (fit_arima_4$arma)[c(3, 7, 4)],
                   include.drift = TRUE, lambda = 0, biasadj = TRUE), h = h)
  }
  
  farima_5 <- function(x, h) {
    forecast(Arima(x, order = (fit_arima_5$arma)[c(1, 6, 2)],
                   seasonal = (fit_arima_5$arma)[c(3, 7, 4)],
                   include.drift = TRUE), h = h)
  }
  
  e_2 <- tsCV(log(rgdp_data), farima_2, h = 8, window = tsCV_win)
  e_4 <- tsCV(rgdp_data, farima_4, h = 8, window = tsCV_win)
  e_5 <- tsCV(yoy_data_ts, farima_5, h = 8, window = tsCV_win)
  
  e_2_nw <- tsCV(log(rgdp_data), farima_2, h = 8)
  e_4_nw <- tsCV(rgdp_data, farima_4, h = 8)
  e_5_nw <- tsCV(yoy_data_ts, farima_5, h = 8)
  
  if(is.null(e_list)) {
    e_list <- list(e_2)
    e_list[[2]] <- e_4
    e_list[[3]] <- e_5
  } else {
    e_list[[length(e_list) + 1]] <-  e_2
    e_list[[length(e_list) + 2]] <-  e_4
    e_list[[length(e_list) + 3]] <-  e_5
  }
  
  if(is.null(e_nw_list)) {
    e_nw_list <- list(e_2_nw)
    e_nw_list[[2]] <- e_4_nw
    e_nw_list[[3]] <- e_5_nw
  } else {
    e_nw_list[[length(e_nw_list) + 1]] <-  e_2_nw
    e_nw_list[[length(e_nw_list) + 2]] <-  e_4_nw
    e_nw_list[[length(e_nw_list) + 3]] <-  e_5_nw
  }
  
  e_names <- c(e_names, c("lau", "aubj", "yoyau"))
  names(e_list) <- e_names
  e_nw_names <- c(e_nw_names, c("lau", "aubj", "yoyau"))
  names(e_nw_list) <- e_nw_names
  
  
  fc_arima_2 <- forecast(fit_arima_2, h = h_max) 
  fc_arima_4 <- forecast(fit_arima_4, h = h_max, lambda = 0, biasadj = TRUE) 
  fc_arima_5 <- forecast(fit_arima_5, h = h_max) 
  
  if (n_offset > 0) {
    acc_arima_2 <- accuracy(f = fc_arima_2, x = log(test_data))
    acc_arima_4 <- accuracy(f = fc_arima_4, x = test_data)
    acc_arima_5 <- accuracy(f = fc_arima_5, x = yoy_test_data)
    
    acc_all_training = rbind(acc_all_training, acc_arima_2[1,])
    acc_all_test = rbind(acc_all_test, acc_arima_2[2,])
    acc_all_training = rbind(acc_all_training, acc_arima_4[1,])
    acc_all_test = rbind(acc_all_test, acc_arima_4[2,])
    acc_all_training = rbind(acc_all_training, acc_arima_5[1,])
    acc_all_test = rbind(acc_all_test, acc_arima_5[2,])
    
  } else {
    acc_arima_2 <- accuracy(f = fc_arima_2)
    acc_arima_4 <- accuracy(f = fc_arima_4)
    acc_arima_5 <- accuracy(f = fc_arima_5)
    
    acc_all_training = rbind(acc_all_training, acc_arima_2[1,])
    acc_all_training = rbind(acc_all_training, acc_arima_4[1,])
    acc_all_training = rbind(acc_all_training, acc_arima_5[1,])
  }
  
  
  difflog_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, dodifflog = TRUE, isloglevel = TRUE)
  yoy_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, isloglevel = TRUE)
  yoy_fc_arima_4 <- fc_yoy_from_fc_level(fc_arima_4)
  
  y_ave_logauto_ldiff <- difflog_fc_arima_2[["yearly_average_yoy"]]
  y_ave_logauto <- yoy_fc_arima_2[["yearly_average_yoy"]]
  y_ave_auto_badj <- yoy_fc_arima_4[["yearly_average_yoy"]]
  
  y_gt_logauto_ldiff <- difflog_fc_arima_2[["yearly_growth_of_total"]]
  y_gt_logauto <- yoy_fc_arima_2[["yearly_growth_of_total"]]
  y_gt_auto_badj <- yoy_fc_arima_4[["yearly_growth_of_total"]]
  
  y_total_logauto_ldiff <- difflog_fc_arima_2[["yearly_total"]]
  y_total_logauto <- yoy_fc_arima_2[["yearly_total"]]
  y_total_auto_badj <- yoy_fc_arima_4[["yearly_total"]]
  
  y_ave_all <- c(y_ave_all, list(y_ave_logauto), list(y_ave_auto_badj))
  y_gt_all <- c(y_gt_all, list(y_gt_logauto), list(y_gt_auto_badj))
  y_total_all <- c(y_total_all, list(y_total_logauto), list(y_total_auto_badj))
  
  p_base_lev <- p_base_lev + 
    autolayer(exp(fc_arima_2$mean), series = "auto of log") + 
    autolayer(fc_arima_4, series = "auto with badj")
  
  p_base_yoy <-  p_base_yoy + 
    autolayer(yoy_fc_arima_2$yoy_fc, series = "auto of log") + 
    autolayer(yoy_fc_arima_4$yoy_fc, series = "auto with badj") + 
    autolayer(fc_arima_5, series = "auto of yoy", PI = FALSE)
}

if (do_ets) {
  print("Fitting auto ETS to level data (model 6)")
  fit_ets_6 <- ets(rgdp_data)
  
  print("Fitting auto ETS to level data, with lambda=0 and bias-adjusted (model 7)")
  fit_ets_7 <- ets(rgdp_data, lambda = 0, biasadj = TRUE)
  
  fit_models <- c(fit_models, fit_ets_6, fit_ets_7)
  
  fets_6 <- function(x, h) {
    forecast(ets(x), h = h)
  }
  
  fets_7 <- function(x, h) {
    forecast(ets(x, lambda = 0, biasadj = TRUE), h = h)
  }
  
  e_6 <- tsCV(rgdp_ts, fets_6, h = 8, window = tsCV_win)
  e_7 <- tsCV(rgdp_ts, fets_7, h = 8, window = tsCV_win)
  e_6_nw <- tsCV(rgdp_ts, fets_6, h = 8)
  e_7_nw <- tsCV(rgdp_ts, fets_7, h = 8)
  
  if(is.null(e_list)) {
    e_list <- list(e_6)
    e_list[[2]] <- e_7
  } else {
    e_list[[length(e_list) + 1]] <-  e_6
    e_list[[length(e_list) + 2]] <-  e_7
  }
  
  if(is.null(e_nw_list)) {
    e_nw_list <- list(e_6_nw)
    e_nw_list[[2]] <- e_7_nw
  } else {
    e_nw_list[[length(e_nw_list) + 1]] <-  e_6_nw
    e_nw_list[[length(e_nw_list) + 2]] <-  e_7_nw
  }
  
  e_names <- c(e_names, c("ets", "etsbj"))
  names(e_list) <- e_names
  e_nw_names <- c(e_nw_names, c("ets", "etsbj"))
  names(e_nw_list) <- e_nw_names
  
  fc_ets_6 <- forecast(fit_ets_6, h = h_max) 
  fc_ets_7 <- forecast(fit_ets_7, h = h_max) 
  
  if (n_offset > 0) {
    acc_ets_6 <- accuracy(f = fc_ets_6, x = test_data)
    acc_ets_7 <- accuracy(f = fc_ets_7, x = test_data)
    
    acc_all_training = rbind(acc_all_training, acc_ets_6[1,])
    acc_all_test = rbind(acc_all_test, acc_ets_6[2,])
    acc_all_training = rbind(acc_all_training, acc_ets_7[1,])
    acc_all_test = rbind(acc_all_test, acc_ets_7[2,])
    
  } else {
    acc_ets_6 <- accuracy(f = fc_ets_6)
    acc_ets_7 <- accuracy(f = fc_ets_7)
    
    acc_all_training = rbind(acc_all_training, acc_ets_6[1,])
    acc_all_training = rbind(acc_all_training, acc_ets_7[1,])
  }
  
  yoy_fc_ets_6 <- fc_yoy_from_fc_level(fc_ets_6)
  yoy_fc_ets_7 <- fc_yoy_from_fc_level(fc_ets_7)
  
  y_ave_ets <- yoy_fc_ets_6[["yearly_average_yoy"]]
  y_ave_ets_badj <- yoy_fc_ets_7[["yearly_average_yoy"]]
  
  y_total_ets <- yoy_fc_ets_6[["yearly_total"]]
  y_total_ets_badj <- yoy_fc_ets_7[["yearly_total"]]
  
  y_gt_ets <- yoy_fc_ets_6[["yearly_growth_of_total"]]
  y_gt_ets_badj <- yoy_fc_ets_7[["yearly_growth_of_total"]]
  
  y_ave_all <- c(y_ave_all, list(y_ave_ets), list(y_ave_ets_badj))
  y_gt_all <- c(y_gt_all, list(y_gt_ets), list(y_gt_ets_badj))
  y_total_all <- c(y_total_all, list(y_total_ets), list(y_total_ets_badj))
  
  p_base_lev <- p_base_lev + 
    autolayer(fc_ets_6, series = "ets", PI = FALSE) + 
    autolayer(fc_ets_7, series = "ets lambda0", PI = FALSE)
  
  p_base_yoy <-  p_base_yoy + 
    autolayer(yoy_fc_ets_6$yoy_fc, series = "ets") + 
    autolayer(yoy_fc_ets_7$yoy_fc, series = "ets lambda0")
  
}

# print(paste("Computing CV errors (tsCV)) for each model, with and without a rolling window of", tsCV_win))

cv_rmse <- map(e_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
cv_mae <- map(e_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)

cv_rmse_nw <- map(e_nw_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
cv_mae_nw <- map(e_nw_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)  

row.names(cv_mae) <- e_names
row.names(cv_rmse) <- e_names
row.names(cv_mae_nw) <- e_nw_names
row.names(cv_rmse_nw) <- e_nw_names

y_ave_all <- reduce(y_ave_all, cbind)
y_gt_all <- reduce(y_gt_all, cbind)
y_total_all <- reduce(y_total_all, cbind)


#### ------ hoo


# ari_1 <- univariate_rgpd_obj$arima_of_log_y_demetra
# ari_2 <- univariate_rgpd_obj$arima_of_log_y_autoarima
# 
# ari_3 <- univariate_rgpd_obj$arima_of_y_autoarima
# ari_4 <- univariate_rgpd_obj$arima_of_y_autoarima_badj
# 
# ari_5 <- univariate_rgpd_obj$arima_of_yoy_y_autoarima
# 
# 
# 
# univariate_rgpd_obj$yearly_total_y
# 
# univariate_rgpd_obj$yearly_average_yoy_growth
# 
# univariate_rgpd_obj$growth_of_yearly_total_y
# 
# gr_dm <- (univariate_rgpd_obj$growth_of_yearly_total_y)[,1]
# gr_dm_2018 <- round(100*as.numeric(gr_dm["2018"]), digits = 2)
# gr_dm_2019 <- round(100*as.numeric(gr_dm["2019"]), digits = 2)
# gr_dm_2018
# gr_dm_2019
# 
# gr_au <- (univariate_rgpd_obj$growth_of_yearly_total_y)[,2]
# gr_au_2018 <- round(100*as.numeric(gr_au["2018"]), digits = 2)
# gr_au_2019 <- round(100*as.numeric(gr_au["2019"]), digits = 2)
# gr_au_2018
# gr_au_2019
# 
# ave_dm <- (univariate_rgpd_obj$yearly_average_yoy_growth)[,2]
# ave_dm_2018 <- round(100*as.numeric(ave_dm["2018"]), digits = 2)
# ave_dm_2019 <- round(100*as.numeric(ave_dm["2019"]), digits = 2)
# ave_dm_2018
# ave_dm_2019
# 
# ave_au <- (univariate_rgpd_obj$yearly_average_yoy_growth)[,4]
# ave_au_2018 <- round(100*as.numeric(ave_au["2018"]), digits = 2)
# ave_au_2019 <- round(100*as.numeric(ave_au["2019"]), digits = 2)
# ave_au_2018
# ave_au_2019
# 
# tot_dm <- (univariate_rgpd_obj$yearly_total_y)[,1]
# tot_dm_2018 <- round(100*as.numeric(tot_dm["2018"]), digits = 0)
# tot_dm_2019 <- round(100*as.numeric(tot_dm["2019"]), digits = 0)
# tot_dm_2018
# tot_dm_2019
# 
# tot_au <- (univariate_rgpd_obj$yearly_total_y)[,2]
# tot_au_2018 <- round(100*as.numeric(tot_au["2018"]), digits = 0)
# tot_au_2019 <- round(100*as.numeric(tot_au["2019"]), digits = 0)
# tot_au_2018
# tot_au_2019
# 
# 
# 
# p_da_lev <- univariate_rgpd_obj$plot_dm_vs_auto_lev 
# an_p_da_lev_2018 <- paste("2018: dm =", tot_dm_2018, ", auto =", tot_au_2018)
# an_p_da_lev_2019 <- paste("2019: dm =", tot_dm_2019, ", auto =", tot_au_2019)
# p_da_lev <- p_da_lev + 
#   annotate("text", x = 2003, y = 40000, label = an_p_da_lev_2018, size = 3) + 
#   annotate("text", x = 2003, y = 35000, label = an_p_da_lev_2019, size = 3) 
# p_da_lev
# 
# 
# p_da_yoy <- univariate_rgpd_obj$plot_dm_vs_auto_yoy
# an_p_da_yoy_2018 <- paste("2018: dm =", ave_dm_2018, ", auto =", ave_au_2018)
# an_p_da_yoy_2019 <- paste("2019: dm =", ave_dm_2019, ", auto =", ave_au_2019)
# p_da_yoy <- p_da_yoy + 
#   annotate("text", x = 2015, y = -0.02, label = an_p_da_yoy_2018, size = 3) + 
#   annotate("text", x = 2015, y = -0.04, label = an_p_da_yoy_2019, size = 3) 
# p_da_yoy
# 
# p_biasornot_yoy <- univariate_rgpd_obj$plot_badj_vs_not_yoy
# p_biasornot_lev <- univariate_rgpd_obj$plot_badj_vs_not_lev
# 
# p_lam_vs_explog_yoy <- univariate_rgpd_obj$plot_lam_vs_explog_yoy
# p_lam_vs_explog_lev <- univariate_rgpd_obj$plot_lam_vs_explog_lev
# 
# p_dir_vs_indir_yoy <- univariate_rgpd_obj$plot_yoy_indirect_vs_direct
# 
# 
# p_da_lev + 
#   coord_cartesian(xlim = c(2015, 2020))
# 
# p_da_yoy + 
#   coord_cartesian(xlim = c(2015, 2020))
# 
# 
# p_biasornot_lev + 
#   coord_cartesian(xlim = c(2015, 2020))
# 
# p_biasornot_yoy + 
#   coord_cartesian(xlim = c(2015, 2020))
# 
# 
# p_lam_vs_explog_yoy + 
#   coord_cartesian(xlim = c(2015, 2020))
# 
# p_lam_vs_explog_lev + 
#   coord_cartesian(xlim = c(2015, 2020))
# 
# 
# p_dir_vs_indir_yoy + 
#   coord_cartesian(xlim = c(2015, 2020))
# 
# 
# # tic()
# # univariate_rgpd_obj <- univariate_analysis(rgdp_ts, models = "all")
# # toc()
# 
# 
# univariate_rgpd_obj_4 <- univariate_analysis(rgdp_ts, n_offset = 4)
# 
# univariate_rgpd_obj_8 <- univariate_analysis(rgdp_ts, n_offset = 8)
# 
# univariate_rgpd_obj_12 <- univariate_analysis(rgdp_ts, n_offset = 12)
# 
# p_yoy <- univariate_rgpd_obj$plot_y_yoy
# print(p_yoy)
# p_lev <- univariate_rgpd_obj$plot_y_level
# print(p_lev)
# 
# 
# p_yoy4 <- univariate_rgpd_obj_4$plot_y_yoy
# 
# p_yoy4 <- p_yoy4
# print(p_yoy4)
# 
# 
# p_lev4 <- univariate_rgpd_obj_4$plot_y_level
# print(p_lev4)
# 
# 
# p_yoy8 <- univariate_rgpd_obj_8$plot_y_yoy
# print(p_yoy8)
# p_lev8 <- univariate_rgpd_obj_8$plot_y_level
# print(p_lev8)
# 
# p_yoy12 <- univariate_rgpd_obj_12$plot_y_yoy
# print(p_yoy12)
# p_lev12 <- univariate_rgpd_obj_12$plot_y_level + 
#   tidyquant::theme_tq()
# print(p_lev12)
# 
