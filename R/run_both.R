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
  
  use_demetra <- TRUE
  
  if (use_demetra) {
    demetra_output <- get_demetra_params(data_path)
    demetra_output_external <- get_demetra_params(external_data_path)
  }
  
  # arima_1 : arima_logrgdp_list_dem, data is in logs, params from demetra
  # arima_2 : arima_logrgdp_auto_slow, data is in logs, auto.arima with stepwise FALSE and approximation FALSE
  # arima_3 : arima_rgdp_auto_slow, data is levels, auto.arima lambda = 0, with stepwise FALSE and approximation FALSE
  # arima_4 : arima_rgdp_auto_slow_badj, arima_3 with bias.adjust = TRUE
  # arima_5 : arima_yoyrgdp_auto_slow, arima_2 but data is in yoy growth
  
  
  if (do_demetra) {
    
  }
  
  
  
  
  
  if (models %in% c("all", "all_arimas", "demetra", "auto.arima", "demetra_and_auto.arima")) {
    
    
    print("Fitting arima suggested by Demetra (model 1)")
    fit_arima_1 <- fit_arimas(
      y_ts = log(rgdp_data), order_list = demetra_output[["rgdp_order_list"]],
      this_arima_names = "rgdp")[[1]]
    
    
    print("Fitting auto.arima to log data (sw and apprx set to FALSE) (model 2)")
    fit_arima_2 <- fit_arimas(
      y_ts = log(rgdp_data), include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]
    
    
    print("Fitting auto.arima (sw and apprx set to FALSE) to level data with lambda=0 (model 3)")
    fit_arima_3 <- fit_arimas(
      y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
      my_lambda = 0, my_biasadj = FALSE
    )[[1]]
    
    
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
    
  }
  
  if (models %in% c("all")) {
    print("Fitting auto ETS to level data (model 6)")
    fit_ets_6 <- ets(rgdp_data)
    
    print("Fitting auto ETS to level data, with lambda=0 and bias-adjusted (model 7)")
    fit_ets_7 <- ets(rgdp_data, lambda = 0, biasadj = TRUE)
    
    
    fets_6 <- function(x, h) {
      forecast(ets(x), h = h)
    }
    
    fets_7 <- function(x, h) {
      forecast(ets(x, lambda = 0, biasadj = TRUE), h = h)
    }
    
  }
  
  print(paste("Computing CV errors (tsCV)) for each model, with and without a rolling window of", tsCV_win))
  
  
  if (models %in% c("all", "all_arimas")) {
    farima_1 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_1$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_1$arma)[c(3, 7, 4)],
                     include.drift = TRUE), h = h)
    }
    
    farima_2 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_2$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_2$arma)[c(3, 7, 4)],
                     include.drift = TRUE), h = h)
    }
    
    farima_3 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_3$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_3$arma)[c(3, 7, 4)],
                     include.drift = TRUE, lambda = 0), h = h)
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
    
    e_1 <- tsCV(log(rgdp_data), farima_1, h = 8, window = tsCV_win)
    e_2 <- tsCV(log(rgdp_data), farima_2, h = 8, window = tsCV_win)
    e_3 <- tsCV(rgdp_data, farima_3, h = 8, window = tsCV_win)
    e_4 <- tsCV(rgdp_data, farima_4, h = 8, window = tsCV_win)
    e_5 <- tsCV(yoy_data_ts, farima_5, h = 8, window = tsCV_win)
    
    e_1nw <- tsCV(log(rgdp_data), farima_1, h = 8)
    e_2nw <- tsCV(log(rgdp_data), farima_2, h = 8)
    e_3nw <- tsCV(rgdp_data, farima_3, h = 8)
    e_4nw <- tsCV(rgdp_data, farima_4, h = 8)
    e_5nw <- tsCV(yoy_data_ts, farima_5, h = 8)
    
    e_list <- list(e_1, e_2, e_3, e_4, e_5)
    e_nw_list <- list(e_1nw, e_2nw, e_3nw, e_4nw, e_5nw)

  }
  
  
  if (models %in% c("all")) {

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
    
    e_list <- list(e_1, e_2, e_3, e_4, e_5, e_6, e_7)
    e_nw_list <- list(e_1nw, e_2nw, e_3nw, e_4nw, e_5nw, e_6nw, e_7nw)
    
  }

  # e_8 <- tsCV(rgdp_ts, thetaf, h = 8, window = tsCV_win)
  # e_8nw <- tsCV(rgdp_ts, thetaf, h = 8)
  

  
  cv_rmse <- map(e_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
  cv_mae <- map(e_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)
  
  cv_rmse_nw <- map(e_nw_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
  cv_mae_nw <- map(e_nw_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)
  
  
  if (models %in% c("all", "all_arimas")) {
    fc_arima_1 <- forecast(fit_arima_1, h = h_max)
    fc_arima_2 <- forecast(fit_arima_2, h = h_max) 
    fc_arima_3 <- forecast(fit_arima_3, h = h_max, lambda = 0) 
    fc_arima_4 <- forecast(fit_arima_4, h = h_max, lambda = 0, biasadj = TRUE) 
    fc_arima_5 <- forecast(fit_arima_5, h = h_max) 

    if (n_offset > 0) {
      
      acc_arima_1 <- accuracy(f = fc_arima_1, x = log(test_data))
      acc_arima_2 <- accuracy(f = fc_arima_2, x = log(test_data))
      acc_arima_3 <- accuracy(f = fc_arima_3, x = test_data)
      acc_arima_4 <- accuracy(f = fc_arima_4, x = test_data)
      acc_arima_5 <- accuracy(f = fc_arima_5, x = yoy_test_data)

      
    } else {
      acc_arima_1 <- accuracy(f = fc_arima_1)
      acc_arima_2 <- accuracy(f = fc_arima_2)
      acc_arima_3 <- accuracy(f = fc_arima_3)
      acc_arima_4 <- accuracy(f = fc_arima_4)
      acc_arima_5 <- accuracy(f = fc_arima_5)
    }
    
    
    acc_all_training <- rbind(acc_arima_1[1,], acc_arima_2[1,],
                              acc_arima_3[1,], acc_arima_4[1,], 
                              acc_arima_5[1,])
    
    acc_all_test <- NULL
    
    if (n_offset > 0) {
      acc_all_test <- rbind(acc_arima_1[2,], acc_arima_2[2,],
                            acc_arima_3[2,], acc_arima_4[2,], 
                            acc_arima_5[2,])
    }
    
    
    difflog_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, dodifflog = TRUE, isloglevel = TRUE)
    difflog_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, dodifflog = TRUE, isloglevel = TRUE)
    yoy_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, isloglevel = TRUE)
    yoy_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, isloglevel = TRUE)
    yoy_fc_arima_3 <- fc_yoy_from_fc_level(fc_arima_3)
    yoy_fc_arima_4 <- fc_yoy_from_fc_level(fc_arima_4)
    
  }
  
  
  
  if (models %in% c("all")) {
    fc_ets_6 <- forecast(fit_ets_6, h = h_max) 
    fc_ets_7 <- forecast(fit_ets_7, h = h_max) 
    
    if (n_offset > 0) {
      
      acc_ets_6 <- accuracy(f = fc_ets_6, x = test_data)
      acc_ets_7 <- accuracy(f = fc_ets_7, x = test_data)
      
    } else {
      acc_ets_6 <- accuracy(f = fc_ets_6)
      acc_ets_7 <- accuracy(f = fc_ets_7)
    }
    
    
    acc_all_training <- rbind(acc_arima_1[1,], acc_arima_2[1,],
                              acc_arima_3[1,], acc_arima_4[1,], 
                              acc_arima_5[1,],
                              acc_ets_6[1,], acc_ets_7[1,])
    
    acc_all_test <- NULL
    
    if (n_offset > 0) {
      acc_all_test <- rbind(acc_arima_1[2,], acc_arima_2[2,],
                            acc_arima_3[2,], acc_arima_4[2,], 
                            acc_arima_5[2,],
                            acc_ets_6[2,], acc_ets_7[2,])
    }
    
    yoy_fc_ets_6 <- fc_yoy_from_fc_level(fc_ets_6)
    yoy_fc_ets_7 <- fc_yoy_from_fc_level(fc_ets_7)
    
  }
  
  
  
  if (models %in% c("all", "all_arimas")) {
    
    p_demetra_vs_autoarima_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) +
      autolayer(yoy_fc_arima_1[["yoy_fc"]], series = "yoy_logdm") + 
      autolayer(yoy_fc_arima_2[["yoy_fc"]], series = "yoy_logauto") +
      labs(y = "GDP", x = "", title = "demetra vs auto.arima (growth)") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    p_demetra_vs_autoarima_lev <- autoplot(rgdp_data, freq = freq) + 
      autolayer(exp(fc_arima_2$mean), series = "logdm")  + 
      autolayer(exp(fc_arima_1$mean), series = "logauto") +
      labs(y = "GDP", x = "", title = "demetra vs auto.arima (level)") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    p_badj_vs_nobadj_lambda0_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) +
      autolayer(yoy_fc_arima_3[["yoy_fc"]], series = "yoy_lam0") + 
      autolayer(yoy_fc_arima_4[["yoy_fc"]], series = "yoy_lam0_badj") +
      labs(y = "GDP", x = "", title = "lambda = 0, with and without bias adj (growth)") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    p_badj_vs_nobadj_lambda0_lev <- autoplot(rgdp_data, freq = freq) + 
      autolayer(fc_arima_3, PI = FALSE, series = "lam0") + 
      autolayer(fc_arima_4, PI = FALSE, series = "lam0 with bias adj.") +
      labs(y = "GDP", x = "", title = "lambda = 0, with and without bias adj (level)") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    p_lam0_vs_expoflog_lev <- autoplot(rgdp_data, freq = freq) + 
      autolayer(fc_arima_3, PI = FALSE, series = "level with lam0") + 
      autolayer(exp(fc_arima_2$mean), series = "exp of log")  +
      labs(y = "GDP", x = "", title = "lambda = 0, with and without bias adj (level)") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    p_lam0_vs_expoflog_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) +
      autolayer(yoy_fc_arima_2[["yoy_fc"]], series = "exp of log") + 
      autolayer(yoy_fc_arima_3[["yoy_fc"]], series = "yoy with lam0") +
      labs(y = "GDP", x = "", title = "lambda = 0, with and without bias adj (growth)") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    p_direct_vs_indirect_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) + 
      autolayer(fc_arima_5$mean, series = "direct_yoy") + 
      autolayer(yoy_fc_arima_3[["yoy_fc"]], series = "indirect_yoy (auto.arima lev)") +
      labs(y = "GDP", x = "", title = "lambda = 0 vs exp(log), with and without bias adj (growth)") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    
    
    
    
    p_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) + 
      autolayer(difflog_fc_arima_1[["yoy_fc"]], series = "dl_logdm") + 
      autolayer(yoy_fc_arima_1[["yoy_fc"]], series = "yoy_logdm") + 
      autolayer(fc_arima_5$mean, series = "direct_yoy") + 
      autolayer(difflog_fc_arima_2[["yoy_fc"]], series = "dl_logauto") + 
      autolayer(yoy_fc_arima_2[["yoy_fc"]], series = "yoy_logauto") + 
      autolayer(yoy_fc_arima_3[["yoy_fc"]], series = "yoy_auto") + 
      autolayer(yoy_fc_arima_4[["yoy_fc"]], series = "yoy_auto_badj") + 
      labs(y = "growth", x = "", title = "YoY growth of quarterly GDP") +
      theme(legend.position = "bottom",
            legend.title = element_blank())

    
    p_level <- autoplot(rgdp_data) + 
      autolayer(fc_arima_3, PI = FALSE, series = "auto.arima, lam0") + 
      autolayer(fc_arima_4, PI = FALSE, series = "auto.arima, lam0 badj") + 
      autolayer(exp(fc_arima_2$mean), series = "auto.arima for logs")  + 
      autolayer(exp(fc_arima_1$mean), series = "demetra for logs")  + 
      labs(y = "GDP", x = "", title = "Quarterly GDP") +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  }
  
  if (models %in% c("all")) {
    
    p_yoy <- p_yoy + 
      autolayer(yoy_fc_ets_6[["yoy_fc"]], series = "yoy_ets") + 
      autolayer(yoy_fc_ets_7[["yoy_fc"]], series = "yoy_ets_lambadj") 
    
    p_level <- p_level + 
      autolayer(fc_ets_6, PI = FALSE, series = "ets") + 
      autolayer(fc_ets_7, PI = FALSE, series = "ets, lam0 badj") 
  }
  
    
  if (n_offset > 0) {
    p_yoy <- p_yoy + autolayer(yoy_test_data, series = "test_data")
    p_level <- p_level + autolayer(test_data, series = "test_data")
  }

  if (models %in% c("all", "all_arimas")) {
    y_ave_logdem_ldiff <- difflog_fc_arima_1[["yearly_average_yoy"]]
    y_ave_logdem <- yoy_fc_arima_1[["yearly_average_yoy"]]
    y_ave_dm_2018 <- round(100*as.numeric(y_ave_logdem["2018"]), digits = 2)
    y_ave_dm_2019 <- round(100*as.numeric(y_ave_logdem["2019"]), digits = 2)
    
    
    y_ave_logauto_ldiff <- difflog_fc_arima_2[["yearly_average_yoy"]]
    y_ave_logauto <- yoy_fc_arima_2[["yearly_average_yoy"]]
    y_ave_dm_2018 <- round(100*as.numeric(y_ave_logdem["2018"]), digits = 2)
    y_ave_dm_2019 <- round(100*as.numeric(y_ave_logdem["2019"]), digits = 2)
    
    
    y_ave_auto <- yoy_fc_arima_3[["yearly_average_yoy"]]
    y_ave_auto_badj <- yoy_fc_arima_4[["yearly_average_yoy"]]
    
    y_gt_logdem_ldiff <- difflog_fc_arima_1[["yearly_growth_of_total"]]
    
    y_gt_logdem <- yoy_fc_arima_1[["yearly_growth_of_total"]]
    gt_dm_2018 <- round(100*as.numeric(y_gt_logdem["2018"]), digits = 2)
    gt_dm_2019 <- round(100*as.numeric(y_gt_logdem["2019"]), digits = 2)
    
    y_gt_logauto_ldiff <- difflog_fc_arima_2[["yearly_growth_of_total"]]
    
    y_gt_logauto <- yoy_fc_arima_2[["yearly_growth_of_total"]]
    gt_au_2018 <- round(100*as.numeric(y_gt_logauto["2018"]), digits = 2)
    gt_au_2019 <- round(100*as.numeric(y_gt_logauto["2019"]), digits = 2)
    
    y_gt_auto <- yoy_fc_arima_3[["yearly_growth_of_total"]]
    
    y_gt_auto_badj <- yoy_fc_arima_4[["yearly_growth_of_total"]]
    
    y_total_logdem_ldiff <- difflog_fc_arima_1[["yearly_total"]]
    y_total_logdem <- yoy_fc_arima_1[["yearly_total"]]
    y_tot_dm_2018 <- round(100*as.numeric(y_total_logdem["2018"]), digits = 2)
    y_tot_dm_2019 <- round(100*as.numeric(y_total_logdem["2019"]), digits = 2)
    
    
    y_total_logauto_ldiff <- difflog_fc_arima_2[["yearly_total"]]
    y_total_logauto <- yoy_fc_arima_2[["yearly_total"]]
    y_tot_au_2018 <- round(100*as.numeric(y_total_logauto["2018"]), digits = 2)
    y_tot_au_2019 <- round(100*as.numeric(y_total_logauto["2019"]), digits = 2)
    
    
    y_total_auto <- yoy_fc_arima_3[["yearly_total"]]
    y_total_auto_badj <- yoy_fc_arima_4[["yearly_total"]]
    
    y_ave_all <- cbind(dem_ld = y_ave_logdem_ldiff, dem_yoy = y_ave_logdem, 
                       logauto_ld = y_ave_logauto_ldiff,
                       logauto_yoy = y_ave_logauto, 
                       auto = y_ave_auto, auto_badj = y_ave_auto_badj)
    
    y_gt_all <- cbind(dem = y_gt_logdem, 
                      auto = y_gt_auto, auto_badj = y_gt_auto_badj)
    
    y_total_all <- cbind(dem = y_total_logdem,  
                         auto = y_total_auto , auto_badj = y_total_auto_badj)
    
    
  }
  
  if (models %in% c("all")) {
    y_ave_ets <- yoy_fc_ets_6[["yearly_average_yoy"]]
    y_ave_ets_badj <- yoy_fc_ets_7[["yearly_average_yoy"]]
    
    y_total_ets <- yoy_fc_ets_6[["yearly_total"]]
    y_total_ets_badj <- yoy_fc_ets_7[["yearly_total"]]
    
    y_gt_ets <- yoy_fc_ets_6[["yearly_growth_of_total"]]
    y_gt_ets_badj <- yoy_fc_ets_7[["yearly_growth_of_total"]]
    
    y_ave_all <- cbind(y_ave_logdem_ldiff, y_ave_logdem, y_ave_logauto_ldiff,
                       y_ave_logauto, y_ave_auto, y_ave_auto_badj, y_ave_ets,
                       y_ave_ets_badj)
    
    y_gt_all <- cbind(y_gt_logdem_ldiff, y_gt_logdem, y_gt_logauto_ldiff,
                      y_gt_logauto, y_gt_auto, y_gt_auto_badj,
                      y_gt_ets, y_gt_ets_badj)
    
    y_total_all <- cbind(y_total_logdem_ldiff, y_total_logdem, y_total_logauto_ldiff,
                         y_total_logauto, y_total_auto, y_total_auto_badj,
                         y_total_ets, y_total_ets_badj)
  }
    

  return(list(arima_of_log_y_demetra = fit_arima_1,
              arima_of_log_y_autoarima = fit_arima_2,
              arima_of_y_autoarima = fit_arima_3,
              arima_of_y_autoarima_badj = fit_arima_4,
              arima_of_yoy_y_autoarima = fit_arima_5,
              yearly_total_y = y_total_all,
              growth_of_yearly_total_y = y_gt_all,
              yearly_average_yoy_growth = y_ave_all,
              accuracy_measures_training_set = acc_all_training,
              accuracy_measures_test_set = acc_all_test,
              plot_y_level = p_level,
              plot_y_yoy =  p_yoy,
              plot_dm_vs_auto_yoy = p_demetra_vs_autoarima_yoy,
              plot_dm_vs_auto_lev = p_demetra_vs_autoarima_lev,
              plot_badj_vs_not_yoy = p_badj_vs_nobadj_lambda0_yoy,
              plot_badj_vs_not_lev = p_badj_vs_nobadj_lambda0_lev,
              plot_lam_vs_explog_yoy = p_lam0_vs_expoflog_yoy,
              plot_lam_vs_explog_lev = p_lam0_vs_expoflog_lev,
              plot_yoy_indirect_vs_direct = p_direct_vs_indirect_yoy,
              cv_rmse = cv_rmse,
              cv_rmse_no_rolling_window = cv_rmse_nw,
              cv_mae = cv_mae,
              cv_mae_no_rolling_window = cv_mae_nw)
         )
  
}

tic()
univariate_rgpd_obj <- univariate_analysis(rgdp_ts, models = "all_arimas")
toc()

ari_1 <- univariate_rgpd_obj$arima_of_log_y_demetra
ari_2 <- univariate_rgpd_obj$arima_of_log_y_autoarima

ari_3 <- univariate_rgpd_obj$arima_of_y_autoarima
ari_4 <- univariate_rgpd_obj$arima_of_y_autoarima_badj

ari_5 <- univariate_rgpd_obj$arima_of_yoy_y_autoarima



univariate_rgpd_obj$yearly_total_y

univariate_rgpd_obj$yearly_average_yoy_growth

univariate_rgpd_obj$growth_of_yearly_total_y

gr_dm <- (univariate_rgpd_obj$growth_of_yearly_total_y)[,1]
gr_dm_2018 <- round(100*as.numeric(gr_dm["2018"]), digits = 2)
gr_dm_2019 <- round(100*as.numeric(gr_dm["2019"]), digits = 2)
gr_dm_2018
gr_dm_2019

gr_au <- (univariate_rgpd_obj$growth_of_yearly_total_y)[,2]
gr_au_2018 <- round(100*as.numeric(gr_au["2018"]), digits = 2)
gr_au_2019 <- round(100*as.numeric(gr_au["2019"]), digits = 2)
gr_au_2018
gr_au_2019

ave_dm <- (univariate_rgpd_obj$yearly_average_yoy_growth)[,2]
ave_dm_2018 <- round(100*as.numeric(ave_dm["2018"]), digits = 2)
ave_dm_2019 <- round(100*as.numeric(ave_dm["2019"]), digits = 2)
ave_dm_2018
ave_dm_2019

ave_au <- (univariate_rgpd_obj$yearly_average_yoy_growth)[,4]
ave_au_2018 <- round(100*as.numeric(ave_au["2018"]), digits = 2)
ave_au_2019 <- round(100*as.numeric(ave_au["2019"]), digits = 2)
ave_au_2018
ave_au_2019

tot_dm <- (univariate_rgpd_obj$yearly_total_y)[,1]
tot_dm_2018 <- round(100*as.numeric(tot_dm["2018"]), digits = 0)
tot_dm_2019 <- round(100*as.numeric(tot_dm["2019"]), digits = 0)
tot_dm_2018
tot_dm_2019

tot_au <- (univariate_rgpd_obj$yearly_total_y)[,2]
tot_au_2018 <- round(100*as.numeric(tot_au["2018"]), digits = 0)
tot_au_2019 <- round(100*as.numeric(tot_au["2019"]), digits = 0)
tot_au_2018
tot_au_2019



p_da_lev <- univariate_rgpd_obj$plot_dm_vs_auto_lev 
an_p_da_lev_2018 <- paste("2018: dm =", tot_dm_2018, ", auto =", tot_au_2018)
an_p_da_lev_2019 <- paste("2019: dm =", tot_dm_2019, ", auto =", tot_au_2019)
p_da_lev <- p_da_lev + 
  annotate("text", x = 2003, y = 40000, label = an_p_da_lev_2018, size = 3) + 
  annotate("text", x = 2003, y = 35000, label = an_p_da_lev_2019, size = 3) 
p_da_lev


p_da_yoy <- univariate_rgpd_obj$plot_dm_vs_auto_yoy
an_p_da_yoy_2018 <- paste("2018: dm =", ave_dm_2018, ", auto =", ave_au_2018)
an_p_da_yoy_2019 <- paste("2019: dm =", ave_dm_2019, ", auto =", ave_au_2019)
p_da_yoy <- p_da_yoy + 
  annotate("text", x = 2015, y = -0.02, label = an_p_da_yoy_2018, size = 3) + 
  annotate("text", x = 2015, y = -0.04, label = an_p_da_yoy_2019, size = 3) 
p_da_yoy

p_biasornot_yoy <- univariate_rgpd_obj$plot_badj_vs_not_yoy
p_biasornot_lev <- univariate_rgpd_obj$plot_badj_vs_not_lev

p_lam_vs_explog_yoy <- univariate_rgpd_obj$plot_lam_vs_explog_yoy
p_lam_vs_explog_lev <- univariate_rgpd_obj$plot_lam_vs_explog_lev

p_dir_vs_indir_yoy <- univariate_rgpd_obj$plot_yoy_indirect_vs_direct


p_da_lev + 
  coord_cartesian(xlim = c(2015, 2020))

p_da_yoy + 
  coord_cartesian(xlim = c(2015, 2020))


p_biasornot_lev + 
  coord_cartesian(xlim = c(2015, 2020))

p_biasornot_yoy + 
  coord_cartesian(xlim = c(2015, 2020))


p_lam_vs_explog_yoy + 
  coord_cartesian(xlim = c(2015, 2020))

p_lam_vs_explog_lev + 
  coord_cartesian(xlim = c(2015, 2020))


p_dir_vs_indir_yoy + 
  coord_cartesian(xlim = c(2015, 2020))


# tic()
# univariate_rgpd_obj <- univariate_analysis(rgdp_ts, models = "all")
# toc()


univariate_rgpd_obj_4 <- univariate_analysis(rgdp_ts, n_offset = 4)

univariate_rgpd_obj_8 <- univariate_analysis(rgdp_ts, n_offset = 8)

univariate_rgpd_obj_12 <- univariate_analysis(rgdp_ts, n_offset = 12)

p_yoy <- univariate_rgpd_obj$plot_y_yoy
print(p_yoy)
p_lev <- univariate_rgpd_obj$plot_y_level
print(p_lev)


p_yoy4 <- univariate_rgpd_obj_4$plot_y_yoy

p_yoy4 <- p_yoy4
print(p_yoy4)


p_lev4 <- univariate_rgpd_obj_4$plot_y_level
print(p_lev4)


p_yoy8 <- univariate_rgpd_obj_8$plot_y_yoy
print(p_yoy8)
p_lev8 <- univariate_rgpd_obj_8$plot_y_level
print(p_lev8)

p_yoy12 <- univariate_rgpd_obj_12$plot_y_yoy
print(p_yoy12)
p_lev12 <- univariate_rgpd_obj_12$plot_y_level + 
  tidyquant::theme_tq()
print(p_lev12)




#### -------- foo -------

rgdp_data <- rgdp_ts
n_offset <- 8
freq <-  4
h_max <- 8
tsCV_win <- 40

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


use_demetra <- TRUE

if (use_demetra) {
  demetra_output <- get_demetra_params(data_path)
  demetra_output_external <- get_demetra_params(external_data_path)
}

# arima_1 : arima_logrgdp_list_dem, data is in logs, params from demetra
# arima_2 : arima_logrgdp_auto_slow, data is in logs, auto.arima with stepwise FALSE and approximation FALSE
# arima_3 : arima_rgdp_auto_slow, data is levels, auto.arima lambda = 0, with stepwise FALSE and approximation FALSE
# arima_4 : arima_rgdp_auto_slow_badj, arima_3 with bias.adjust = TRUE
# arima_5 : arima_yoyrgdp_auto_slow, arima_2 but data is in yoy growth



fit_arima_1 <- fit_arimas(
  y_ts = log(rgdp_data), order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp")[[1]]


fit_arima_2 <- fit_arimas(
  y_ts = log(rgdp_data), include.constant = TRUE, auto = TRUE, 
  do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]


fit_arima_3 <- fit_arimas(
  y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
  do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
  my_lambda = 0, my_biasadj = FALSE
)[[1]]


fit_arima_4 <- fit_arimas(
  y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
  do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
  my_lambda = 0, my_biasadj = TRUE
)[[1]]


fit_arima_5 <- fit_arimas(
  y_ts = make_yoy_ts(rgdp_data, freq = freq), include.constant = TRUE, auto = TRUE, 
  do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]


fit_ets_6 <- ets(rgdp_data)

fit_ets_7 <- ets(rgdp_data, lambda = 0, biasadj = TRUE)




farima_1 <- function(x, h) {
  forecast(Arima(x, order = (fit_arima_1$arma)[c(1, 6, 2)],
                 seasonal = (fit_arima_1$arma)[c(3, 7, 4)],
                 include.drift = TRUE), h = h)
}

farima_2 <- function(x, h) {
  forecast(Arima(x, order = (fit_arima_2$arma)[c(1, 6, 2)],
                 seasonal = (fit_arima_2$arma)[c(3, 7, 4)],
                 include.drift = TRUE), h = h)
}

farima_3 <- function(x, h) {
  forecast(Arima(x, order = (fit_arima_3$arma)[c(1, 6, 2)],
                 seasonal = (fit_arima_3$arma)[c(3, 7, 4)],
                 include.drift = TRUE, lambda = 0), h = h)
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

fets_6 <- function(x, h) {
  forecast(ets(x), h = h)
}

fets_7 <- function(x, h) {
  forecast(ets(x, lambda = 0, biasadj = TRUE), h = h)
}

ftheta_8 <- function(x, h) {
  thetaf(x,  h = h)
}


yoy_data_ts <- make_yoy_ts(rgdp_data, freq = freq)
e_1 <- tsCV(log(rgdp_data), farima_1, h = 8, window = tsCV_win)
e_2 <- tsCV(log(rgdp_data), farima_2, h = 8, window = tsCV_win)
e_3 <- tsCV(rgdp_data, farima_3, h = 8, window = tsCV_win)
e_4 <- tsCV(rgdp_data, farima_4, h = 8, window = tsCV_win)
e_5 <- tsCV(yoy_data_ts, farima_5, h = 8, window = tsCV_win)
e_6 <- tsCV(rgdp_ts, fets_6, h = 8, window = tsCV_win)
e_7 <- tsCV(rgdp_ts, fets_7, h = 8, window = tsCV_win)
e_8 <- tsCV(rgdp_ts, thetaf, h = 8, window = tsCV_win)

e_1nw <- tsCV(log(rgdp_data), farima_1, h = 8)
e_2nw <- tsCV(log(rgdp_data), farima_2, h = 8)
e_3nw <- tsCV(rgdp_data, farima_3, h = 8)
e_4nw <- tsCV(rgdp_data, farima_4, h = 8)
e_5nw <- tsCV(yoy_data_ts, farima_5, h = 8)
e_6nw <- tsCV(rgdp_ts, fets_6, h = 8)
e_7nw <- tsCV(rgdp_ts, fets_7, h = 8)
e_8nw <- tsCV(rgdp_ts, thetaf, h = 8)

e_list <- list(e_1, e_2, e_3, e_4, e_5, e_6, e_7, e_8)
e_nw_list <- list(e_1nw, e_2nw, e_3nw, e_4nw, e_5nw, e_6nw, e_7nw, e_8nw)

roo <- map(e_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
roo

moo <- map(e_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)
moo


roo_nw <- map(e_nw_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
roo_nw
moo_nw <- map(e_nw_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)
moo_nw


fc_arima_1 <- forecast(fit_arima_1, h = h_max)
fc_arima_2 <- forecast(fit_arima_2, h = h_max) 
fc_arima_3 <- forecast(fit_arima_3, h = h_max) 
fc_arima_4 <- forecast(fit_arima_4, h = h_max) 
fc_arima_5 <- forecast(fit_arima_5, h = h_max) 
fc_ets_6 <- forecast(fit_ets_6, h = h_max) 
fc_ets_7 <- forecast(fit_ets_7, h = h_max) 



if (n_offset > 0) {
  
  # print("fc_arima_rgdp_auto_slow$mean")
  # print(fc_arima_rgdp_auto_slow$mean)
  # print("test_data")
  # print(test_data)
  
  acc_arima_1 <- accuracy(f = fc_arima_1, x = log(test_data))
  acc_arima_2 <- accuracy(f = fc_arima_2, x = log(test_data))
  acc_arima_3 <- accuracy(f = fc_arima_3, x = test_data)
  acc_arima_4 <- accuracy(f = fc_arima_4, x = test_data)
  acc_arima_5 <- accuracy(f = fc_arima_5, x = yoy_test_data)
  acc_ets_6 <- accuracy(f = fc_ets_6, x = test_data)
  acc_ets_7 <- accuracy(f = fc_ets_7, x = test_data)
  
  # print("acc_arima_yoyrgdp_auto_slow")
  # print(acc_arima_yoyrgdp_auto_slow)
  # print("acc_arima_rgdp_auto_slow")
  # print(acc_arima_rgdp_auto_slow)
  # 
  
} else {
  acc_arima_1 <- accuracy(f = fc_arima_1)
  acc_arima_2 <- accuracy(f = fc_arima_2)
  acc_arima_3 <- accuracy(f = fc_arima_3)
  acc_arima_4 <- accuracy(f = fc_arima_4)
  acc_arima_5 <- accuracy(f = fc_arima_5)
  acc_ets_6 <- accuracy(f = fc_ets_6)
  acc_ets_7 <- accuracy(f = fc_ets_7)
}


acc_all_training <- rbind(acc_arima_1[1,], acc_arima_2[1,],
                          acc_arima_3[1,], acc_arima_4[1,], 
                          acc_arima_5[1,],
                          acc_ets_6[1,], acc_ets_7[1,])

acc_all_test <- NULL

if (n_offset > 0) {
  acc_all_test <- rbind(acc_arima_1[2,], acc_arima_2[2,],
                        acc_arima_3[2,], acc_arima_4[2,], 
                        acc_arima_5[2,],
                        acc_ets_6[2,], acc_ets_7[2,])
}



foo <- autoplot(rgdp_ts)

foo + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    plot.background = element_rect(fill = "antiquewhite"))

foo

#### ------ hoo
library(forecastHybrid)
quickModel <- hybridModel(rgdp_ts)
accuracy(quickModel)

plot(quickModel, type = "fit")
