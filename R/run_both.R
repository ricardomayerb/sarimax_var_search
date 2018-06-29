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





tic()
univariate_rgpd_obj <- univariate_analysis(rgdp_ts)
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
