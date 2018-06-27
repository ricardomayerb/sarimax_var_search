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

fit_arima_rgdp_list_dem <- fit_arimas(
  y_ts = log(rgdp_ts), order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp")

rgdp_uncond_fc <- forecast(fit_arima_rgdp_list_dem[["rgdp"]], h = h_max)
rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean

rgdp_uncond_fc_mean

fit_foo <- Arima(y = make_yoy_ts(log(rgdp_ts)), order = c(2,0,0), 
                 seasonal = c(0,0,0), include.constant = TRUE)

fc_foo <- forecast(fit_foo, h = h_max)

data_fc_foo <- ts(c(fit_foo$x, fc_foo$mean), start = start(fit_foo$x),
                  frequency = 4)

moo <- un_yoy_ts(vec_yoy = data_fc_foo, init_lev = subset(log(rgdp_ts), end = 4))  

ts.union(log(rgdp_ts), moo)  


roo <- un_yoy_ts(vec_yoy = make_yoy_ts(log(rgdp_ts)), 
                 init_lev = subset(log(rgdp_ts), end = 4))

ts.union(roo, log(rgdp_ts))

if (always_include_constant) {
  
  fit_arima_monthly_list_dem <- fit_arimas(
    y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
    this_arima_names = monthly_names, force_constant = TRUE)
  
} else {
  
  fit_arima_monthly_list_dem <- fit_arimas(
    y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
    this_arima_names = monthly_names)
}



# goo <- tk_tbl(external_monthly_ts)
# goo
# goodf <- as_data_frame(goo)




tic()
univariate_rgpd_obj <- univariate_analysis(rgdp_ts, do_demetra = TRUE, do_auto_lambda = FALSE)
toc()

univariate_rgpd_obj$plot_levels
univariate_rgpd_obj$plot_yoy
univariate_rgpd_obj$fit_models

univariate_rgpd_obj$yearly_total_y


tic()
univariate_rgpd_obj_da <- univariate_analysis(rgdp_ts, do_demetra = TRUE, do_auto_lambda = TRUE)
toc()




univariate_rgpd_obj_da_o4 <- univariate_analysis(rgdp_ts, do_demetra = TRUE, 
                                                 do_auto_lambda = TRUE,
                                                 n_offset = 4)

univariate_rgpd_obj_da_o8 <- univariate_analysis(rgdp_ts, do_demetra = TRUE, 
                                                 do_auto_lambda = TRUE,
                                                 n_offset = 8)

univariate_rgpd_obj_da_o8$yearly_total_y
univariate_rgpd_obj_da_o8$yearly_average_yoy_growth
univariate_rgpd_obj_da_o8$growth_of_yearly_total_y
univariate_rgpd_obj_da_o8$accuracy_measures_training_set
univariate_rgpd_obj_da_o8$accuracy_measures_test_set
univariate_rgpd_obj_da_o8$cv_rmse
univariate_rgpd_obj_da_o8$cv_rmse_no_rolling_window
univariate_rgpd_obj_da_o8$fit_models


univariate_rgpd_obj_da_o12 <- univariate_analysis(rgdp_ts, do_demetra = TRUE, 
                                                  do_auto_lambda = TRUE,
                                                  n_offset = 12)

univariate_rgpd_obj_da_o16 <- univariate_analysis(rgdp_ts, do_demetra = TRUE, 
                                                  do_auto_lambda = TRUE,
                                                  n_offset = 16)

univariate_rgpd_obj_da_o20 <- univariate_analysis(rgdp_ts, do_demetra = TRUE, 
                                                  do_auto_lambda = TRUE,
                                                  n_offset = 20)

# walk(univariate_rgpd_obj_da$fit_models, print)


univariate_rgpd_obj_da$plot_levels
univariate_rgpd_obj_da$plot_yoy
univariate_rgpd_obj_da_o4$plot_levels
univariate_rgpd_obj_da_o4$plot_yoy
univariate_rgpd_obj_da_o8$plot_levels
univariate_rgpd_obj_da_o8$plot_yoy
univariate_rgpd_obj_da_o12$plot_levels
univariate_rgpd_obj_da_o12$plot_yoy
univariate_rgpd_obj_da_o16$plot_levels
univariate_rgpd_obj_da_o16$plot_yoy
univariate_rgpd_obj_da_o20$plot_levels
univariate_rgpd_obj_da_o20$plot_yoy



#### -------- foo -------

rgdp_data <- rgdp_ts
n_offset <- 8
freq <-  4
h_max <- 8
tsCV_win <- 40
do_auto_lambda = FALSE
do_demetra = TRUE
do_auto_biasadj = FALSE
do_other_auto = FALSE
do_ets = FALSE


