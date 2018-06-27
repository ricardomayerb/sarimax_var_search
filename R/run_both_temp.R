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

fit_arima_rgdp_list_dem <- fit_arimas(
  y_ts = log(rgdp_ts), order_list = demetra_output[["rgdp_order_list"]],
  this_arima_names = "rgdp")

rgdp_uncond_fc <- forecast(fit_arima_rgdp_list_dem[["rgdp"]], h = h_max)
rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean

# default: forecast package's criteria about constants and differencing
# force_constant <-  TRUE
fit_arima_monthly_list_demetra_stata_constants <- fit_arimas(
  y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
  this_arima_names = monthly_names,  force_constant = TRUE, freq = 12)

fit_arima_monthly_list_demetra_r_constants <- fit_arimas(
  y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
  this_arima_names = monthly_names,  force_constant = FALSE, freq = 12)

fit_arima_external_monthly_list_demetra_stata_constants <- fit_arimas(
  y_ts = external_monthly_ts, order_list = demetra_output_external[["monthly_order_list"]],
  this_arima_names = external_monthly_names,  force_constant = force_constant, freq = 12)

fit_arima_external_monthly_list_demetra_r_constants <- fit_arimas(
  y_ts = external_monthly_ts, order_list = demetra_output_external[["monthly_order_list"]],
  this_arima_names = external_monthly_names,  force_constant = force_constant, freq = 12)


# gdp_order <- get_order_from_arima(fit_arima_rgdp_list_dem)[[1]]
gdp_and_dates <- get_rgdp_and_dates(data_path)
mdata_ext_dem_r <- extend_and_qtr(data_mts = monthly_ts, 
                            final_horizon_date = final_forecast_horizon , 
                            vec_of_names = monthly_names, 
                            fitted_arima_list = fit_arima_monthly_list_demetra_r_constants,
                            start_date_gdp = gdp_and_dates[["gdp_start"]],
                            order_list = demetra_output[["monthly_order_list"]])

mdata_ext_dem_stata <- extend_and_qtr(data_mts = monthly_ts, 
                              final_horizon_date = final_forecast_horizon , 
                              vec_of_names = monthly_names, 
                              fitted_arima_list = fit_arima_monthly_list_demetra_stata_constants,
                              start_date_gdp = gdp_and_dates[["gdp_start"]],
                              force_constant = TRUE,
                              order_list = demetra_output[["monthly_order_list"]])

mdata_ext_auto_slow_r <- extend_and_qtr(data_mts = monthly_ts, 
                                  final_horizon_date = final_forecast_horizon , 
                                  vec_of_names = monthly_names, 
                                  fitted_arima_list = fit_arima_monthly_list_auto_slow,
                                  start_date_gdp = gdp_and_dates[["gdp_start"]],
                                  order_list = demetra_output[["monthly_order_list"]])

mdata_ext_auto_r <- extend_and_qtr(data_mts = monthly_ts, 
                                        final_horizon_date = final_forecast_horizon , 
                                        vec_of_names = monthly_names, 
                                        fitted_arima_list = fit_arima_monthly_list_auto,
                                        start_date_gdp = gdp_and_dates[["gdp_start"]],
                                        order_list = demetra_output[["monthly_order_list"]])

mdata_ext_auto_noapp_r <- extend_and_qtr(data_mts = monthly_ts, 
                                   final_horizon_date = final_forecast_horizon , 
                                   vec_of_names = monthly_names, 
                                   fitted_arima_list = fit_arima_monthly_list_auto_noapp,
                                   start_date_gdp = gdp_and_dates[["gdp_start"]],
                                   order_list = demetra_output[["monthly_order_list"]])


# ari_ima_stata <- fit_arima_external_monthly_list_demetra_stata_constants

mdata_ext_r_ts <- mdata_ext_dem_r$quarterly_series_ts
mdata_ext_stata_ts <- mdata_ext_dem_stata$quarterly_series_ts
mdata_ext_r_ts_auto_slow <- mdata_ext_auto_slow_r$quarterly_series_ts
mdata_ext_r_ts_auto <- mdata_ext_auto_r$quarterly_series_ts
mdata_ext_r_ts_auto_noapp <- mdata_ext_auto_noapp_r$quarterly_series_ts

ima_ex_r <- mdata_ext_r_ts[, "imacec"]
ima_ex_s <- mdata_ext_stata_ts[, "imacec"]
ima_ex_r_auto <- mdata_ext_r_ts_auto[, "imacec"]
ima_ex_r_auto_noapp <- mdata_ext_r_ts_auto_noapp[, "imacec"]
ima_ex_r_auto_slow <- mdata_ext_r_ts_auto_slow[, "imacec"]


mdata_ext_r_tsm <- mdata_ext_dem_r$monthly_series_ts
mdata_ext_stata_tsm <- mdata_ext_dem_stata$monthly_series_ts
ima_ex_rm <- mdata_ext_r_tsm[, "imacec"]
ima_ex_sm <- mdata_ext_stata_tsm[, "imacec"]
two_imam <- na.omit(ts.union(ima_ex_rm, ima_ex_sm))
two_ima_yoym <- na.omit(ts.union(make_yoy_ts(ima_ex_r, freq = 12), make_yoy_ts(ima_ex_s, freq = 12)))
autoplot(two_ima_yoy)
autoplot(two_imam)

all_ima <- na.omit(ts.union(ima_ex_r, ima_ex_s, ima_ex_r_auto, ima_ex_r_auto_noapp, ima_ex_r_auto_slow))
all_ima_yoy <- na.omit(ts.union(make_yoy_ts(ima_ex_r), make_yoy_ts(ima_ex_s), make_yoy_ts(ima_ex_r_auto), make_yoy_ts(ima_ex_r_auto_noapp), make_yoy_ts(ima_ex_r_auto_slow)))
autoplot(all_ima_yoy)
autoplot(all_ima)

two_ima <- na.omit(ts.union(ima_ex_r, ima_ex_s))
two_ima_yoy <- na.omit(ts.union(make_yoy_ts(ima_ex_r), make_yoy_ts(ima_ex_s )))
autoplot(two_ima_yoy)
autoplot(two_ima)

three_ima <- na.omit(ts.union(ima_ex_r_auto, ima_ex_r_auto_noapp, ima_ex_r_auto_slow))
three_ima_yoy <- na.omit(ts.union(make_yoy_ts(ima_ex_r_auto), make_yoy_ts(ima_ex_r_auto_noapp), make_yoy_ts(ima_ex_r_auto_slow)))
autoplot(three_ima_yoy)
autoplot(three_ima)



cred_ex_r <- mdata_ext_r_ts[, "cred"]
cred_ex_s <- mdata_ext_stata_ts[, "cred"]
cred_ex_r_auto <- mdata_ext_r_ts_auto[, "cred"]
cred_ex_r_auto_noapp <- mdata_ext_r_ts_auto_noapp[, "cred"]
cred_ex_r_auto_slow <- mdata_ext_r_ts_auto_slow[, "cred"]


all_cred <- na.omit(ts.union(cred_ex_r, cred_ex_s, cred_ex_r_auto, cred_ex_r_auto_noapp, cred_ex_r_auto_slow))

all_cred_yoy <- na.omit(ts.union(make_yoy_ts(cred_ex_r), make_yoy_ts(cred_ex_s), make_yoy_ts(cred_ex_r_auto), make_yoy_ts(cred_ex_r_auto_noapp), make_yoy_ts(cred_ex_r_auto_slow)))

autoplot(all_cred_yoy)
autoplot(all_cred)





imce_ex_r <- mdata_ext_r_ts[, "imce"]
imce_ex_s <- mdata_ext_stata_ts[, "imce"]
imce_ex_r_auto <- mdata_ext_r_ts_auto[, "imce"]
imce_ex_r_auto_noapp <- mdata_ext_r_ts_auto_noapp[, "imce"]
imce_ex_r_auto_slow <- mdata_ext_r_ts_auto_slow[, "imce"]


all_imce <- na.omit(ts.union(imce_ex_r, imce_ex_s, imce_ex_r_auto, imce_ex_r_auto_noapp, imce_ex_r_auto_slow))

all_imce_yoy <- na.omit(ts.union(make_yoy_ts(imce_ex_r), make_yoy_ts(imce_ex_s), make_yoy_ts(imce_ex_r_auto), make_yoy_ts(imce_ex_r_auto_noapp), make_yoy_ts(imce_ex_r_auto_slow)))

autoplot(all_imce_yoy)
autoplot(all_imce)




#### ----- some experiments ----
rgdp_uncond_fc_mean

s4logrgdp <- diff(log(rgdp_ts), lag = 4)
init_logrgdp <- subset(log(rgdp_ts), end = 4)
uns4logrgdp <- un_yoy_ts(init_lev = init_logrgdp, vec_yoy = s4logrgdp)

fit_s4 <- Arima(y = s4logrgdp, order = c(2,0,0), 
                 seasonal = c(0,0,0), include.constant = TRUE)

fc_s4 <- forecast(fit_s4, h = h_max)

data_fc_s4 <- ts(c(fit_s4$x, fc_s4$mean), start = start(fit_s4$x),
                  frequency = 4)

init_fc <- subset(log(rgdp_ts), start = 85)

uns4_fc_s4 <- un_yoy_ts(init_lev = init_fc, vec_yoy = fc_s4$mean)

uns4_data_fc <- ts(c(log(rgdp_ts), uns4_fc_s4), start = start(rgdp_ts),
                   frequency = 4)

res4 <- diff(uns4_data_fc, lag = 4)
subset(res4, start = 85)

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


