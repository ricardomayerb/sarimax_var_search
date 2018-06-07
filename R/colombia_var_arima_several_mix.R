source('./R/utils_av.R')

country_name <- "Colombia"

# final_forecast_horizon <- c(2020, 12)
# h_max = 8 # last rgdp data is 2017 Q4
# number_of_cv = 8
# train_span = 16
# arima_res2 <- get_arima_results(country_name = country_name, h_max = 8,
#                                number_of_cv = 8, train_span = 16,
#                                final_ext_horizon = final_forecast_horizon)

arima_res <- get_arima_results(country_name = country_name, read_results = TRUE)

# rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
# rmse_level_sarimax <- arima_res$compare_rmse
# v_lags_order_season <- arima_res$var_lag_order_season 
extended_x_data_ts <- arima_res$mdata_ext_ts
rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima


#### VAR and sarimax together -------------- 


models_and_accu <- readRDS("./data/Colombia_by_step_12345.rds")
cv_objects <- readRDS("./data/Colombia_by_step_12345_cv_objects.rds")
VAR_data <- readRDS("./data/VAR_data_Colombia.rds")

h_max <- 6


models_tbl <- make_models_tbl(
  arima_res = arima_res, var_models_and_rmse = models_and_accu, VAR_data = VAR_data,
  h_max = h_max)


models_tbl_ssel <- make_models_tbl(arima_res, var_models_and_rmse = models_and_accu, VAR_data = VAR_data,
h_max = h_max, ave_rmse_sel = TRUE)


ffall <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                           h = h_max, extended_x_data_ts = extended_x_data_ts,
                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                           max_rank_h = 30)
summ_all <- ffall %>% 
    group_by(horizon) %>%
    summarise(sum_one_h = reduce(one_model_w_fc, sum))


ffall_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                            h = h_max, extended_x_data_ts = extended_x_data_ts,
                            rgdp_ts_in_arima = rgdp_ts_in_arima,
                            max_rank_h = 30)
summ_all_ssel <- ffall_ssel %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



ffall_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                            h = h_max, extended_x_data_ts = extended_x_data_ts,
                            rgdp_ts_in_arima = rgdp_ts_in_arima,
                            max_rank_h = 20)
summ_all_20 <- ffall %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))




ffall_VAR <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                           h = h_max, extended_x_data_ts = extended_x_data_ts,
                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                           model_type = "VAR", max_rank_h = 30)
summ_all_VAR <- ffall_VAR %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



ffall_arima <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                h = h_max, extended_x_data_ts = extended_x_data_ts,
                                rgdp_ts_in_arima = rgdp_ts_in_arima,
                                model_type = "Arima")

summ_all_arima <- ffall_arima %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))


ffall_arima_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                  h = h_max, extended_x_data_ts = extended_x_data_ts,
                                  rgdp_ts_in_arima = rgdp_ts_in_arima,
                                  model_type = "Arima")

summ_all_arima_ssel <- ffall_arima_ssel %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))














