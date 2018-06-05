source('./R/utils_av.R')
library(tictoc)


#### start bsarimax part -------------- 

final_forecast_horizon <- c(2019, 12)
h_max = 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16

country_name <- "Colombia"
data_path <- paste0("./data/excel/", country_name,".xlsx")
m_analysis_path <- paste0("data/", country_name,"_m_analysis_rgdp.xlsx")
rds_file_name = paste0("data/sarimax_objects_", country_name,".rds")

tic()
arima_res <- bsarimax_as_function(data_path = data_path, number_of_cv = number_of_cv,
                              train_span = train_span, h_max = h_max,
                              final_forecast_horizon = final_forecast_horizon,
                              outer_cv_round = 0, s4xreg = FALSE)
toc()


rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
rmse_level_sarimax <- arima_res$compare_rmse
v_lags_order_season <- arima_res$var_lag_order_season 
extended_x_data_ts <- arima_res$mdata_ext_ts
rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima

#### VAR and sarimax together -------------- 


models_and_accu <- readRDS("./data/Colombia_by_step_12345.rds")
cv_objects <- readRDS("./data/Colombia_by_step_12345_cv_objects.rds")
VAR_data <- readRDS("./data/VAR_data_Colombia.rds")


h_max <- 6
limit_per_h <- 30


rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
  left_join(v_lags_order_season, by = c("variable", "lag"))


# just_model_and_ave_rmse_2 <- rmse_yoy_sarimax %>% 
#   dplyr::select(variable, lag, arima_order, arima_seasonal) %>% 
#   rename(variables = variable,  lags = lag)


each_h_just_model_and_ave_rmse_var <- models_and_accu %>% 
  mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
  dplyr::select(-c(rank_1, rank_2, rank_3, rank_4, rank_5, rank_6))

each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
  mutate(model_function = "Arima") %>% 
  dplyr::select(variable, lag, yoy_rmse_1, yoy_rmse_2, yoy_rmse_3, yoy_rmse_4, 
                yoy_rmse_5, yoy_rmse_6, arima_order, arima_seasonal, 
                model_function) %>% 
  rename(variables = variable, rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, 
         rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, rmse_5 = yoy_rmse_5, 
         rmse_6 = yoy_rmse_6, lags = lag)

models_rmse_at_each_h_arima <- as_tibble(each_h_just_model_and_ave_rmse_sarimax) %>% 
  gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
  arrange(variables) %>% 
  group_by(rmse_h, variables) %>% 
  mutate(min_per_variable_and_h = min(rmse)) %>% 
  filter(rmse == min_per_variable_and_h) %>% 
  select(-min_per_variable_and_h ) %>%  
  ungroup() %>% 
  group_by(rmse_h) %>% 
  mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
  filter(rmse <= rgdp_rmse) %>% 
  ungroup() %>% 
  select(-rgdp_rmse) %>% 
  arrange(rmse_h, rmse)

# # if lags specificarion are chosen based on average(rmse_1, rmse_2 ...etc)
# models_rmse_at_each_h_arima_stataway <- as_tibble(each_h_just_model_and_ave_rmse_sarimax) %>% 
#   gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
#   arrange(variables) %>% 
#   group_by(rmse_h, variables) %>% 
#   mutate(min_per_variable_and_h = min(rmse)) %>% 
#   filter(rmse == min_per_variable_and_h) %>% 
#   select(-min_per_variable_and_h ) %>%  
#   ungroup() %>% 
#   group_by(rmse_h) %>% 
#   mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
#   filter(rmse <= rgdp_rmse) %>% 
#   ungroup() %>% 
#   select(-rgdp_rmse) %>% 
#   arrange(rmse_h, rmse)
  

models_rmse_at_each_h_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  gather(key = "rmse_h", value = "rmse", starts_with("rmse"))

models_rmse_at_each_h <- rbind(models_rmse_at_each_h_var, 
                               models_rmse_at_each_h_arima) %>% 
  mutate(inv_mse = 1/rmse^2) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  arrange(rmse_h, rank_h)
  

ffall <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_rmse_at_each_h,
                           h = h_max, extended_x_data_ts = extended_x_data_ts,
                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                           max_rank_h = 30)
summ_all <- ffall %>% 
    group_by(horizon) %>%
    summarise(sum_one_h = reduce(one_model_w_fc, sum))



ffall_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_rmse_at_each_h,
                            h = h_max, extended_x_data_ts = extended_x_data_ts,
                            rgdp_ts_in_arima = rgdp_ts_in_arima,
                            max_rank_h = 20)
summ_all_20 <- ffall %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))




ffall_VAR <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_rmse_at_each_h,
                           h = h_max, extended_x_data_ts = extended_x_data_ts,
                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                           model_type = "VAR", max_rank_h = 30)
summ_all_VAR <- ffall_VAR %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



ffall_arima <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_rmse_at_each_h,
                                h = h_max, extended_x_data_ts = extended_x_data_ts,
                                rgdp_ts_in_arima = rgdp_ts_in_arima,
                                model_type = "Arima")

summ_all_arima <- ffall_arima %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))














## code to examine bsarimax individual models predictions

# allfcise <- arima_res$all_raw_fcs %>% 
#   filter(id_fc == "ise") 
# isefcmean <- allfcise[["raw_rgdp_fc"]]
# isefcmean
# isefc <- allfcise[["fc"]]
# isefc[[1]][["model"]]


# just_arima_a <- models_rmse_at_each_h_arima %>% 
#   filter(rank_h <= 24, model_function == "Arima") %>% 
#   group_by(rmse_h) %>% 
#   mutate(sum_invmse_h = sum(inv_mse),
#          model_weight_h = inv_mse/sum_invmse_h,
#          horizon = as.numeric(substr(rmse_h, 6, 6)),
#          fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
#                     ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
#                                     arima_rgdp_ts = rgdp_ts_in_arima, 
#                                     extended_x_data_ts = extended_x_data_ts,
#                                     lags = ..3, order = ..4, seasonal = ..5))
#          )
# 
# 
# just_arima_b <- just_arima_a %>% 
#   mutate(fc_obj = pmap(list(model_function, variables, lags, fit),
#                        ~ forecast_VAR_Arima(model_function = ..1, h = h_max,
#                                             mat_x_ext = extended_x_data_ts, 
#                                             variables = ..2, lags = ..3,
#                                             fit = ..4)
#          ),
#          fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
#          fc_yoy = map2(model_function, fc_mean, 
#                        ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
#                                     fc_ts = .y)),
#          one_model_w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
#                                ~ subset(..1 * ..2, start = ..3, end = ..3)
#          )
#   ) %>% 
#   ungroup()
# 
# summ_just_arima <- just_arima_b %>% 
#   group_by(horizon) %>%
#   summarise(sum_one_h = reduce(one_model_w_fc, sum))



# aoo470 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_rmse_at_each_h,
#                              h = h_max, extended_x_data_ts = extended_x_data_ts,
#                              rgdp_ts_in_arima = rgdp_ts_in_arima, max_rank_h = 470)

# #### ----- experiments  ----------
# 
# 
# ise_ext <- arima_mdata_ext[, "ise"]
# ise_arimax_0 <- myres$all_arimax %>% 
#   filter(id_fc == "ise", lag == 0)
# ise_0_obj <- (ise_arimax_0$arimax)[[1]]
# rgdp_in_arimax <-  ise_0_obj$x
# end_arimax <- stats::end(rgdp_in_arimax)
# maxtime_arimax <- max(time(rgdp_in_arimax))
# start_forecast <- c(year(as.yearqtr(0.25 + maxtime_arimax)),
#                     quarter(as.yearqtr(0.25 + maxtime_arimax)))
# ise_rest <- window(ise_ext, start = start_forecast)
# this_h <- 1
# ise_for_fc <- subset(ise_rest, end = this_h)
# forecast(object = ise_0_obj, h = this_h, xreg = ise_for_fc)
# this_h <- 2
# ise_for_fc <- subset(ise_rest, end = this_h)
# forecast(object = ise_0_obj, h = this_h, xreg = ise_for_fc)
# this_h <- 8
# ise_for_fc <- subset(ise_rest, end = this_h)
# forecast(object = ise_0_obj, h = this_h, xreg = ise_for_fc)
# 
# ise_arimax_1 <- myres$all_arimax %>% 
#   filter(id_fc == "ise", lag == 1)
# this_lag <- ise_arimax_1$lag
# ise_1_obj <- (ise_arimax_1$arimax)[[1]]
# rgdp_in_arimax <-  ise_1_obj$x
# end_arimax <- stats::end(rgdp_in_arimax)
# maxtime_arimax <- max(time(rgdp_in_arimax))
# start_forecast <- c(year(as.yearqtr(0.25 + maxtime_arimax)),
#                     quarter(as.yearqtr(0.25 + maxtime_arimax)))
# ise_xreg <- ise_1_obj$xreg
# 
# 
# ise_arimax_2 <- myres$all_arimax %>% 
#   filter(id_fc == "ise", lag == 2)
# ise_2_obj <- (ise_arimax_2$arimax)[[1]]
# 
# 
# 
# 
# foo <- make_xreg_fc(variable_name = "ise", mx_ext = arima_mdata_ext, lags = 2,
#              start_fc = start_forecast, h = this_h)
# foo
# 
# forecast(object = ise_1_obj, h = this_h, xreg = ise_for_fc)
# forecast(object = ise_2_obj, h = this_h, xreg = foo)
# forecast(object = ise_1_obj, h = this_h, 
#          xreg = make_xreg_fc(
#            variable_name = "ise", mx_ext = arima_mdata_ext, lags = 1,
#            start_fc = start_forecast, h = this_h))
# 
# forecast(object = ise_1_obj, h = this_h, 
#          xreg = make_xreg_fc(
#            variable_name = "ise", mx_ext = arima_mdata_ext, lags = 1,
#            start_fc = start_forecast, h = this_h))
# 
# 
# forecast_from_arimax_obj(arimax_obj = ise_arimax_2, x_variable = "ise",
#                    mat_x_ext = arima_mdata_ext, lags = 2, h = this_h) 

