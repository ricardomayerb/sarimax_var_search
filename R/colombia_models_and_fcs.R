source('./R/utils_av.R')

models_and_accu <- readRDS("./data/Colombia_by_step_12345.rds")
cv_objects <- readRDS("./data/Colombia_by_step_12345_cv_objects.rds")
from_sarima <- readRDS(file = "./data/sarimax_objects_Colombia.rds")



rmse_yoy_sarimax <- from_sarima$compare_rmse_yoy
rmse_level_sarimax <- from_sarima$compare_rmse
v_lags_order_season <- from_sarima$var_lag_order_season 


h_max <- 6
limit_per_h <- 30


rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
  left_join(v_lags_order_season, by = c("variable", "lag"))


rmse_yoy_sarimax$ave_rmse_h1h6 <- rowMeans(cbind(rmse_yoy_sarimax$yoy_rmse_1, 
                                                 rmse_yoy_sarimax$yoy_rmse_2, 
                                                 rmse_yoy_sarimax$yoy_rmse_3, 
                                                 rmse_yoy_sarimax$yoy_rmse_4, 
                                                 rmse_yoy_sarimax$yoy_rmse_5, 
                                                 rmse_yoy_sarimax$yoy_rmse_6)) 

rmse_level_sarimax$ave_rmse_h1h6 <- rowMeans(cbind(rmse_level_sarimax$level_rmse_1, 
                                                   rmse_level_sarimax$level_rmse_2, 
                                                   rmse_level_sarimax$level_rmse_3, 
                                                   rmse_level_sarimax$level_rmse_4, 
                                                   rmse_level_sarimax$level_rmse_5, 
                                                   rmse_level_sarimax$level_rmse_6)) 

rmse_level_sarimax$ave_rmse_h1h6_stata <- rowMeans(cbind(rmse_level_sarimax$rmse1, 
                                                         rmse_level_sarimax$rmse2, 
                                                         rmse_level_sarimax$rmse3, 
                                                         rmse_level_sarimax$rmse4, 
                                                         rmse_level_sarimax$rmse5, 
                                                         rmse_level_sarimax$rmse6))


####### AVERAGE RMSE OVER H = 1, 2, 3, 4, 5 AND 6
# level_accu_r_vs_stata <- rmse_level_sarimax %>% 
#   dplyr::select(variable, lag,  ave_rmse_h1h6, ave_rmse_h1h6_stata)
# 
# just_model_and_ave_rmse_1 <- models_and_accu %>% 
#   dplyr::select(variables, lags, accu_yoy) %>% 
#   mutate(lags = unlist(lags)) %>% 
#   mutate(arima_order = NA, arima_seasonal = NA)

# just_model_and_ave_rmse_1 <- models_and_accu %>% 
#   dplyr::select(variables, lags, accu_yoy) %>% 
#   mutate(lags = unlist(lags)) 

just_model_and_ave_rmse_2 <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, ave_rmse_h1h6, arima_order, arima_seasonal) %>% 
  rename(variables = variable, accu_yoy = ave_rmse_h1h6, lags = lag)


########## RMSE at each H
each_h_just_model_and_ave_rmse_var <- models_and_accu %>% 
  mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
  dplyr::select(-c(rank_1, rank_2, rank_3, rank_4, rank_5, rank_6))

each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
  mutate(model_function = "Arima") %>% 
  dplyr::select(variable, lag, yoy_rmse_1, yoy_rmse_2, yoy_rmse_3, yoy_rmse_4, yoy_rmse_5, yoy_rmse_6, arima_order, arima_seasonal, model_function) %>% 
  rename(variables = variable, rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, 
         rmse_5 = yoy_rmse_5, rmse_6 = yoy_rmse_6, lags = lag)

models_rmse_at_each_h <- as_tibble(rbind(each_h_just_model_and_ave_rmse_var, 
                                         each_h_just_model_and_ave_rmse_sarimax)) %>% 
  arrange(rmse_1) %>% mutate(index = 1:n()) %>% 
  gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
  mutate(inv_mse = 1/rmse^2) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup()





# my_arima_one_x <- function(y_ts, y_order, y_seasonal, xreg_lags, x_name,
#                            xreg_data = NULL)


extended_x_data_ts <- from_sarima$extended_x_data_ts
rgdp_ts_in_arima <- from_sarima[["rgdp_ts_in_arima"]]
VAR_data <- readRDS("./data/VAR_data_Colombia.rds")




fc_log2yoy <- function(model, rgdp_log_ts, fc_ts) {
  
  if (model == "VAR") {
    fc_yoy = fc_ts
  }
  
  if (model == "Arima") {
    fc_log_ts <- fc_ts
    data_fc_log <-  ts(c(rgdp_log_ts, fc_log_ts), frequency = 4,
                       start = stats::start(rgdp_log_ts))
    
    data_fc <- exp(data_fc_log)
    data_fc_yoy <- make_yoy_ts(data_fc)
    fc_yoy <- window(data_fc_yoy, start = stats::start(fc_log_ts))
  }
  
  return(fc_yoy)
  
}

fc_mean_var_arima <- function(model, fc_obj) {
  if (model == "VAR") {
    fc_mean <- fc_obj[["forecast"]][["rgdp"]][["mean"]]
  }
  
  if (model == "Arima") {
    fc_mean <- fc_obj[["mean"]]
  }
  
  return(fc_mean)
}

logyoy <- function(logfc_ts) {
  full_log_ts <- ts(c(rgdp_ts_in_arima, logfc_ts), frequency = 4,
                    start = stats::start(rgdp_ts_in_arima))
  
  diff_full_log_ts <- diff(full_log_ts, lag = 4)
  
  diff_log_fc <- window(diff_full_log_ts, start = stats::start(logfc_ts))
  
  return(diff_log_fc)
}



fit_VAR_Arima <- function(model_function, variables, lags, order, seasonal) {
  if (model_function == "VAR") {
    
    fit <- vars::VAR(y = VAR_data[, variables], p = lags)
  } 
  
  if (model_function == "Arima") {

    fit <- my_arima_one_x(y_ts = rgdp_ts_in_arima, y_order = order, 
                          y_seasonal = seasonal, xreg_lags = lags, 
                          x_name = variables, xreg_data = extended_x_data_ts)
  } 
  return(fit)
}




forecast_VAR_Arima <- function(model_function, variables, lags, fit) {
  
  if (model_function == "VAR") {
    fc <- forecast(fit, h = h_max)
  } 
  
  if (model_function == "Arima") {
    fc <- forecast_one_xreg(arimax_fit = fit, xreg_lags = lags, h = h_max,
                            x_name = variables, xreg_data = extended_x_data_ts)
  } 
  return(fc)
}

h_max = 8




just_arima <- models_rmse_at_each_h %>% 
  filter(rank_h <= 370, model_function == "Arima") %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5)),
         fc_obj = pmap(list(model_function, variables, lags, fit),
                       ~ forecast_VAR_Arima(model_function = ..1, 
                                            variables = ..2, lags = ..3,
                                            fit = ..4)
         ),
         fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
         fc_yoy = map2(model_function, fc_mean, 
                       ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
                                    fc_ts = .y)),
         one_model_w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                               ~ subset(..1 * ..2, start = ..3, end = ..3)
         )
  ) %>% 
  ungroup()


ari_summ <- just_arima %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(one_model_w_fc, sum))




just_arima5 <- models_rmse_at_each_h %>% 
  filter(rank_h <= 5, model_function == "Arima", rmse_h == "rmse_1") %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5)),
         fc_obj = pmap(list(model_function, variables, lags, fit),
                       ~ forecast_VAR_Arima(model_function = ..1, 
                                            variables = ..2, lags = ..3,
                                            fit = ..4)
         ),
         fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
         fc_yoy = map2(model_function, fc_mean, 
                       ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
                                    fc_ts = .y)),
         one_model_w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                               ~ subset(..1 * ..2, start = ..3, end = ..3)
                                    )
  ) %>% 
  ungroup()

ari5_summ <- just_arima5 %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



just_vars5 <- models_rmse_at_each_h %>% 
  filter(rank_h <= 5, model_function == "VAR") %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5))
  )




jafc <- just_arima5$fc_obj
jafcmean <- just_arima5$fc_mean

jafc
jafcmean


allfcise <- from_sarima$all_raw_fcs %>% 
  filter(id_fc == "ise") 

isefcmean <- allfcise[["raw_rgdp_fc"]]
isefcmean
isefc <- allfcise[["fc"]]
isefc[[1]][["model"]]

jafc[[1]][["model"]]



fooa <- models_rmse_at_each_h  %>%  
  filter(rank_h <= limit_per_h) %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5))
         )




foo <- models_rmse_at_each_h  %>%  
  filter(rank_h <= limit_per_h) %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5)),
         fc_obj = pmap(list(model_function, variables, lags, fit),
                            ~ forecast_VAR_Arima(model_function = ..1, 
                                                 variables = ..2, lags = ..3,
                                                 fit = ..4)
                            ),
         fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
         fc_yoy = map2(model_function, fc_mean, 
                       ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
                                    fc_ts = .y)),
         one_model_w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                     ~ subset(..1 * ..2, start = ..3, end = ..3)
                     ) 
                        
  ) %>% 
  ungroup()

moo <- foo %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



foo





foo3 <- models_rmse_at_each_h  %>%  
  filter(rank_h <= 3) %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5)),
         fc_obj = pmap(list(model_function, variables, lags, fit),
                       ~ forecast_VAR_Arima(model_function = ..1, 
                                            variables = ..2, lags = ..3,
                                            fit = ..4)
         ),
         fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
         diff_fc_mean = map(fc_mean, ~ logyoy(.)),
         fc_yoy = map2(model_function, fc_mean, 
                       ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
                                    fc_ts = .y)),
         w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                     ~ subset(..1 * ..2, start = ..3, end = ..3)
         )
         
  ) %>% 
  ungroup()

moo3 <- foo3 %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum))


arima_original_fcs 

foo31 <- foo3[1,]
foo31$fit
foo31$fc_obj





loo3 <- foo3$diff_fc_mean
boo3 <- foo3$fc_yoy

loo3[[1]]
boo3[[1]]



foo10 <- models_rmse_at_each_h  %>%  
  filter(rank_h <= 10) %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5)),
         fc_obj = pmap(list(model_function, variables, lags, fit),
                       ~ forecast_VAR_Arima(model_function = ..1, 
                                            variables = ..2, lags = ..3,
                                            fit = ..4)
         ),
         fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
         diff_fc_mean = map(fc_mean, ~ logyoy(.)),
         fc_yoy = map2(model_function, fc_mean, 
                       ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
                                    fc_ts = .y)),
         w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                     ~ subset(..1 * ..2, start = ..3, end = ..3)
         )
         
  ) %>% 
  ungroup()

moo10 <- foo10 %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum))




vars10 <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  arrange(rmse_1) %>% mutate(index = 1:n()) %>% 
  gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
  mutate(inv_mse = 1/rmse^2) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup() %>%  
  filter(rank_h <= 10) %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5)),
         fc_obj = pmap(list(model_function, variables, lags, fit),
                       ~ forecast_VAR_Arima(model_function = ..1, 
                                            variables = ..2, lags = ..3,
                                            fit = ..4)
         ),
         fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
         diff_fc_mean = map(fc_mean, ~ logyoy(.)),
         fc_yoy = map2(model_function, fc_mean, 
                       ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
                                    fc_ts = .y)),
         w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                     ~ subset(..1 * ..2, start = ..3, end = ..3)
         )
         
  ) %>% 
  ungroup()

summary10 <- vars10 %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum))


vars30 <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  arrange(rmse_1) %>% mutate(index = 1:n()) %>% 
  gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
  mutate(inv_mse = 1/rmse^2) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup() %>%  
  filter(rank_h <= 30) %>% 
  group_by(rmse_h) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         horizon = as.numeric(substr(rmse_h, 6, 6)),
         fit = pmap(list(model_function, variables, lags, arima_order, arima_seasonal),
                    ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                    lags = ..3, order = ..4, seasonal = ..5)),
         fc_obj = pmap(list(model_function, variables, lags, fit),
                       ~ forecast_VAR_Arima(model_function = ..1, 
                                            variables = ..2, lags = ..3,
                                            fit = ..4)
         ),
         fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
         diff_fc_mean = map(fc_mean, ~ logyoy(.)),
         fc_yoy = map2(model_function, fc_mean, 
                       ~ fc_log2yoy(model = .x, rgdp_log_ts = rgdp_ts_in_arima, 
                                    fc_ts = .y)),
         w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                     ~ subset(..1 * ..2, start = ..3, end = ..3)
         )
         
  ) %>% 
  ungroup()

summary30 <- vars30 %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum))

