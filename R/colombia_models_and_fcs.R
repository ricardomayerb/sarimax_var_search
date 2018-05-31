library(vars)
library(forecast)
library(tidyverse)

models_and_accu <- readRDS("./data/Colombia_by_step_12345.rds")
cv_objects <- readRDS("./data/Colombia_by_step_12345_cv_objects.rds")
from_sarima <- readRDS(file = "./data/sarimax_objects_Colombia.rds")



rmse_yoy_sarimax <- from_sarima$compare_rmse_yoy
rmse_level_sarimax <- from_sarima$compare_rmse

v_lags_order_season <- from_sarima$var_lag_order_season 
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
  mutate(arima_order = NA, arima_seasonal = NA, model_type = "VAR") %>% 
  dplyr::select(-c(rank_1, rank_2, rank_3, rank_4, rank_5, rank_6))

each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
  mutate(model_type = "arima") %>% 
  dplyr::select(variable, lag, yoy_rmse_1, yoy_rmse_2, yoy_rmse_3, yoy_rmse_4, yoy_rmse_5, yoy_rmse_6, arima_order, arima_seasonal, model_type) %>% 
  rename(variables = variable, rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, 
         rmse_5 = yoy_rmse_5, rmse_6 = yoy_rmse_6, lags = lag)

models_rmse_at_each_h <- as_tibble(rbind(each_h_just_model_and_ave_rmse_var, 
                                     each_h_just_model_and_ave_rmse_sarimax))


VAR_data <- readRDS("./data/VAR_data_Colombia.rds")
h_max <- 6

indiv_rmse_fit_fcs_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  mutate(fit = map2(variables, lags, ~ VAR(y = VAR_data[,.x], p = .y)),
         indiv_fc = map(fit, ~ forecast(., h = h_max)))





moo <- as_tibble(each_h_just_model_and_ave_rmse_sarimax) %>%
  mutate(fit = ifelse(model_type == "VAR",
                      map2(variables, lags, ~ VAR(y = VAR_data[,.x], p = .y)),
                      666))


