library(tidyverse)

models_and_accu <- readRDS("./data/Colombia_by_step_12345.rds")
cv_objects <- readRDS("./data/Colombia_by_step_12345_cv_objects.rds")
from_sarima <- readRDS(file = "./data/sarimax_objects_Colombia.rds")

# add_column_cv_yoy_errors <- function(data = cv_objects){
#   
#   cv_errors_yoy <- list_along(1:nrow(data))
#   
#   for (i in 1:nrow(data)) {
#     data_one_row <- data[i,]
#     test_data_yoy <- data_one_row[["cv_test_data_yoy"]][[1]]
#     fcs_yoy <- data_one_row[["cv_fcs_yoy"]][[1]]
#     errors_yoy <- map2(test_data_yoy, fcs_yoy, ~ .x - .y)
#     
#     cv_errors_yoy[[i]] <- errors_yoy
#     
#   }
#   
#   data$cv_errors_yoy <- cv_errors_yoy 
#   
#   return(data)
#   
# }
# 
# cv_objects <- add_column_cv_yoy_errors(cv_objects)
# 
# get_rmse_var_table_at_each_h_diff_yoy <- function(data = cv_objects){
#   cv_errors <- cv_objects[["cv_errors_yoy"]]
#   
#   all_rmses <- map(cv_errors, function(x) sqrt(colMeans( (reduce(x, rbind))^2))  )
#   all_rmses_tbl <- reduce(all_rmses, rbind)
#   rmse_names <- paste0("rmse_", 1:6)
#   colnames(all_rmses_tbl) <- rmse_names
#   row.names(all_rmses_tbl) <- NULL
#   rmse_each_h <- cbind(cv_objects, all_rmses_tbl)
#   rmse_each_h$cv_errors <- NULL
#   rmse_each_h$accu_yoy <- NULL
#   rmse_each_h$cv_test_data_yoy <- NULL
#   rmse_each_h$cv_fcs_yoy <- NULL
#   rmse_each_h$accu_lev <- NULL
#   rmse_each_h$cv_test_data_lev <- NULL
#   rmse_each_h$cv_fcs_lev <- NULL
#   rmse_each_h$cv_errors_yoy <- NULL
#   
#   return(rmse_each_h)
#   
# }
# 
# with_rmses <- get_rmse_var_table_at_each_h_diff_yoy(data = cv_objects)



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

# rmse_yoy_sarimax$ave_rmse_h1h8 <- rowMeans(cbind(rmse_yoy_sarimax$yoy_rmse_1, 
#                                                  rmse_yoy_sarimax$yoy_rmse_2, 
#                                                  rmse_yoy_sarimax$yoy_rmse_3, 
#                                                  rmse_yoy_sarimax$yoy_rmse_4, 
#                                                  rmse_yoy_sarimax$yoy_rmse_5, 
#                                                  rmse_yoy_sarimax$yoy_rmse_6, 
#                                                  rmse_yoy_sarimax$yoy_rmse_7, 
#                                                  rmse_yoy_sarimax$yoy_rmse_8)) 
# 
# rmse_level_sarimax$ave_rmse_h1h8 <- rowMeans(cbind(rmse_level_sarimax$level_rmse_1, 
#                                                    rmse_level_sarimax$level_rmse_2, 
#                                                    rmse_level_sarimax$level_rmse_3, 
#                                                    rmse_level_sarimax$level_rmse_4, 
#                                                    rmse_level_sarimax$level_rmse_5, 
#                                                    rmse_level_sarimax$level_rmse_6, 
#                                                    rmse_level_sarimax$level_rmse_7, 
#                                                    rmse_level_sarimax$level_rmse_8)) 
# 
# rmse_level_sarimax$ave_rmse_h1h8_stata <- rowMeans(cbind(rmse_level_sarimax$rmse1, 
#                                                          rmse_level_sarimax$rmse2, 
#                                                          rmse_level_sarimax$rmse3, 
#                                                          rmse_level_sarimax$rmse4, 
#                                                          rmse_level_sarimax$rmse5, 
#                                                          rmse_level_sarimax$rmse6, 
#                                                          rmse_level_sarimax$rmse7, 
#                                                          rmse_level_sarimax$rmse8)) 

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

# just_model_and_ave_rmse_2_stata <- rmse_yoy_sarimax %>% 
#   dplyr::select(variable, lag, ave_rmse_h1h6) %>% 
#   rename(variables = variable, accu_yoy = ave_rmse_h1h6, lags = lag)
# 
# model_and_ave_rmse_r <- rbind(just_model_and_ave_rmse_1, 
#                               just_model_and_ave_rmse_2) %>% 
#   arrange(accu_yoy)

########## RMSE at each H
each_h_just_model_and_ave_rmse_var <- models_and_accu %>% 
  mutate(arima_order = NA, arima_seasonal = NA) %>% 
  dplyr::select(-c(rank_1, rank_2, rank_3, rank_4, rank_5, rank_6))

each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, yoy_rmse_1, yoy_rmse_2, yoy_rmse_3, yoy_rmse_4, yoy_rmse_5, yoy_rmse_6, arima_order, arima_seasonal) %>% 
  rename(variables = variable, rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, 
         rmse_5 = yoy_rmse_5, rmse_6 = yoy_rmse_6, lags = lag)

each_h_model_and_ave_rmse_r <- rbind(each_h_just_model_and_ave_rmse_var, 
                                     each_h_just_model_and_ave_rmse_sarimax)

# # h = 1
# 
# h1_just_model_and_ave_rmse_var <- each_h_just_model_and_ave_rmse_var  %>% 
#   dplyr::select(variables, lags, rmse_1, arima_order, arima_seasonal)
# 
# h1_just_model_and_ave_rmse_sarimax <- each_h_just_model_and_ave_rmse_sarimax %>% 
#   dplyr::select(variables, lags, rmse_1, arima_order, arima_seasonal)
# 
# h1_model_and_ave_rmse_r <- rbind(h1_just_model_and_ave_rmse_var, 
#                                      h1_just_model_and_ave_rmse_sarimax) %>% 
#   arrange(rmse_1)
# 
# # h = 2
# 
# h2_just_model_and_ave_rmse_var <- each_h_just_model_and_ave_rmse_var  %>% 
#   dplyr::select(variables, lags, rmse_2, arima_order, arima_seasonal)
# 
# h2_just_model_and_ave_rmse_sarimax <- each_h_just_model_and_ave_rmse_sarimax %>% 
#   dplyr::select(variables, lags, rmse_2, arima_order, arima_seasonal)
# 
# h2_model_and_ave_rmse_r <- rbind(h2_just_model_and_ave_rmse_var, 
#                                  h2_just_model_and_ave_rmse_sarimax) %>% 
#   arrange(rmse_2)
# 
# # h = 3
# 
# h3_just_model_and_ave_rmse_var <- each_h_just_model_and_ave_rmse_var  %>% 
#   dplyr::select(variables, lags, rmse_3, arima_order, arima_seasonal)
# 
# h3_just_model_and_ave_rmse_sarimax <- each_h_just_model_and_ave_rmse_sarimax %>% 
#   dplyr::select(variables, lags, rmse_3, arima_order, arima_seasonal)
# 
# h3_model_and_ave_rmse_r <- rbind(h3_just_model_and_ave_rmse_var, 
#                                  h3_just_model_and_ave_rmse_sarimax) %>% 
#   arrange(rmse_3)
# 
# # h = 4
# 
# h4_just_model_and_ave_rmse_var <- each_h_just_model_and_ave_rmse_var  %>% 
#   dplyr::select(variables, lags, rmse_4, arima_order, arima_seasonal)
# 
# h4_just_model_and_ave_rmse_sarimax <- each_h_just_model_and_ave_rmse_sarimax %>% 
#   dplyr::select(variables, lags, rmse_4, arima_order, arima_seasonal)
# 
# h4_model_and_ave_rmse_r <- rbind(h4_just_model_and_ave_rmse_var, 
#                                  h4_just_model_and_ave_rmse_sarimax) %>% 
#   arrange(rmse_4)
# 
# # h = 5
# 
# h5_just_model_and_ave_rmse_var <- each_h_just_model_and_ave_rmse_var  %>% 
#   dplyr::select(variables, lags, rmse_5, arima_order, arima_seasonal)
# 
# h5_just_model_and_ave_rmse_sarimax <- each_h_just_model_and_ave_rmse_sarimax %>% 
#   dplyr::select(variables, lags, rmse_5, arima_order, arima_seasonal)
# 
# h5_model_and_ave_rmse_r <- rbind(h5_just_model_and_ave_rmse_var, 
#                                  h5_just_model_and_ave_rmse_sarimax) %>% 
#   arrange(rmse_5)
# 
# # h = 6
# 
# h6_just_model_and_ave_rmse_var <- each_h_just_model_and_ave_rmse_var  %>% 
#   dplyr::select(variables, lags, rmse_6, arima_order, arima_seasonal)
# 
# h6_just_model_and_ave_rmse_sarimax <- each_h_just_model_and_ave_rmse_sarimax %>% 
#   dplyr::select(variables, lags, rmse_6, arima_order, arima_seasonal)
# 
# h6_model_and_ave_rmse_r <- rbind(h6_just_model_and_ave_rmse_var, 
#                                  h6_just_model_and_ave_rmse_sarimax) %>% 
#   arrange(rmse_6)
# 
