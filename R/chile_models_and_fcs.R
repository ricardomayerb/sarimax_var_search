source('./R/utils_av.R')

models_and_accu <- readRDS("./data/Chile_by_step_12345.rds")
cv_objects <- readRDS("./data/Chile_by_step_12345_cv_objects.rds")
from_sarima <- readRDS(file = "./data/sarimax_objects_Chile.rds")

cv_objects <- cv_objects %>% mutate(id = 1:n())
models_and_accu <- models_and_accu %>% mutate(id = 1:n())

rmse_yoy_sarimax <- from_sarima$compare_rmse_yoy %>% mutate(id = 1:n())
rmse_level_sarimax <- from_sarima$compare_rmse %>% mutate(id = 1:n())

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
  mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
  dplyr::select(-c(rank_1, rank_2, rank_3, rank_4, rank_5, rank_6))

each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
  mutate(model_function = "Arima") %>% 
  dplyr::select(variable, lag, id, yoy_rmse_1, yoy_rmse_2, yoy_rmse_3, yoy_rmse_4, yoy_rmse_5, yoy_rmse_6, arima_order, arima_seasonal, model_function) %>% 
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





h_max <- 6
limit_per_h <- 30

# my_arima_one_x <- function(y_ts, y_order, y_seasonal, xreg_lags, x_name,
#                            xreg_data = NULL)


extended_x_data_ts <- from_sarima$extended_x_data_ts
rgdp_ts_in_arima <- from_sarima[["rgdp_ts_in_arima"]]
VAR_data <- readRDS("./data/VAR_data_Chile.rds")


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
  if(model == "VAR") {
    fc_mean <- fc_obj[["forecast"]][["rgdp"]][["mean"]]
  }
  
  if(model == "Arima") {
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
         w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                     ~ subset(..1 * ..2, start = ..3, end = ..3)
                     )
                        
  ) %>% 
  ungroup()

moo <- foo %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum))






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




vars20 <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  arrange(rmse_1) %>% mutate(index = 1:n()) %>% 
  gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
  mutate(inv_mse = 1/rmse^2) %>% 
  group_by(rmse_h) %>% 
  mutate(rank_h = rank(rmse)) %>% 
  ungroup() %>%  
  filter(rank_h <= 20) %>% 
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

cv_20 <- left_join(vars20, cv_objects, by = c("id")) %>% 
  select(variables.x, lags.x, id, rmse, horizon, cv_errors) %>% 
  rename(variables = variables.x, lags = lags.x)

selected_model_list <- list()
horizon_names <- c("h = 1", "h = 2", "h = 3", "h = 4", "h = 5", "h = 6")

for (i in 1:6){
  cv_20_h <- cv_20 %>% filter(horizon == i) %>% 
    arrange(rmse) %>% 
    mutate(cv_errors_all_h_mat = map(cv_errors, ~ reduce(., rbind)),
           cv_errors_this_h = map(cv_errors_all_h_mat, ~ .[,i]))
  
  just_errors_h <- cv_20_h %>% select(cv_errors_this_h)
  
  poo_h <- reduce(just_errors_h[[1]], rbind)
  row.names(poo_h) <- cv_20_h$id
  colnames(poo_h) <- paste0("cv_", 1:8)
  poo_h <- t(poo_h)
  poo_h <- as_data_frame(poo_h)
  poo_cor_h <- cor(poo_h)
  neg_poo_1 <- poo_cor_h[1, ] < 0
  min_poo_1 <-  min(min(poo_cor_h[1, ] ))
  neg_poo_2 <- poo_cor_h[2, ] < 0
  min_poo_2 <-  min(min(poo_cor_h[2, ] ))
  neg_poo_3 <- poo_cor_h[3, ] < 0
  min_poo_3 <-  min(min(poo_cor_h[3, ] )) 
  neg_poo_4 <- poo_cor_h[4, ] < 0
  min_poo_4 <-  min(min(poo_cor_h[4, ] ))
  neg_poo_5 <- poo_cor_h[5, ] < 0
  min_poo_5 <-  min(min(poo_cor_h[5, ] ))
  a1 <- names(poo_h)[neg_poo_1] 
  b1 <- names(poo_h)[poo_cor_h[1,] == min_poo_1]
  a2 <- names(poo_h)[neg_poo_2] 
  b2 <- names(poo_h)[poo_cor_h[2,] == min_poo_2]
  a3 <- names(poo_h)[neg_poo_3]
  b3 <- names(poo_h)[poo_cor_h[3,] == min_poo_3]
  a4 <- names(poo_h)[neg_poo_4]
  b4 <- names(poo_h)[poo_cor_h[4,] == min_poo_4]
  a5 <- names(poo_h)[neg_poo_5]
  b5 <- names(poo_h)[poo_cor_h[5,] == min_poo_5]
  
  selected_model_1 <- ifelse(sum(neg_poo_1)==0, "", b1 )
  selected_model_2 <- ifelse(sum(neg_poo_2)==0, "", b2 )
  selected_model_3 <- ifelse(sum(neg_poo_3)==0, "", b3 )
  selected_model_4 <- ifelse(sum(neg_poo_4)==0, "", b4 )
  selected_model_5 <- ifelse(sum(neg_poo_5)==0, "", b5 )
  
  selected_model_list[[i]] <- as.vector(c(selected_model_1, selected_model_2, selected_model_3, selected_model_4, 
                                                  selected_model_5))
  
  # selected_model_list[[i]] <- horizon_names[i]
  
}

names(selected_model_list) <- c("h = 1", "h = 2", "h = 3", "h = 4", "h = 5", "h = 6")
selected_model_list <- map(selected_model_list, ~ unique(as.numeric(.)) )

rmse_h_vector <- c("rmse_1", "rmse_2", "rmse_3", "rmse_4", "rmse_5", "rmse_6")
foo_list <- list()
rmse_h_vector[1]

for (i in 1:6) {
  h = rmse_h_vector[i]
  foo <- vars20 %>% filter(rmse_h == h) %>% mutate(rank_h = rank(rmse)) %>% arrange(rank_h) %>% 
    filter(rank_h <= 5) %>% select(id)
  foo <- as.vector(foo$id)
  foo <- as.numeric(foo); foo
  
  foo_list[[i]] <- foo
  
}

names(foo_list) <- c("h = 1", "h = 2", "h = 3", "h = 4", "h = 5", "h = 6")

list3 <- map2(selected_model_list, foo_list, ~ c(.x, .y))
list3 <- map(list3, unique)


# drop the na values to precent issues later
# must be able to map this but somehow i cant map
for (i in 1:6){
  h <- horizon_names[i]
  df <- list3[[h]]
  df <- df[!is.na(df)]
  list3[[i]] <- df
}

tibble_best_neg_cor <- tibble()
for (i in 1:6) {
  list_to_tibble <- tibble(horizon = i, id = list3[[i]])
  tibble_best_neg_cor <- rbind(tibble_best_neg_cor, list_to_tibble)
}


vars_antithetical_20 <- left_join(tibble_best_neg_cor, 
                                  vars20, by = c("horizon", "id")) %>% 
  group_by(horizon) %>% 
  mutate(sum_invmse_h = sum(inv_mse),
         model_weight_h = inv_mse/sum_invmse_h,
         w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                     ~ subset(..1 * ..2, start = ..3, end = ..3)
         )
  ) %>% 
  ungroup()

summ_anti_vars20 <- vars_antithetical_20 %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum))


# summary10 <- vars10 %>% 
#   group_by(horizon) %>% 
#   summarise(sum_one_h = reduce(w_fc, sum))
# 
# cv_10 <- left_join(vars10, cv_objects, by = c("id")) %>% 
#   select(variables.x, lags.x, id, rmse, horizon, cv_errors) %>% 
#   rename(variables = variables.x, lags = lags.x)
# 
# 
# cv_10_1 <- cv_10 %>% filter(horizon == 1) %>% 
#   arrange(rmse) %>% 
#   mutate(cv_errors_all_h_mat = map(cv_errors, ~ reduce(., rbind)),
#          cv_errors_this_h = map(cv_errors_all_h_mat, ~ .[,1]))
# 
# cv_20 <- left_join(vars20, cv_objects, by = c("id")) %>% 
#   select(variables.x, lags.x, id, rmse, horizon, cv_errors) %>% 
#   rename(variables = variables.x, lags = lags.x)
# 
# 
# summary20 <- vars20 %>% 
#   group_by(horizon) %>% 
#   summarise(sum_one_h = reduce(w_fc, sum))
# 
# row.names(summary20) <- c("2017 Q4", "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4", "2019 Q1")
# zoo <- (summary20[2:5,2])
# Economic_Growth_2018_best_20 <- mean(zoo$sum_one_h); Economic_Growth_2018_best_50
# 
# plot(summary20)
# 
# summary3 <- vars3 %>% 
#   group_by(horizon) %>% 
#   summarise(sum_one_h = reduce(w_fc, sum))
# 
# summary30 <- vars30 %>% 
#   group_by(horizon) %>% 
#   summarise(sum_one_h = reduce(w_fc, sum))
# 
# summary50 <- vars50 %>% 
#   group_by(horizon) %>% 
#   summarise(sum_one_h = reduce(w_fc, sum))
# 
# 
# row.names(summary10) <- c("2017 Q4", "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4", "2019 Q1")
# zoo <- (summary10[2:5,2])
# Economic_Growth_2018 <- mean(zoo$sum_one_h); Economic_Growth_2018
# 
# row.names(summary3) <- c("2017 Q4", "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4", "2019 Q1")
# zoo <- (summary3[2:5,2])
# Economic_Growth_2018_best_3 <- mean(zoo$sum_one_h); Economic_Growth_2018_best_3
# 
# row.names(summary30) <- c("2017 Q4", "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4", "2019 Q1")
# zoo <- (summary30[2:5,2])
# Economic_Growth_2018_best_30 <- mean(zoo$sum_one_h); Economic_Growth_2018_best_30
# 
# row.names(summary50) <- c("2017 Q4", "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4", "2019 Q1")
# zoo <- (summary50[2:5,2])
# Economic_Growth_2018_best_50 <- mean(zoo$sum_one_h); Economic_Growth_2018_best_50
# 
# 
# cv_errors <- cv_objects$cv_errors
# cv_errors <- map(cv_errors, ~ reduce(., rbind))
# map(cv_errors, cor)
# 
# 
