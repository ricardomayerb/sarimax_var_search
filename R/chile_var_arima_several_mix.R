source('./R/utils_av.R')
# library(zoo)
# library(here)
# library(printr)

country_name <- "Chile"

# Estimate and Save new Arimax RDS file
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

make_models_tbl <- function(arima_res, var_models_and_rmse, VAR_data, h_max,
                            ave_rmse_sel = FALSE) {
  
  rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
  rmse_level_sarimax <- arima_res$compare_rmse
  v_lags_order_season <- arima_res$var_lag_order_season 
  extended_x_data_ts <- arima_res$mdata_ext_ts
  rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
  
  
  rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
    left_join(v_lags_order_season, by = c("variable", "lag"))
  
  
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
  
  if (ave_rmse_sel) {
    models_rmse_at_each_h_arima  <- as_tibble(
      each_h_just_model_and_ave_rmse_sarimax) %>% 
      mutate(ave_rmse = rowMeans(select(., starts_with("rmse")))) %>% 
      group_by(variables) %>%
      mutate(min_ave_per_variable = min(ave_rmse)) %>% 
      filter(ave_rmse == min_ave_per_variable) %>% 
      ungroup() %>% 
      gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
      ungroup() %>% 
      group_by(rmse_h) %>% 
      mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
      filter(rmse <= rgdp_rmse) %>% 
      ungroup() %>% 
      select(-c(ave_rmse, rgdp_rmse, min_ave_per_variable)) %>% 
      arrange(rmse_h, variables)
    
  } else {
    models_rmse_at_each_h_arima <- as_tibble(
      each_h_just_model_and_ave_rmse_sarimax) %>% 
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
  }
   
  models_rmse_at_each_h_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
    gather(key = "rmse_h", value = "rmse", starts_with("rmse"))
  
  models_rmse_at_each_h <- rbind(models_rmse_at_each_h_var, 
                                 models_rmse_at_each_h_arima) %>% 
    mutate(inv_mse = 1/rmse^2) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) %>% 
    arrange(rmse_h, rank_h)
  
  return(models_rmse_at_each_h)
  
  
}

# Where is ssel standing for?
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

# we are estimating VARs, is it a problem that rgdp_ts_in_arima
# maybe turn this into a function maybe?
ffall_VAR <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                           h = h_max, extended_x_data_ts = extended_x_data_ts,
                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                           model_type = "VAR", max_rank_h = 30)

summ_all_VAR <- ffall_VAR %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

ffall_VAR_10 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                h = h_max, extended_x_data_ts = extended_x_data_ts,
                                rgdp_ts_in_arima = rgdp_ts_in_arima,
                                model_type = "VAR", max_rank_h = 10)

summ_all_VAR_10 <- ffall_VAR_10 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

ffall_VAR_5 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                   h = h_max, extended_x_data_ts = extended_x_data_ts,
                                   rgdp_ts_in_arima = rgdp_ts_in_arima,
                                   model_type = "VAR", max_rank_h = 5)

summ_all_VAR_5 <- ffall_VAR_5 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

ffall_VAR_15 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                  h = h_max, extended_x_data_ts = extended_x_data_ts,
                                  rgdp_ts_in_arima = rgdp_ts_in_arima,
                                  model_type = "VAR", max_rank_h = 15)

summ_all_VAR_15 <- ffall_VAR_15 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

ffall_VAR_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                   h = h_max, extended_x_data_ts = extended_x_data_ts,
                                   rgdp_ts_in_arima = rgdp_ts_in_arima,
                                   model_type = "VAR", max_rank_h = 20)

summ_all_VAR_20 <- ffall_VAR_20 %>% 
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

### Play Around with Graphs
# Our average forecasts in the summ_ tables are numeric and not in time-series form
# what I want is to bind these forecast with the rgdp ts series for a nice graph

# Take the forecasts from the best n models summary
# fcs <- summ_all_20$sum_one_h
# The forecasts are numeric, we need to turn it into a timseries before we can ot 
# autoplot(rgdp, fcs)
# plot(fcs)
# class(fcs)

# VAR Data 
rgdp_var <- VAR_data[ ,1]
autoplot(rgdp_var)

start_ts_fcs_var_year <- year(as.yearqtr(last(time(rgdp_var)))) # use year from lubridate. Time gives you the time like 2017.5 etc
# last gives you the last observation. as.yearqtr turns it into 2017 Q3 form and year gives the year
start_ts_fcs_var_qtr <- quarter(as.yearqtr(last(time(rgdp_var)))) # use quarter from lubridate

# summ_all_20 add an extra row with horizon = 0 and sum_one_h is the last observation of rgdp_var
summ_all_VAR_5 <- summ_all_VAR_5 %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)
summ_all_VAR_10 <- summ_all_VAR_10 %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)
summ_all_VAR_15 <- summ_all_VAR_15 %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)
summ_all_VAR_20 <- summ_all_VAR_20 %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)

# note that i usee the arimax summary now, i should replace that for VAR
fcs_var_ts_best5 <- ts(data = summ_all_VAR_5$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))
fcs_var_ts_best10 <- ts(data = summ_all_VAR_10$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))
fcs_var_ts_best15 <- ts(data = summ_all_VAR_15$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))
fcs_var_ts_best20 <- ts(data = summ_all_VAR_20$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))

rgdp_var_and_fcs <- ts.union(rgdp_var, fcs_var_ts_best5, fcs_var_ts_best10, fcs_var_ts_best15, fcs_var_ts_best20)

forecast_plot_best_vars <- autoplot(rgdp_var) + 
  autolayer(fcs_var_ts_best5, series="Best 5 VARs") + 
  autolayer(fcs_var_ts_best10, series="Best 10 VARs") +
  autolayer(fcs_var_ts_best15, series="Best 15 VARs") +
  autolayer(fcs_var_ts_best20, series="Best 20 VARs") +
  ggtitle("GDP Forecast VARs") +
  xlab("Year") +
  ylab("Real GDP Growth YoY (%)") +
  guides(colour=guide_legend(title="Time-Series:"))

forecast_plot_best_vars
# autoplot(rgdp_var_and_fcs) + 
#   ggtitle("GDP Forecast VARs") +
#   xlab("Year") +
#   ylab("Real GDP Growth YoY (%)") +
#   guides(colour=guide_legend(title="Time-Series:"))

# Arimax Data
rgdp_arimax <- make_yoy_ts(exp(rgdp_ts_in_arima))
last(time(rgdp_arimax))

# select similar start date for rgdp_arimax as for rgdp_var so that graph is comparable
# first_year_var_ts is used to set the start of arimax series equal to the var series. Besides that we do not use it
first_year_var_ts <- year(as.yearqtr(first(time(rgdp_var))))
first_qtr_var_ts <- quarter(as.yearqtr(first(time(rgdp_var))))
# rgdp_arimax <- window(rgdp_arimax, start = c(first_year_var_ts, first_qtr_var_ts))

start_ts_fcs_arimax_year <- year(as.yearqtr(last(time(rgdp_arimax)))) 
start_ts_fcs_arimax_qtr <- quarter(as.yearqtr(last(time(rgdp_arimax))))

# summ_all_20 add an extra row with horizon = 0 and sum_one_h is the last observation of rgdp_arimax
summ_all_20 <- summ_all_20 %>% add_row(horizon = 0, sum_one_h = last(rgdp_arimax)) %>% arrange(horizon)

fcs_arimax_ts <- ts(data = summ_all_20$sum_one_h, frequency = 4, start = c(start_ts_fcs_arimax_year, start_ts_fcs_arimax_qtr))
end_ts_fcs_var_year <- year(as.yearqtr(last(time(fcs_arimax_ts)))) 
end_ts_fcs_var_qtr <- quarter(as.yearqtr(last(time(fcs_arimax_ts)))) 

# Two ways to combine the time-series. 1) Use autolayer, which adds a layer to an existing plot, or 2) ts.union the time-series and then plot them
forecast_plot_best_arimax <- autoplot(window(rgdp_arimax, start = c(first_year_var_ts, first_qtr_var_ts), end = c(end_ts_fcs_var_year, end_ts_fcs_var_qtr))) + 
  autolayer(fcs_arimax_ts, series="Forecasts Arimax") + 
  ggtitle("GDP Forecast Arimax") +
  xlab("Year") +
  ylab("Real GDP Growth YoY (%)") +
  guides(colour=guide_legend(title="Time-Series:"))

rgdp_arimax_and_fcs <- ts.union(rgdp_arimax, fcs_arimax_ts)
rgdp_var_and_arimax_fcs <- ts.union(rgdp_var_and_fcs, rgdp_arimax_and_fcs)

# gdp_forecast_plot_arimax_union <- autoplot(window(rgdp_arimax_and_fcs, start = c(first_year_var_ts, first_qtr_var_ts), end = c(end_ts_fcs_var_year, end_ts_fcs_var_qtr))) + 
#   ggtitle("GDP Forecast Arimax") +
#   xlab("Year") +
#   ylab("Real GDP Growth YoY (%)") +
#   guides(colour=guide_legend(title="Time-Series:"))
# gdp_forecast_plot_arimax_union

forecast_plot_best_vars_arimax <- autoplot(window(rgdp_arimax, start = c(first_year_var_ts, first_qtr_var_ts), end = c(end_ts_fcs_var_year, end_ts_fcs_var_qtr))) + 
  autolayer(fcs_arimax_ts, series="Forecasts Best Arimax") + 
  autolayer(fcs_var_ts_best20, series="Forecasts Best 20 VARs") +
  autolayer(fcs_var_ts_best5, series="Forecasts Best 5 VARs") +
  ggtitle("GDP Forecast Best VARs and ARIMAX") +
  xlab("Year") +
  ylab("Real GDP Growth YoY (%)") +
  guides(colour=guide_legend(title="Time-Series:"))

forecast_plot_best_vars_arimax
