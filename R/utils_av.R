library(MTS)
library(vars)
library(xts)
library(tibbletime)
library(readxl)
library(timetk)
library(lubridate)
library(forecast)
library(gridExtra)
library(grid)
library(haven)
library(tictoc)
library(tidyverse)
library(tsibble)
library(broom)
library(sweep)
library(gridExtra)


add_average_fcs <- function(var_fc_tbl, n_ave = c(1, 3, 5)) {
  just_fcs <- var_fc_tbl %>% dplyr::select(fc_rgdp_mean, tibble_id)
  new_just_fcs <- just_fcs
  j_names <- names(just_fcs)
  
  fc_h <- length(var_fc_tbl$fc_rgdp_mean[[1]])
  
  ts_start <- (var_fc_tbl %>%
                 mutate(st = map(fc_rgdp_mean, start)) %>%
                 dplyr::select(st))[[1, 1]]
  
  var_all_with_rankings <- var_fc_tbl %>%
    arrange(RMSE) %>%
    mutate(rmse_ranking = 1:n()) %>%
    arrange(MAE) %>%
    mutate(mae_ranking = 1:n()) %>%
    arrange(Theil) %>%
    mutate(theil_ranking = 1:n())
  
  
  do_list_ave <- function(sorted_tbl) {
    rgdp_fc_as_matrix <- sorted_tbl %>%
      dplyr::select(fc_7_rgdp_mean) %>%
      unlist() %>%
      matrix(ncol = fc_h, byrow = TRUE)
    
    fc_colmeans <- colMeans(rgdp_fc_as_matrix)
    
    return(fc_colmeans)
  }
  
  for (i in 1:length(n_ave)) {
    rmse_id <- paste("ave_rmse", n_ave[i], sep = "_")
    mae_id <- paste("ave_mae", n_ave[i], sep = "_")
    theil_id <- paste("ave_theil", n_ave[i], sep = "_")
    ave_of_ave_id <- paste("ave_r_m_t", n_ave[i], sep = "_")
    
    this_ave_rmse <- var_all_with_rankings %>%
      filter(rmse_ranking <= n_ave[i]) %>%
      do_list_ave()
    
    this_ave_mae <- var_all_with_rankings %>%
      filter(mae_ranking <= n_ave[i]) %>%
      do_list_ave()
    
    this_ave_theil <- var_all_with_rankings %>%
      filter(rmse_ranking <= n_ave[i]) %>%
      do_list_ave()
    
    this_ave_of_aves <- colMeans(rbind(
      this_ave_rmse, this_ave_mae,
      this_ave_theil
    ))
    
    this_ave_rmse_ts <- tk_ts(this_ave_rmse, start = ts_start, frequency = 4)
    this_ave_mae_ts <- tk_ts(this_ave_mae, start = ts_start, frequency = 4)
    this_ave_theil_ts <- tk_ts(this_ave_theil, start = ts_start, frequency = 4)
    this_ave_of_aves_ts <- tk_ts(this_ave_of_aves,
                                 start = ts_start,
                                 frequency = 4
    )
    
    this_ave_rmse_tbl <- tibble(list(this_ave_rmse_ts), rmse_id)
    names(this_ave_rmse_tbl) <- j_names
    
    this_ave_mae_tbl <- tibble(list(this_ave_mae_ts), mae_id)
    names(this_ave_mae_tbl) <- j_names
    
    this_ave_theil_tbl <- tibble(list(this_ave_theil_ts), theil_id)
    names(this_ave_theil_tbl) <- j_names
    
    this_ave_of_aves_tbl <- tibble(list(this_ave_of_aves_ts), ave_of_ave_id)
    names(this_ave_of_aves_tbl) <- j_names
    
    new_ave_fcs <- this_ave_of_aves_tbl %>%
      rbind(this_ave_rmse_tbl) %>%
      rbind(this_ave_mae_tbl) %>%
      rbind(this_ave_theil_tbl)
    
    new_just_fcs <- new_just_fcs %>% rbind(new_ave_fcs)
  }
  
  return(new_just_fcs)
}

add_column_cv_yoy_errors <- function(data = cv_objects){
  
  cv_errors_yoy <- list_along(1:nrow(data))
  
  for (i in 1:nrow(data)) {
    data_one_row <- data[i,]
    test_data_yoy <- data_one_row[["cv_test_data_yoy"]][[1]]
    fcs_yoy <- data_one_row[["cv_fcs_yoy"]][[1]]
    errors_yoy <- map2(test_data_yoy, fcs_yoy, ~ .x - .y)
    
    cv_errors_yoy[[i]] <- errors_yoy
    
  }
  data$cv_errors_yoy <- cv_errors_yoy 
  
  return(data)
}

aggregate_and_transform_fcs <- function(arimax_and_fcs, cv_cond_uncond,
                                        rgdp_ts, rgdp_uncond_fc_mean, 
                                        test_length = 8, h_max = 8,
                                        data_is_log_log = FALSE) {
  
  
  cv_all_x_rmse_each_h <- cv_cond_uncond$cv_all_x_rmse_each_h
  cv_all_x_rmse_each_h_yoy <- cv_cond_uncond$cv_all_x_rmse_each_h_yoy
  cv_all_x_rmse_each_h_logdiff <- cv_cond_uncond$cv_all_x_rmse_each_h_logdiff
  cv_all_x_rmse_each_h_percent <- cv_cond_uncond$cv_all_x_rmse_each_h_percent
  
  cv_rmse_each_h_rgdp <-  cv_cond_uncond[["cv_rmse_each_h_rgdp"]]
  cv_rmse_each_h_rgdp_yoy <-  cv_cond_uncond[["cv_rmse_each_h_rgdp_yoy"]]
  cv_rmse_each_h_rgdp_logdiff <-  cv_cond_uncond[["cv_rmse_each_h_rgdp_logdiff"]]
  cv_rmse_each_h_rgdp_percent <-  cv_cond_uncond[["cv_rmse_each_h_rgdp_percent"]]
  
  
  mat_of_raw_fcs <- arimax_and_fcs$mat_of_raw_fcs
  tbl_raw_fcs <- as_tibble(t(mat_of_raw_fcs))
  cond_names <- paste(cv_all_x_rmse_each_h_logdiff$variable,
                      cv_all_x_rmse_each_h_logdiff$lag, sep = "_")
  names(tbl_raw_fcs) <- cond_names
  tbl_raw_fcs$type <- "cond_fc"
  # tbl_raw_fcs$yq <-  as.yearqtr(time(rgdp_uncond_fc_mean))
  tbl_raw_fcs$date <-  date(as.yearqtr(time(rgdp_uncond_fc_mean)))
  
  tsbl_raw_fcs <- as_tsibble(tbl_raw_fcs, index = date, key = id(type))

  print("tbl_raw_fcs")
  print(tbl_raw_fcs)
  print("tsbl_raw_fcs")
  print(tsbl_raw_fcs)

  # print(as.yearqtr(time(rgdp_uncond_fc_mean)))
  # print(date(as.yearqtr(time(rgdp_uncond_fc_mean))))
  
  
  
  
  weigthed_fcs <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                   mat_cv_rmses_from_x = cv_all_x_rmse_each_h,
                                   vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp)
  
  weigthed_fcs_cv_yoy_errors <- get_weighted_fcs(
    raw_fcs = mat_of_raw_fcs, 
    mat_cv_rmses_from_x =  cv_all_x_rmse_each_h_yoy,
    vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_yoy)
  
  weigthed_fcs_cv_logdiff_errors <- get_weighted_fcs(
    raw_fcs = mat_of_raw_fcs, 
    mat_cv_rmses_from_x =  cv_all_x_rmse_each_h_logdiff,
    vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_logdiff)
  
  weigthed_fcs_cv_percent_errors <- get_weighted_fcs(
    raw_fcs = mat_of_raw_fcs, 
    mat_cv_rmses_from_x =  cv_all_x_rmse_each_h_percent,
    vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_percent)
  
  
  # weigthed_fcs[is.nan(weigthed_fcs)] <- rgdp_uncond_fc_mean[is.nan(weigthed_fcs)]
  # 
  # fcs_using_yoy_weights <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
  #                                           mat_cv_rmses_from_x = cv_all_x_rmse_each_h_yoy,
  #                                           vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_yoy)
  
  # fcs_using_yoy_weights[is.nan(fcs_using_yoy_weights)] <- rgdp_uncond_fc_mean[is.nan(fcs_using_yoy_weights)]
  
  weigthed_fcs <- ts(weigthed_fcs, 
                     start = stats::start(rgdp_uncond_fc_mean), 
                     frequency = 4)
  
  weigthed_fcs_cv_yoy_errors <- ts(weigthed_fcs_cv_yoy_errors, 
                     start = stats::start(rgdp_uncond_fc_mean), 
                     frequency = 4)
  
  weigthed_fcs_cv_logdiff_errors <- ts(weigthed_fcs_cv_logdiff_errors, 
                     start = stats::start(rgdp_uncond_fc_mean), 
                     frequency = 4)
  
  weigthed_fcs_cv_percent_errors <- ts(weigthed_fcs_cv_percent_errors, 
                     start = stats::start(rgdp_uncond_fc_mean), 
                     frequency = 4)
  
  rgdp_data_and_uncond_fc <- ts(data = c(rgdp_ts, rgdp_uncond_fc_mean), 
                                frequency = 4, start = stats::start(rgdp_ts))
  
  if (data_is_log_log) {
    yoy_rgdp_data_and_uncond_fc <- make_yoy_ts(exp(rgdp_data_and_uncond_fc))
  } else {
    yoy_rgdp_data_and_uncond_fc <- make_yoy_ts(rgdp_data_and_uncond_fc)
  }
  
  rgdp_uncond_yoy_fc_mean <- window(yoy_rgdp_data_and_uncond_fc,
                                    start = stats::start(rgdp_uncond_fc_mean))
  
  # fcs_using_yoy_weights <- ts(fcs_using_yoy_weights, 
  #                             start = stats::start(rgdp_uncond_fc_mean), 
  #                             frequency = 4)
  
  final_rgdp_and_w_fc <- ts(c(rgdp_ts, weigthed_fcs), frequency = 4,
                            start = stats::start(rgdp_ts))
  
  final_rgdp_and_w_fc_yoy_errors <- ts(c(rgdp_ts, weigthed_fcs_cv_yoy_errors), 
                                       frequency = 4,
                                       start = stats::start(rgdp_ts))
  
  final_rgdp_and_w_fc_logdiff_errors <- ts(c(rgdp_ts, weigthed_fcs_cv_logdiff_errors), 
                                       frequency = 4,
                                       start = stats::start(rgdp_ts))
  
  final_rgdp_and_w_fc_percent_errors <- ts(c(rgdp_ts, weigthed_fcs_cv_percent_errors),
                                       frequency = 4,
                                       start = stats::start(rgdp_ts))
  
  if (data_is_log_log) {
    final_rgdp_and_w_fc <- exp(final_rgdp_and_w_fc)
    final_rgdp_and_w_fc_yoy_errors <- exp(final_rgdp_and_w_fc_yoy_errors)
    final_rgdp_and_w_fc_logdiff_errors <- exp(final_rgdp_and_w_fc_logdiff_errors)
    final_rgdp_and_w_fc_percent_errors <- exp(final_rgdp_and_w_fc_percent_errors)
  }

  yoy_growth_rgdp_and_w_fc <- make_yoy_ts(final_rgdp_and_w_fc, is_log = FALSE)
  
  yoy_growth_rgdp_and_w_fc_yoy_errors <- make_yoy_ts(
    final_rgdp_and_w_fc_yoy_errors, is_log = FALSE)
  
  yoy_growth_rgdp_and_w_fc_logdiff_errors <- make_yoy_ts(
    final_rgdp_and_w_fc_logdiff_errors, is_log = FALSE)
  
  yoy_growth_rgdp_and_w_fc_percent_errors <- make_yoy_ts(
    final_rgdp_and_w_fc_percent_errors, is_log = FALSE)

  yoy_fc_using_weights <- yoy_growth_rgdp_and_w_fc
  yoy_fc_using_weights_yoy_errors <- yoy_growth_rgdp_and_w_fc_yoy_errors
  yoy_fc_using_weights_logdiff_errors <- yoy_growth_rgdp_and_w_fc_logdiff_errors
  yoy_fc_using_weights_percent_errors <- yoy_growth_rgdp_and_w_fc_percent_errors

  cv_rmse_level_rgdp_conditional_on_x <- cv_all_x_rmse_each_h
  cv_rmse_level_rgdp <- cv_rmse_each_h_rgdp
  
  cv_rmse_yoy_rgdp_conditional_on_x <- cv_all_x_rmse_each_h_yoy
  cv_rmse_yoy_rgdp <- cv_rmse_each_h_rgdp_yoy
  
  cv_rmse_logdiff_rgdp_conditional_on_x <- cv_all_x_rmse_each_h_logdiff
  cv_rmse_logdiff_rgdp <- cv_rmse_each_h_rgdp_logdiff
  
  cv_rmse_percent_rgdp_conditional_on_x <- cv_all_x_rmse_each_h_percent
  cv_rmse_percent_rgdp <- cv_rmse_each_h_rgdp_percent
  
  names(cv_rmse_level_rgdp_conditional_on_x)[1:test_length] <- paste0("level_rmse_", 1:test_length)
  names(cv_rmse_level_rgdp)[1:test_length] <- paste0("level_rmse_", 1:test_length)
  
  names(cv_rmse_yoy_rgdp_conditional_on_x)[1:test_length] <- paste0("yoy_rmse_", 1:test_length)
  names(cv_rmse_yoy_rgdp)[1:test_length] <- paste0("yoy_rmse_", 1:test_length)
  
  names(cv_rmse_logdiff_rgdp_conditional_on_x)[1:test_length] <- paste0("logdiff_rmse_", 1:test_length)
  names(cv_rmse_logdiff_rgdp)[1:test_length] <- paste0("logdiff_rmse_", 1:test_length)
  
  names(cv_rmse_percent_rgdp_conditional_on_x)[1:test_length] <- paste0("yoy_percent_", 1:test_length)
  names(cv_rmse_percent_rgdp)[1:test_length] <- paste0("yoy_percent_", 1:test_length)
  
  compare_rmse <- rbind(cv_rmse_level_rgdp, 
                        cv_rmse_level_rgdp_conditional_on_x) 
  
  compare_rmse_yoy <- rbind(cv_rmse_yoy_rgdp, 
                            cv_rmse_yoy_rgdp_conditional_on_x)
  
  compare_rmse_logdiff <- rbind(cv_rmse_logdiff_rgdp, 
                            cv_rmse_logdiff_rgdp_conditional_on_x)
  
  compare_rmse_percent <- rbind(cv_rmse_percent_rgdp, 
                            cv_rmse_percent_rgdp_conditional_on_x)
  
  return(list(
    tbl_raw_fcs = tbl_raw_fcs,
    weigthed_fcs = weigthed_fcs,
    fcs_using_yoy_weights = weigthed_fcs_cv_yoy_errors,
    fcs_using_logdiff_weights = weigthed_fcs_cv_logdiff_errors,
    fcs_using_percent_weights = weigthed_fcs_cv_percent_errors,
    rgdp_data_and_uncond_fc = rgdp_data_and_uncond_fc,
    yoy_rgdp_data_and_uncond_fc = yoy_rgdp_data_and_uncond_fc,
    rgdp_uncond_yoy_fc_mean = rgdp_uncond_yoy_fc_mean,
    final_rgdp_and_w_fc = final_rgdp_and_w_fc,
    final_rgdp_and_yoyw_fc = final_rgdp_and_w_fc_yoy_errors,
    final_rgdp_and_logdiffw_fc = final_rgdp_and_w_fc_logdiff_errors,
    final_rgdp_and_percentw_fc = final_rgdp_and_w_fc_percent_errors,
    level_fc_using_accu_level_weights = final_rgdp_and_w_fc,
    level_fc_using_accu_yoy_weights = final_rgdp_and_w_fc_yoy_errors,
    yoy_fc_using_accu_level_weights = yoy_fc_using_weights,
    yoy_fc_using_accu_yoy_weights = yoy_fc_using_weights_yoy_errors,
    yoy_fc_using_accu_logdiff_weights = yoy_fc_using_weights_logdiff_errors,
    yoy_fc_using_accu_percent_weights = yoy_fc_using_weights_percent_errors,
    cv_rmse_level_rgdp_conditional_on_x = cv_rmse_level_rgdp_conditional_on_x,
    cv_rmse_level_rgdp = cv_rmse_level_rgdp,
    cv_rmse_yoy_rgdp_conditional_on_x = cv_rmse_yoy_rgdp_conditional_on_x,
    cv_rmse_yoy_rgdp = cv_rmse_yoy_rgdp,
    cv_rmse_logdiff_rgdp_conditional_on_x = cv_rmse_logdiff_rgdp_conditional_on_x,
    cv_rmse_logdiff_rgdp = cv_rmse_logdiff_rgdp,
    cv_rmse_percent_rgdp_conditional_on_x = cv_rmse_percent_rgdp_conditional_on_x,
    cv_rmse_percent_rgdp = cv_rmse_percent_rgdp,
    compare_rmse = compare_rmse,
    compare_rmse_yoy = compare_rmse_yoy,
    compare_rmse_logdiff = compare_rmse_logdiff,
    compare_rmse_percent = compare_rmse_percent,
    mat_of_raw_fcs = mat_of_raw_fcs,
    tsbl_raw_fcs = tsbl_raw_fcs
  ))
  
}

bsarimax_as_function <- function(data_path, train_span = 16, h_max = 6,
                                 number_of_cv = 8, 
                                 final_forecast_horizon = c(2019, 12),
                                 outer_cv_round = 0, s4xreg = FALSE) {
  
  
  m_analysis_path <- paste0("data/", country_name,"_m_analysis_rgdp.xlsx")
  gdp_and_dates <- get_rgdp_and_dates(data_path)
  
  monthly_data <- get_monthly_variables(data_path = data_path)
  monthly_ts <- make_monthly_ts(monthly_data)
  monthly_ts  <- log(monthly_ts)
  monthly_names <- colnames(monthly_ts)
  
  external_data_path <- "./data/external/external.xlsx"
  external_monthly_data <- get_monthly_variables(data_path = external_data_path)
  external_monthly_ts <- make_monthly_ts(external_monthly_data)
  external_monthly_ts  <- log(external_monthly_ts)
  external_monthly_names <- colnames(external_monthly_ts)
  demetra_output_external <- get_demetra_params(external_data_path)
  
  
  
  rgdp_ts <- ts(data = gdp_and_dates[["gdp_data"]], 
                start = gdp_and_dates[["gdp_start"]], frequency = 4)
  rgdp_ts <- log(rgdp_ts)
  
  # if(outer_cv_round > 0) {
  #   total_obs <- length(rgdp_ts)
  #   print(total_obs)
  #   
  #   print("rgdp_ts")
  #   print(rgdp_ts)
  #   
  #   outer_obs <- total_obs - outer_cv_round  + 1
  #   print(outer_obs)
  #   
  #   cv_rgdp_ts <- ts(data = rgdp_ts[1:outer_obs], 
  #                    start = gdp_and_dates[["gdp_start"]], frequency = 4)
  #   
  #   print("cv_rgdp_ts")
  #   print(cv_rgdp_ts)
  #   
  #   traininig_set_rgdp <- subset(cv_rgdp_ts, 
  #                                start = outer_obs - h_max - train_span + 1,
  #                                end = outer_obs - h_max)
  #   
  #   print("traininig_set_rgdp")
  #   print(traininig_set_rgdp)
  #   
  #   test_set_rgdp <- subset(cv_rgdp_ts, 
  #                           start = outer_obs - h_max + 1,
  #                           end = outer_obs)
  #   
  #   print("test_set_rgdp")
  #   print(test_set_rgdp)
  #   
  #   rgdp_ts <- traininig_set_rgdp
  #   
  # }
  
  demetra_output <- get_demetra_params(data_path)
  
  tic()
  fit_arima_rgdp_list_dem <- fit_arimas(
    y_ts = rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
    this_arima_names = "rgdp")
  toc()
  
  rgdp_uncond_fc <- forecast(fit_arima_rgdp_list_dem[["rgdp"]], h = h_max)
  rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean
  
  
  tic()
  fit_arima_monthly_list_dem <- fit_arimas(
    y_ts = monthly_ts, order_list = demetra_output[["monthly_order_list"]],
    this_arima_names = monthly_names)
  toc()
  
  fit_arima_external_monthly_list_dem <- fit_arimas(
    y_ts = external_monthly_ts, order_list = demetra_output_external[["monthly_order_list"]],
    this_arima_names = external_monthly_names)
  
  
  gdp_order <- get_order_from_arima(fit_arima_rgdp_list_dem)[[1]]
  
  monthly_order <- get_order_from_arima(fit_arima_monthly_list_dem, 
                                        suffix = "dm",
                                        this_arima_names = monthly_names)
  
  mdata_ext <- extend_and_qtr(data_mts = monthly_ts, 
                              final_horizon_date = final_forecast_horizon , 
                              vec_of_names = monthly_names, 
                              fitted_arima_list = fit_arima_monthly_list_dem,
                              start_date_gdp = gdp_and_dates[["gdp_start"]])
  
  external_mdata_ext <- extend_and_qtr(data_mts = external_monthly_ts, 
                                       final_horizon_date = final_forecast_horizon , 
                                       vec_of_names = external_monthly_names, 
                                       fitted_arima_list = fit_arima_external_monthly_list_dem,
                                       start_date_gdp = gdp_and_dates[["gdp_start"]])
  
  # doox <- mdata_ext[["series_xts"]]
  internal_mdata_ext_ts <- mdata_ext[["series_ts"]]
  internal_yoy_mdata_ext_ts <- diff(internal_mdata_ext_ts, lag = 4)
  internal_monthly_names <- monthly_names
  
  external_mdata_ext_ts <- external_mdata_ext[["series_ts"]]
  external_yoy_mdata_ext_ts <- diff(external_mdata_ext_ts, lag = 4)
  
  
  # # doox <- mdata_ext[["series_xts"]]
  # mdata_ext_ts <- mdata_ext[["series_ts"]]
  # yoy_mdata_ext_ts <- diff(mdata_ext_ts, lag = 4)
  # 
  # my_emae <- mdata_ext_ts[, "emae"]
  
  rgdp_order <-  gdp_order[c("p", "d", "q")]
  rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
  
  
  mdata_ext_ts <- ts.union(internal_mdata_ext_ts, external_mdata_ext_ts)
  monthly_names <- c(internal_monthly_names, external_monthly_names)
  colnames(mdata_ext_ts) <- monthly_names
  
  
  
  # my_emaeip <- mdata_ext_ts[, c("emae", "ip")]
  
  
  tic()
  # using contemporary xregs (k = 0)
  cv0_e_i <- cv_arimax(y_ts = rgdp_ts, xreg_ts = internal_mdata_ext_ts,  h_max =  h_max, n_cv = number_of_cv,
                       training_length = train_span,  y_order = rgdp_order, 
                       y_seasonal = rgdp_seasonal, vec_of_names = internal_monthly_names,
                       method = "ML", s4xreg = FALSE)
  
  cv0_e_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = external_mdata_ext_ts, 
                       h_max =  h_max, n_cv = number_of_cv,
                       training_length = train_span,  y_order = rgdp_order, 
                       y_seasonal = rgdp_seasonal, 
                       vec_of_names = external_monthly_names,
                       method = "ML", s4xreg = FALSE)
  
  cv0_e <- list(cv_errors_all_pairs_yx = c(cv0_e_i$cv_errors_all_pairs_yx,
                                           cv0_e_e$cv_errors_all_pairs_yx),
                cv_yoy_errors_all_pairs_yx = c(cv0_e_i$cv_yoy_errors_all_pairs_yx,
                                               cv0_e_e$cv_yoy_errors_all_pairs_yx)
  )
  
  
  
  # using one-lag xregs (k = 1)
  cv1_e_i <- cv_arimax(y_ts = rgdp_ts, xreg_ts = internal_mdata_ext_ts,  h_max = h_max,
                       n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                       y_seasonal = rgdp_seasonal, vec_of_names = internal_monthly_names,
                       method = "ML", s4xreg = FALSE, xreg_lags = 0:1)
  
  cv1_e_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = external_mdata_ext_ts,  h_max = h_max,
                       n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                       y_seasonal = rgdp_seasonal, vec_of_names = external_monthly_names,
                       method = "ML", s4xreg = FALSE, xreg_lags = 0:1)
  
  cv1_e <- list(cv_errors_all_pairs_yx = c(cv1_e_i$cv_errors_all_pairs_yx,
                                           cv1_e_e$cv_errors_all_pairs_yx),
                cv_yoy_errors_all_pairs_yx = c(cv1_e_i$cv_yoy_errors_all_pairs_yx,
                                               cv1_e_e$cv_yoy_errors_all_pairs_yx)
  )
  
  # using two-lags xregs (k = 2)
  cv2_e_i <- cv_arimax(y_ts = rgdp_ts, xreg_ts = internal_mdata_ext_ts,  h_max = h_max,
                       n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                       y_seasonal = rgdp_seasonal, vec_of_names = internal_monthly_names,
                       method = "ML", s4xreg = FALSE, xreg_lags = 0:2)
  
  cv2_e_e <- cv_arimax(y_ts = rgdp_ts, xreg_ts = external_mdata_ext_ts,  h_max = h_max,
                       n_cv = number_of_cv, training_length = train_span,  y_order = rgdp_order, 
                       y_seasonal = rgdp_seasonal, vec_of_names = external_monthly_names,
                       method = "ML", s4xreg = FALSE, xreg_lags = 0:2)
  
  cv2_e <- list(cv_errors_all_pairs_yx = c(cv2_e_i$cv_errors_all_pairs_yx,
                                           cv2_e_e$cv_errors_all_pairs_yx),
                cv_yoy_errors_all_pairs_yx = c(cv2_e_i$cv_yoy_errors_all_pairs_yx,
                                               cv2_e_e$cv_yoy_errors_all_pairs_yx)
  )
  
  cv_rgdp_e <- cv_arima(y_ts = rgdp_ts, h_max = h_max, n_cv = number_of_cv,
                        training_length = train_span,  y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal,
                        method = "ML")
  
  cv0_e_yoy <- cv0_e[["cv_yoy_errors_all_pairs_yx"]]
  cv1_e_yoy <- cv1_e[["cv_yoy_errors_all_pairs_yx"]]
  cv2_e_yoy <- cv2_e[["cv_yoy_errors_all_pairs_yx"]]
  
  cv0_e <- cv0_e[["cv_errors_all_pairs_yx"]]
  cv1_e <- cv1_e[["cv_errors_all_pairs_yx"]]
  cv2_e <- cv2_e[["cv_errors_all_pairs_yx"]]
  
  cv_rgdp_e_yoy <- cv_rgdp_e[["cv_yoy_errors"]]
  cv_rgdp_e <- cv_rgdp_e[["cv_errors"]]
  
  toc()
  
  # example with weights_vec set to default
  cv0_rmse_list <- map(cv0_e, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  cv1_rmse_list <- map(cv1_e, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  cv2_rmse_list <- map(cv2_e, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  
  cv0_rmse_list_yoy <- map(cv0_e_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  cv1_rmse_list_yoy <- map(cv1_e_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  cv2_rmse_list_yoy <- map(cv2_e_yoy, compute_rmse, h_max = h_max, n_cv = number_of_cv)
  
  cv_rdgp_rmse <- compute_rmse(cv_rgdp_e, h_max = h_max, n_cv = number_of_cv)
  cv_rdgp_rmse_yoy <- compute_rmse(cv_rgdp_e_yoy, h_max = h_max, n_cv = number_of_cv)
  
  
  cv0_rmse_each_h <- map(cv0_rmse_list, "same_h_rmse") %>% reduce(., rbind) %>% 
    mutate(variable = monthly_names, lag = 0)
  cv1_rmse_each_h <- map(cv1_rmse_list, "same_h_rmse") %>% reduce(., rbind) %>% 
    mutate(variable = monthly_names, lag = 1)
  cv2_rmse_each_h <- map(cv2_rmse_list, "same_h_rmse") %>% reduce(., rbind) %>% 
    mutate(variable = monthly_names, lag = 2)
  cv_rmse_each_h_rgdp <- cv_rdgp_rmse[["same_h_rmse"]] %>% 
    mutate(variable = "rgdp", lag = 0)
  
  cv_all_x_rmse_each_h <- rbind(cv0_rmse_each_h,
                                cv1_rmse_each_h, cv2_rmse_each_h)
  
  cv0_rmse_each_h_yoy <- map(cv0_rmse_list_yoy, "same_h_rmse") %>% reduce(., rbind) %>% 
    mutate(variable = monthly_names, lag = 0)
  cv1_rmse_each_h_yoy <- map(cv1_rmse_list_yoy, "same_h_rmse") %>% reduce(., rbind) %>% 
    mutate(variable = monthly_names, lag = 1)
  cv2_rmse_each_h_yoy <- map(cv2_rmse_list_yoy, "same_h_rmse") %>% reduce(., rbind) %>% 
    mutate(variable = monthly_names, lag = 2)
  cv_rmse_each_h_rgdp_yoy <- cv_rdgp_rmse_yoy[["same_h_rmse"]] %>% 
    mutate(variable = "rgdp", lag = 0)
  
  
  cv_all_x_rmse_each_h_yoy <- rbind(cv0_rmse_each_h_yoy,
                                    cv1_rmse_each_h_yoy, cv2_rmse_each_h_yoy)
  
  
  
  all_arimax_0 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
                            y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                            s4xreg = s4xreg)
  
  all_arimax_1 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
                            y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                            s4xreg = s4xreg, xreg_lags = 0:1)
  
  all_arimax_2 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
                            y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                            s4xreg = s4xreg, xreg_lags = 0:2)
  
  all_fcs_0 <- forecast_xreg(all_arimax_0, mdata_ext_ts, h = h_max, 
                             vec_of_names = monthly_names)
  all_fcs_1 <- forecast_xreg(all_arimax_1, mdata_ext_ts, h = h_max, 
                             vec_of_names = monthly_names, xreg_lags = 0:1)
  all_fcs_2 <- forecast_xreg(all_arimax_2, mdata_ext_ts, h = h_max,
                             vec_of_names = monthly_names, xreg_lags = 0:2)  
  
  all_fcs <- tibble(fc_0 = all_fcs_0, fc_1 = all_fcs_1, fc_2 = all_fcs_2, 
                    id_fc = monthly_names) %>%
    gather(key = "type_fc", value = "fc", -id_fc) %>% 
    mutate(lag = as.integer(str_remove(type_fc, "fc_")),
           raw_rgdp_fc = map(fc, "mean")) %>% 
    mutate(armapar = map(fc, c("model", "arma")),
           arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
           arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
    ) %>% 
    mutate(data_and_fc = map(raw_rgdp_fc, ~ ts(data = c(rgdp_ts, .), frequency = 4,
                                               start = stats::start(rgdp_ts))),
           yoy_data_and_fc = map(data_and_fc, ~ make_yoy_ts(exp(.))),
           yoy_raw_rgdp_fc = map2(yoy_data_and_fc, raw_rgdp_fc,
                                  ~ window(.x, start = stats::start(.y)))
    )
  
  all_arimax <- tibble(arimax_0 = all_arimax_0, arimax_1 = all_arimax_1, 
                       arimax_2 = all_arimax_2,  id_fc = monthly_names) %>%
    gather(key = "type_arimax", value = "arimax", -id_fc) %>% 
    mutate(lag = as.integer(str_remove(type_arimax, "arimax_")), 
           armapar = map(arimax, c("arma")),
           arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
           arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
    )
  
  
  var_lag_order_season <- all_fcs %>% 
    dplyr::select(id_fc, lag, arima_order, arima_seasonal) %>% 
    rename(variable = id_fc, lag = lag)
  
  rgdp_var_lag_order_season <- tibble(
    variable = "rgdp", lag = 0, 
    arima_order = list(rgdp_order), arima_seasonal = list(rgdp_seasonal)) 
  
  var_lag_order_season <- rbind(rgdp_var_lag_order_season, var_lag_order_season)
  
  
  mat_of_raw_fcs <- reduce(all_fcs$raw_rgdp_fc, rbind)
  
  
  weigthed_fcs <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                   mat_cv_rmses_from_x = cv_all_x_rmse_each_h,
                                   vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp)
  
  weigthed_fcs[ is.nan(weigthed_fcs)] <- rgdp_uncond_fc_mean[ is.nan(weigthed_fcs)]
  
  
  fcs_using_yoy_weights <- get_weighted_fcs(raw_fcs = mat_of_raw_fcs,
                                            mat_cv_rmses_from_x = cv_all_x_rmse_each_h_yoy,
                                            vec_cv_rmse_from_rgdp = cv_rmse_each_h_rgdp_yoy)
  
  fcs_using_yoy_weights[ is.nan(fcs_using_yoy_weights)] <- rgdp_uncond_fc_mean[ is.nan(fcs_using_yoy_weights)]
  
  weigthed_fcs <- ts(weigthed_fcs, 
                     start = stats::start(rgdp_uncond_fc_mean), 
                     frequency = 4)
  
  fcs_using_yoy_weights <- ts(fcs_using_yoy_weights, 
                              start = stats::start(rgdp_uncond_fc_mean), 
                              frequency = 4)
  
  final_rgdp_and_w_fc <- ts(c(rgdp_ts, weigthed_fcs), frequency = 4,
                            start = stats::start(rgdp_ts))
  
  final_rgdp_and_yoyw_fc <- ts(c(rgdp_ts, fcs_using_yoy_weights), frequency = 4,
                               start = stats::start(rgdp_ts))
  
  rgdp_data_and_uncond_fc <- ts(data = c(rgdp_ts, rgdp_uncond_fc_mean), 
                                frequency = 4, start = stats::start(rgdp_ts))
  
  yoy_rgdp_data_and_uncond_fc <- make_yoy_ts(exp(rgdp_data_and_uncond_fc))
  
  rgdp_uncond_yoy_fc_mean <- window(yoy_rgdp_data_and_uncond_fc,
                                    start = stats::start(rgdp_uncond_fc_mean))
  
  
  expo_final_rgdp_and_w_fc <- exp(final_rgdp_and_w_fc)
  expo_final_rgdp_and_yoyw_fc <- exp(final_rgdp_and_yoyw_fc)
  
  yoy_growth_expo_final_rgdp_and_w_fc <- diff(expo_final_rgdp_and_w_fc, lag = 4)/lag.xts(expo_final_rgdp_and_w_fc, k = 4)
  yoy_growth_expo_final_rgdp_and_yoyw_fc <- diff(expo_final_rgdp_and_yoyw_fc, lag = 4)/lag.xts(expo_final_rgdp_and_yoyw_fc, k = 4)
  
  
  level_fc_using_accu_level_weights <- expo_final_rgdp_and_w_fc
  
  level_fc_using_accu_yoy_weights <- expo_final_rgdp_and_yoyw_fc
  
  yoy_fc_using_accu_level_weights <- yoy_growth_expo_final_rgdp_and_w_fc
  yoy_fc_using_accu_yoy_weights <- yoy_growth_expo_final_rgdp_and_yoyw_fc
  cv_rmse_yoy_rgdp_conditional_on_x <- cv_all_x_rmse_each_h_yoy
  cv_rmse_yoy_rgdp <- cv_rmse_each_h_rgdp_yoy
  cv_rmse_level_rgdp_conditional_on_x <- cv_all_x_rmse_each_h
  cv_rmse_level_rgdp <- cv_rmse_each_h_rgdp

  
  names(cv_rmse_level_rgdp_conditional_on_x)[1:8] <- paste0("level_rmse_", 1:8)
  names(cv_rmse_yoy_rgdp_conditional_on_x)[1:8] <- paste0("yoy_rmse_", 1:8)
  names(cv_rmse_yoy_rgdp)[1:8] <- paste0("yoy_rmse_", 1:8)
  names(cv_rmse_level_rgdp)[1:8] <- paste0("level_rmse_", 1:8)
  
  
  m_arg <- read_excel(m_analysis_path)
  
  m_all_rmse <- m_arg[, c("cond_exo", "rmse1", "rmse2", "rmse3", "rmse4", "rmse5", "rmse6", "rmse7", "rmse8")]
  m_to_compare_rmse <- m_all_rmse %>% 
    mutate_if(is.double, function(x) 0.01 * x) %>% 
    mutate(pre_variable = str_remove(cond_exo, "S4.l"),
           variable = str_extract(pre_variable, "([^\\s]+)"),
           lag = ifelse(str_detect(pre_variable, "LS"), 
                        ifelse(str_detect(pre_variable, "L2S"), 2, 1) , 0)
    ) %>% 
    select(-pre_variable) %>% 
    mutate(variable = ifelse(variable == "_NONE", "rgdp", variable))
  
  compare_rmse <- rbind(cv_rmse_level_rgdp, cv_rmse_level_rgdp_conditional_on_x) %>%
    left_join(m_to_compare_rmse, by = c("variable", "lag")) %>% 
    dplyr::select( -c(cond_exo))
  
  compare_rmse_yoy <- rbind(cv_rmse_yoy_rgdp, 
                            cv_rmse_yoy_rgdp_conditional_on_x)
  
  
  
  return(list(cv_all_x_rmse_each_h = cv_all_x_rmse_each_h,
              cv_all_x_rmse_each_h_yoy = cv_all_x_rmse_each_h_yoy,
              cv_rmse_each_h_rgdp = cv_rmse_each_h_rgdp,
              cv_rmse_each_h_rgdp_yoy = cv_rmse_each_h_rgdp_yoy,
              expo_final_rgdp_and_w_fc = expo_final_rgdp_and_w_fc,
              expo_final_rgdp_and_yoyw_fc = expo_final_rgdp_and_yoyw_fc,
              yoy_growth_expo_final_rgdp_and_w_fc = yoy_growth_expo_final_rgdp_and_w_fc,
              yoy_growth_expo_final_rgdp_and_yoyw_fc = yoy_growth_expo_final_rgdp_and_yoyw_fc,
              var_lag_order_season = var_lag_order_season,
              mdata_ext_ts = mdata_ext_ts,
              rgdp_ts_in_arima = rgdp_ts,
              all_raw_fcs = all_fcs,
              all_arimax = all_arimax,
              compare_rmse_yoy = compare_rmse_yoy,
              compare_rmse = compare_rmse,
              var_lag_order_season = var_lag_order_season,
              uncond_fc = rgdp_uncond_fc_mean,
              uncond_yoy_fc = rgdp_uncond_yoy_fc_mean))
  
}

calc_ee <- function(vec_of_dQ, levQ, cut_1 = 3, cut_2 = 7) {
  ee2_start <- cut_1 + 1
  vec_Q_dQ_ee1 <- c(levQ, vec_of_dQ[1:cut_1])
  Q_ee_1 <- mean(cumsum(vec_Q_dQ_ee1))
  vec_Q_dQ_ee2 <- c(Q_ee_1, vec_of_dQ[ee2_start:cut_2])
  Q_ee_2 <- mean(cumsum(vec_Q_dQ_ee2))
  
  return(
    list(ee1 = Q_ee_1, ee2 = Q_ee_2)
  )
}

calc_bp <- function(vec_of_dQ, levQ, cut_1 = 1, cut_2 = 5) {
  bp2_start <- cut_1 + 1
  vec_Q_dQ_bp1 <- c(levQ, vec_of_dQ[1:cut_1])
  Q_bp_1 <- mean(cumsum(vec_Q_dQ_bp1))
  vec_Q_dQ_bp2 <- c(Q_bp_1, vec_of_dQ[bp2_start:cut_2])
  Q_bp_2 <- mean(cumsum(vec_Q_dQ_bp2))
  
  return(
    list(bp1 = Q_bp_1, bp2 = Q_bp_2)
  )
}

chop_start_xts <- function(df_xts, start_date){
  subset_string <- paste0(as.Date(start_date), "/")
  new_xts <- df_xts[subset_string]
  return(new_xts)
}

chop_start_end_xts <- function(df_xts, start_date, end_date) {
  
  start_str <- start_date
  end_str <- end_date
  
  subset_string <- paste0(start_str, "/", end_str)
  
  new_xts <- df_xts[subset_string]
  
  return(new_xts)
}

comb_ndiffs <- function(this_series, return_4_seas = FALSE, 
                        do_other_seas = FALSE, seas_test = "seas") {
  
  tests_names <- c("kpss", "pp", "adf")
  tests_season_names <- c("seas", "ocsb", "hegy", "ch")
  tests_alpha <- c(0.01, 0.05, 0.1)
  tests_type <- c("level", "trend")
  
  
  tests_of_stationarity <- as_tibble(
    expand.grid(tests_names, tests_type, tests_alpha,
                stringsAsFactors = FALSE)) %>% 
    rename(test = Var1, deter_part = Var2, alpha = Var3) %>% 
    mutate(seas_result = map_dbl(alpha,
                                 ~ nsdiffs(x = this_series, alpha = ., 
                                           test = seas_test)),
           seas_test = seas_test,
           sta_result = pmap_dbl(list(test, alpha, deter_part),
                                 ~ ndiffs(x = this_series, alpha = ..2,
                                          test = ..1, type = ..3)),
           sta_result_after_seas = pmap_dbl(
             list(test, alpha, deter_part, seas_result),
             ~ ndiffs(x = my_diff(this_series, lag = 4, differences = ..4), 
                      alpha = ..2, test = ..1, type = ..3)),
           recommendation = pmap_chr(
             list(seas_result, sta_result, sta_result_after_seas),
             ~ make_recommendation(seas = ..1, sta = ..2, sta_after_seas = ..3)
           )
    ) %>% 
    dplyr::select(test, deter_part, alpha, sta_result, seas_test,
                  seas_result, sta_result_after_seas, recommendation)
  
  if (do_other_seas) {
    tests_of_seasonality <- as_tibble(
      expand.grid(tests_season_names, tests_alpha, stringsAsFactors = FALSE)) %>% 
      rename(test = Var1, alpha = Var2) %>% 
      mutate(seas_result = map2_dbl(test, alpha,
                                    suppressWarnings(
                                      ~ nsdiffs(x = this_series, alpha = .y,
                                                test = .x)))
      )
  }
  
  
  if (return_4_seas) {
    return(list(stationarity = tests_of_stationarity, 
                seas = tests_of_seasonality))
  } else {
    return(tests_of_stationarity)
  }
  
}


compare_two_orders <- function(ord1, ord2, vec_of_names) {
  
  ord1_df <- t(as.data.frame(ord1))
  ord2_df <- t(as.data.frame(ord2))
  
  order_both_df <- cbind(ord1_df, ord2_df)
  
  order_both_diff_df <- as.data.frame(order_both_df) %>%
    mutate(p_diff = p_r - p_dm, q_diff = q_r - q_dm, d_diff = d_r - d_dm,
           P_diff = P_r - P_dm, Q_diff = Q_r - Q_dm, D_diff = D_r - D_dm) %>%
    select(c(p_diff, q_diff, d_diff, P_diff, Q_diff, D_diff)) %>% 
    mutate(id = vec_of_names)
  
  return(
    list(diffs = order_both_diff_df, both = order_both_df)
  )
}



cutback_ts <- function(single_ts, nrows_to_cut) {
  
  if (nrows_to_cut > 0) {
    
    if (  is.na(last(single_ts))  ) {
      ddc_ts <- diff(diff(cumsum(is.na(single_ts))))
      ind_last_obs <- which(ddc_ts == 1) + 1
      new_ts <- single_ts
      new_ts[(ind_last_obs - nrows_to_cut + 1):ind_last_obs] <- NA
    } else {
      new_ts <- single_ts
      new_ts[(length(single_ts) - nrows_to_cut + 1):length(single_ts)] <- NA
    }
    return(new_ts)
  } else{
    return(single_ts)
  }
  
}


cv_obs_fc_back_from_diff <- function(yoy_ts, diff_ts, training_length,
                                     n_cv, h_max, cv_fcs_one_model,
                                     level_ts){
  
  cv_marks <-  make_test_dates_list(ts_data = diff_ts, n = n_cv,
                                    h_max = h_max, 
                                    training_length = training_length)
  
  cv_yq <- cv_marks[["list_of_year_quarter"]]
  training_end_yq <- map(cv_yq, "tra_e")
  test_start_yq <- map(cv_yq, "tes_s")
  test_end_yq <- map(cv_yq, "tes_e")
  
  cv_last_tra_obs_yoy <- list_along(training_end_yq)
  
  cv_test_set_obs_yoy <- list_along(training_end_yq)
  cv_fcs_yoy <- list_along(training_end_yq)
  cv_errors_yoy <- list_along(training_end_yq)
  
  cv_test_set_obs_level <- list_along(training_end_yq)
  cv_fcs_level <- list_along(training_end_yq)
  cv_errors_level <- list_along(training_end_yq)
  
  
  for (i in seq_along(test_end_yq)) {
    this_training_end_yq <- training_end_yq[[i]]
    this_test_end_yq <- test_end_yq[[i]]
    
    this_test_start_yq <- test_start_yq[[i]]
    
    this_training_end_y <- this_training_end_yq[1]
    this_training_end_q <- this_training_end_yq[2]
    
    decimal_q <- (this_training_end_q/4) - 0.25
    decimal_yq <- this_training_end_y + decimal_q
    new_decimal_yq <- decimal_yq - 0.75
    
    this_last_year_training_y <- floor(new_decimal_yq)
    this_last_year_training_q <- 4*(new_decimal_yq - floor(new_decimal_yq) + 0.25)
    
    this_year_before_train_end <- c(this_last_year_training_y, 
                                    this_last_year_training_q)
    
    # print("c(this_training_end_y, this_training_end_q)")
    # print(c(this_training_end_y, this_training_end_q))
    # 
    # print("decimal_q")
    # print(decimal_q)
    # 
    # print("decimal_yq")
    # print(decimal_yq)
    # 
    # print("this_last_year_training_y")
    # print(this_last_year_training_y)
    # 
    # print("this_last_year_training_q")
    # print(this_last_year_training_q)
    # 
    # 
    # 
    # print("this_year_before_train_end")
    # print(this_year_before_train_end)
    
    this_diff_fc <- cv_fcs_one_model[[i]]
    
    this_last_yoy_tra <- window(yoy_ts, start = this_training_end_yq,
                                end = this_training_end_yq)
    
    this_last_yoy_tra_rgdp <- this_last_yoy_tra[, "rgdp"]
    
    this_test_yoy <- window(yoy_ts, start = this_test_start_yq,
                            end = this_test_end_yq)
    this_test_yoy_rgdp <- this_test_yoy[, "rgdp"]
    
    
    
    # print("this_training_end_yq")
    # print(this_training_end_yq)
    
    
    this_last_year_of_train_level <- window(level_ts, 
                                            start = this_year_before_train_end,
                                            end = this_training_end_yq)
    
    this_last_year_of_train_level_rgdp <- this_last_year_of_train_level[, "rgdp"]
    
    # print("this_last_year_of_train_level_rgdp")
    # print(this_last_year_of_train_level_rgdp) 
    
    this_diff_fc_rgdp <- this_diff_fc
    
    # this_yoy_fc_rgdp <-  this_last_yoy_tra_rgdp[1] + cumsum(this_diff_fc_rgdp)
    this_yoy_fc_rgdp <-  un_diff(last_undiffed = this_last_yoy_tra_rgdp[1], 
                                 diffed_ts = this_diff_fc_rgdp)
    alt_ts_this_yoy_fc_rgdp <-  un_diff_ts(last_undiffed = this_last_yoy_tra_rgdp[1], 
                                           diffed_ts = this_diff_fc_rgdp)
    
    this_yoy_error <- this_test_yoy_rgdp - this_yoy_fc_rgdp
    alt_ts_this_yoy_error <- this_test_yoy_rgdp - alt_ts_this_yoy_fc_rgdp
    
    this_level_fc_rgdp <- un_yoy(init_lev = this_last_year_of_train_level_rgdp,
                                 vec_yoy = this_yoy_fc_rgdp)  
    
    # alt_ts_this_level_fc_rgdp <- un_yoy_ts(init_lev = this_last_year_of_train_level_rgdp,
    #                              vec_yoy = this_yoy_fc_rgdp)  
    # 
    # print("this_level_fc_rgdp")
    # print(this_level_fc_rgdp)
    # 
    # print("alt_ts_this_level_fc_rgdp")
    # print(alt_ts_this_level_fc_rgdp)
    
    # print(alt_ts_this_level_fc_rgdp)
    # print(alt_ts_this_yoy_fc_rgdp)
    
    this_test_level <- window(level_ts, start = this_test_start_yq,
                              end = this_test_end_yq)
    this_test_level_rgdp <- this_test_level[, "rgdp"]
    
    # print("this_test_level_rgdp")
    # print(this_test_level_rgdp)
    # 
    # print("level_ts[, rgdp]")
    # print(level_ts[, "rgdp"])
    
    
    this_level_error <- this_test_level_rgdp - this_level_fc_rgdp 
    # alt_ts_this_level_error <- this_test_level_rgdp - alt_ts_this_level_fc_rgdp 
    
    cv_last_tra_obs_yoy[[i]] <- this_last_yoy_tra_rgdp
    
    cv_test_set_obs_yoy[[i]] <- this_test_yoy_rgdp
    cv_errors_yoy[[i]] <- this_yoy_error
    cv_fcs_yoy[[i]] <- this_yoy_fc_rgdp
    
    cv_test_set_obs_level[[i]] <- this_test_level_rgdp
    cv_errors_level[[i]] <- this_level_error
    cv_fcs_level[[i]] <- this_level_fc_rgdp
    
  }
  
  return(list(test_obs_yoy = cv_test_set_obs_yoy,
              fcs_yoy = cv_fcs_yoy,
              fcs_errors_yoy = cv_errors_yoy,
              test_obs_level = cv_test_set_obs_level,
              fcs_level = cv_fcs_level,
              fcs_errors_level = cv_errors_level))
  
}


cv_arimax <- function(y_ts, xreg_ts, h_max, n_cv, training_length,
                          y_order, y_seasonal,
                          y_include_drift = TRUE, 
                          vec_of_names = NULL, method = "ML", 
                          s4xreg = FALSE,
                          xreg_lags = NULL,
                          data_is_log_log = FALSE,
                          force.constant = FALSE) {
  

  i = 1
  y_ts <- na.omit(y_ts)
  # xreg_ts <- na.omit(xreg_ts)

  y_start_year_quarter <- stats::start(y_ts)
  y_end_year_quarter <- stats::end(y_ts)

  number_of_xregs <- ncol(as.matrix(xreg_ts))

  cv_errors_all_pairs_yx <- list_along(seq.int(1, number_of_xregs)) 
  cv_yoy_errors_all_pairs_yx <- list_along(seq.int(1, number_of_xregs)) 
  cv_logdiff_errors_all_pairs_yx <- list_along(seq.int(1, number_of_xregs)) 
  cv_percent_errors_all_pairs_yx <- list_along(seq.int(1, number_of_xregs)) 
  
  for (x in 1:number_of_xregs) {
    # print(paste("x =", x))
    
    this_arima_name <- vec_of_names[x]
    # print("this_arima_name")
    # print(this_arima_name)
    
    
    if (is.null(ncol(xreg_ts))) {
      x_series <-  xreg_ts
      dim(x_series) <- c(length(x_series), 1)
      
    } else {
      x_series <-  xreg_ts[ , x]
    }
    
    x_time <- time(x_series)
    y_time <- time(y_ts)
    

    if (min(x_time) > min(y_time)) {
      latest_start <- stats::start(x_series)
    } else {
      latest_start <- stats::start(y_ts)
    }
    
    if (max(x_time) < max(y_time)) {
      earliest_end <- stats::end(x_series)
    } else {
      earliest_end <- stats::end(y_ts)
    }
    


    procrustean_y <- window(y_ts, start = latest_start, end = earliest_end, 
                            frequency = 4)

    procrustean_x <- window(x_series, start = latest_start, end = earliest_end,
                            frequency = 4)
    
    n_x <- length(procrustean_x)
    n_y <- length(procrustean_y)
    
    # print("n_x == n_y")
    # print(n_x == n_y)

    cv_errors_this_x <- list_along(1:n_cv)
    cv_yoy_errors_this_x  <- list_along(1:n_cv)
    cv_logdiff_errors_this_x  <- list_along(1:n_cv)
    cv_percent_errors_this_x  <- list_along(1:n_cv)
    

    for (i in seq_along(1:n_cv)) {

      train_plus_test_plus_im1 <- training_length + h_max + (i - 1)
      start_training_index_y <-  n_y - train_plus_test_plus_im1 + 1
      end_training_index_y <-  start_training_index_y + training_length - 1
      start_test_index_y <- end_training_index_y + 1
      end_test_index_y <- start_test_index_y + h_max - 1
      
      start_training_index_x <-  n_x - train_plus_test_plus_im1 + 1
      end_training_index_x <-  start_training_index_x + training_length - 1
      start_test_index_x <- end_training_index_x + 1
      end_test_index_x <- start_test_index_x + h_max - 1

      # print("start_training_index_y")
      # print(start_training_index_y)
      # 
      # print("start_training_index_x")
      # print(start_training_index_x)
      
      training_y <- subset(y_ts, 
                           start = start_training_index_y,
                           end = end_training_index_y)

      training_x <- window(x_series, start = stats::start(training_y),
                           end = stats::end(training_y) )
      
      test_y <- subset(y_ts, 
                       start = start_test_index_y,
                       end = end_test_index_y)
      
      test_x <- window(x_series,
                       start = stats::start(test_y),
                       end = stats::end(test_y))

      this_arimax_list <- my_arimax(y_ts = training_y, xreg_ts = training_x, 
                               y_order = y_order, y_seasonal = y_seasonal,
                               vec_of_names = this_arima_name, xreg_lags = xreg_lags,
                               method = method, force.constant = force.constant,
                               y_include_mean = y_include_drift
                               )
      
      # print("names(this_arimax_list)") 
      # print(names(this_arimax_list))
      
      this_arimax <- this_arimax_list[[names(this_arimax_list)]]


      # this_fc <- forecast(this_arimax, h = h_max, xreg = test_x)
      # print(test_x)
      this_fc_list <- forecast_xreg(arimax_list = this_arimax_list, 
                               h = length(test_y), 
                               xreg_mts = x_series,
                               xreg_lags = xreg_lags, 
                               vec_of_names = this_arima_name, 
                               force.constant = force.constant
                               )
      
      this_fc <- this_fc_list[[this_arima_name]]
      
      train_rgdp_and_fc <- ts(data = c(training_y, this_fc$mean),
                              frequency = 4,
                              start = stats::start(training_y))
      
      train_and_test_ts <- ts(data = c(training_y, test_y), 
                              frequency = 4,
                              start = start(training_y))
      
      if (data_is_log_log) {
        train_rgdp_and_fc_yoy <- make_yoy_ts(train_rgdp_and_fc, freq = 4, 
                                             is_log = TRUE)
        
        len_tf_yoy <- length(train_rgdp_and_fc_yoy)
        
        fc_rgdp_yoy <- train_rgdp_and_fc_yoy[(len_tf_yoy - h_max + 1):len_tf_yoy]
        
        train_and_test_yoy <- make_yoy_ts(train_and_test_ts, freq = 4, 
                                          is_log = TRUE)
        
        len_tt_yoy <- length(train_and_test_yoy)
        
        test_y_yoy <- train_and_test_yoy[(len_tt_yoy - h_max + 1):len_tt_yoy]
        
        fc_error_of_yoy <- test_y_yoy - fc_rgdp_yoy
        
        fc_error_as_log_diff <- 100*(test_y - this_fc$mean)
        
        fc_error_as_percent <- 100*((exp(test_y) - exp(this_fc$mean))/exp(test_y))
        
        fc_error <- exp(test_y) - exp(this_fc$mean)
        
        
      } else {
        
        train_rgdp_and_fc_yoy <- make_yoy_ts(train_rgdp_and_fc, freq = 4, 
                                             is_log = FALSE)
        
        len_tf_yoy <- length(train_rgdp_and_fc_yoy)
        
        fc_rgdp_yoy <- train_rgdp_and_fc_yoy[(len_tf_yoy - h_max + 1):len_tf_yoy]
        
        train_and_test_yoy <- make_yoy_ts(train_and_test_ts, freq = 4, 
                                          is_log = FALSE)
        
        len_tt_yoy <- length(train_and_test_yoy)
        
        test_y_yoy <- train_and_test_yoy[(len_tt_yoy - h_max + 1):len_tt_yoy]
        
        fc_error_of_yoy <- test_y_yoy - fc_rgdp_yoy
        
        fc_error_as_log_diff <- 100*(log(test_y) - log(this_fc$mean))
        
        fc_error_as_percent <- 100*((test_y - this_fc$mean)/test_y)
        
        fc_error <- test_y - this_fc$mean
        
      }
      
      cv_yoy_errors_this_x[[i]] <- fc_error_of_yoy
      cv_errors_this_x[[i]] <- fc_error
      cv_logdiff_errors_this_x[[i]] <- fc_error_as_log_diff
      cv_percent_errors_this_x[[i]] <- fc_error_as_percent
      
    }
    

    cv_errors_all_pairs_yx[[x]] <- cv_errors_this_x
    cv_yoy_errors_all_pairs_yx[[x]] <- cv_yoy_errors_this_x
    cv_logdiff_errors_all_pairs_yx[[x]] <- cv_logdiff_errors_this_x
    cv_percent_errors_all_pairs_yx[[x]] <- cv_percent_errors_this_x
  }
  
  cv_errors_all_pairs_yx <- map(cv_errors_all_pairs_yx, reduce, rbind)
  names(cv_errors_all_pairs_yx) <- vec_of_names
  
  cv_yoy_errors_all_pairs_yx <- map(cv_yoy_errors_all_pairs_yx, reduce, rbind)
  names(cv_yoy_errors_all_pairs_yx) <- vec_of_names
  
  cv_logdiff_errors_all_pairs_yx <- map(cv_logdiff_errors_all_pairs_yx, reduce, rbind)
  names(cv_logdiff_errors_all_pairs_yx) <- vec_of_names
  
  cv_percent_errors_all_pairs_yx <- map(cv_percent_errors_all_pairs_yx, reduce, rbind)
  names(cv_percent_errors_all_pairs_yx) <- vec_of_names
  
  return(list(cv_errors_all_pairs_yx = cv_errors_all_pairs_yx,
              cv_yoy_errors_all_pairs_yx = cv_yoy_errors_all_pairs_yx,
              cv_logdiff_errors_all_pairs_yx = cv_logdiff_errors_all_pairs_yx,
              cv_percent_errors_all_pairs_yx = cv_percent_errors_all_pairs_yx))
}


cv_arima <- function(y_ts,  h_max, n_cv, training_length,
                     y_order, y_seasonal,
                     y_include_mean = FALSE, 
                     method = "CSS",
                     y_include_drift = TRUE,
                     data_is_log_log = FALSE,
                     force.constant = FALSE) {
  
  i = 1
  y_ts <- na.omit(y_ts)
  
  y_time_min <- min(time(y_ts))
  y_time_max <- max(time(y_ts))
  
  y_end_year <- year(as.yearqtr(y_time_max))
  y_end_quarter <- quarter(as.yearqtr(y_time_max))
  
  y_start_year <- year(as.yearqtr(y_time_min))
  y_start_quarter <- quarter(as.yearqtr(y_time_min))
  
  n <- length(y_ts)
  
  cv_errors <- list_along(1:n_cv)
  cv_yoy_errors <- list_along(1:n_cv)
  cv_logdiff_errors  <- list_along(1:n_cv)
  cv_percent_errors  <- list_along(1:n_cv)
  
  for (i in seq_along(1:n_cv)) {
    
    train_plus_test_plus_im1 <- training_length + h_max + (i - 1)
    start_training_index_y <-  n - train_plus_test_plus_im1 + 1
    end_training_index_y <-  start_training_index_y + training_length - 1
    start_test_index_y <- end_training_index_y + 1
    end_test_index_y <- start_test_index_y + h_max - 1
    
    training_y <- subset(y_ts, 
                         start = start_training_index_y,
                         end = end_training_index_y)
    
    test_y <- subset(y_ts, 
                     start = start_test_index_y,
                     end = end_test_index_y)
    
    this_d <- y_order[2]
    this_D <- y_seasonal[2]
    
    if (force.constant & (this_d + this_D >= 2)) {
      
      sq_trend <- seq(1, length(training_y))^2
      
      this_arima <- Arima(training_y, order = y_order,
                          seasonal = y_seasonal,
                          method = method,
                          include.drift =  y_include_drift,
                          xreg = sq_trend)
      
      sq_trend_for_fc <- seq(length(training_y) + 1, 
                             length(training_y) + length(test_y))^2
      
      this_fc <- forecast(this_arima, h = h_max, xreg = sq_trend_for_fc)
      
      
    } else {
      
      this_arima <- Arima(training_y, order = y_order,
                          seasonal = y_seasonal,
                          method = method,
                          include.drift =  y_include_drift)
      
      this_fc <- forecast(this_arima, h = h_max)
      
    }
    
    
    
    
    train_rgdp_and_fc <- c(training_y, this_fc$mean)
    train_rgdp_and_fc <- ts(data = train_rgdp_and_fc,
                            frequency = 4,
                            start = stats::start(training_y))
    
    
    train_and_test_ts <- ts(data = c(training_y, test_y), 
                            frequency = 4,
                            start = start(training_y))
    
    if (data_is_log_log) {
      train_rgdp_and_fc_yoy <- make_yoy_ts(train_rgdp_and_fc, freq = 4, 
                                           is_log = TRUE)
      
      len_tf_yoy <- length(train_rgdp_and_fc_yoy)
      
      fc_rgdp_yoy <- train_rgdp_and_fc_yoy[(len_tf_yoy - h_max + 1):len_tf_yoy]
      
      train_and_test_yoy <- make_yoy_ts(train_and_test_ts, freq = 4, 
                                        is_log = TRUE)
      
      len_tt_yoy <- length(train_and_test_yoy)
      
      test_y_yoy <- train_and_test_yoy[(len_tt_yoy - h_max + 1):len_tt_yoy]
      
      fc_error_of_yoy <- test_y_yoy - fc_rgdp_yoy
      
      fc_error_as_log_diff <- 100*(test_y - this_fc$mean)
      
      fc_error_as_percent <- 100*((exp(test_y) - exp(this_fc$mean))/exp(test_y))
      
      fc_error <- exp(test_y) - exp(this_fc$mean)
      
      
      
    } else {
      
      train_rgdp_and_fc_yoy <- make_yoy_ts(train_rgdp_and_fc, freq = 4, 
                                           is_log = FALSE)
      
      len_tf_yoy <- length(train_rgdp_and_fc_yoy)
      
      fc_rgdp_yoy <- train_rgdp_and_fc_yoy[(len_tf_yoy - h_max + 1):len_tf_yoy]
      
      train_and_test_yoy <- make_yoy_ts(train_and_test_ts, freq = 4, 
                                        is_log = FALSE)
      
      len_tt_yoy <- length(train_and_test_yoy)
      
      test_y_yoy <- train_and_test_yoy[(len_tt_yoy - h_max + 1):len_tt_yoy]
      
      fc_error_of_yoy <- test_y_yoy - fc_rgdp_yoy
      
      fc_error_as_log_diff <- 100*(log(test_y) - log(this_fc$mean))
      
      fc_error_as_percent <- 100*((test_y - this_fc$mean)/test_y)
      
      fc_error <- test_y - this_fc$mean
      
    }
    
    cv_yoy_errors[[i]] <- fc_error_of_yoy
    cv_errors[[i]] <- fc_error
    cv_logdiff_errors[[i]] <- fc_error_as_log_diff
    cv_percent_errors[[i]] <- fc_error_as_percent
    
  }
  
  cv_errors <- reduce(cv_errors, rbind)
  cv_yoy_errors <- reduce(cv_yoy_errors, rbind)
  cv_logdiff_errors <- reduce(cv_logdiff_errors, rbind)
  cv_percent_errors <- reduce(cv_percent_errors, rbind)
  
  return(list(cv_errors = cv_errors, 
              cv_yoy_errors = cv_yoy_errors,
              cv_logdiff_errors = cv_logdiff_errors,
              cv_percent_errors = cv_percent_errors) )
  
}




compute_rmse <- function(mycv, h_max = 8, n_cv, col_weights_vec = NULL,
                         row_weights_vec = NULL) {
  
  vec_h <- 1:h_max
  vec_cv <- 1:n_cv
  
  horizon_names <- paste("h = ", vec_h)
  cv_names <- paste("cv = ", vec_cv)
  
  same_h_mse <- colMeans(mycv^2, na.rm = T)
  same_h_rmse <- same_h_mse %>% sqrt()
  same_h_rmse <- as.data.frame(t(same_h_rmse))
  colnames(same_h_rmse) <- horizon_names 
  
  across_hs_mse <- rowMeans(mycv^2, na.rm = T)
  across_hs_rmse <- across_hs_mse %>% sqrt()
  across_hs_rmse <- as.data.frame(t(across_hs_rmse))
  colnames(across_hs_rmse) <- cv_names 
  
  if(is.null(col_weights_vec))
    col_weights_vec <- seq(1/length(vec_h), by = 0, length.out = length(vec_h))
  else col_weights_vec <- col_weights_vec
  
  weighted_same_h_rmse_all_hs <- weighted.mean(same_h_rmse, col_weights_vec)
  
  if(is.null(row_weights_vec))
    row_weights_vec <- seq(1/length(vec_cv), by = 0, length.out = length(vec_cv))
  else row_weights_vec <- row_weights_vec
  
  weighted_across_h_rmse_all_cvs <- weighted.mean(across_hs_rmse, row_weights_vec)
  
  return(list(same_h_rmse = same_h_rmse, 
              across_hs_rmse = across_hs_rmse,
              weighted_same_h = weighted_same_h_rmse_all_hs,
              weighted_across_hs = weighted_across_h_rmse_all_cvs))
}


drop_this_vars <- function(df, vars_to_drop) {
  new_df <- df[,!(names(df) %in% vars_to_drop)]
}


drop_this_vars_this_country <- function(df, id_col, country, vars_to_drop) {
  if (id_col == country) {
    new_df <- drop_this_vars(df, vars_to_drop)
  }  else {
    new_df <- df
  }
}


extend_and_qtr <- function(data_mts, final_horizon_date, vec_of_names, 
                           fitted_arima_list, start_date_gdp,
                           force_constant = FALSE, order_list = NULL) {
  
  # print("final_horizon_date")
  # print(final_horizon_date)
  
  fc_list_m <- list() 
  extended_m_ts_list <- list()
  
  final_year <- final_horizon_date[1]
  final_month <- final_horizon_date[2] - 1
  
  final_horizon_decimal <- final_year + final_month/12
  
  # print("final_horizon_decimal")
  # print(final_horizon_decimal)
  
  
  for (i in seq_along(vec_of_names)) {
    
    this_arima <- fitted_arima_list[[i]]
    monthly_series <- data_mts[, vec_of_names[i]]
    
    series_date_max <- monthly_series %>% na.omit() %>% time %>% max
    # diff_decimal <- final_horizon_decimal - series_date_max 
    # diff_in_month <- as.integer(12 * diff_decimal)
    
    series_date_max_ym <- yearmon(series_date_max) 
    final_horizon_decimal_ym <- yearmon(final_horizon_decimal)
    series_to_final_interval <- lubridate::interval(series_date_max_ym,
                                           final_horizon_decimal_ym)
    
    diff_in_month <- series_to_final_interval %/% months(1)
    
    # print("diff_in_month")
    # print(diff_in_month)
    # 
    
    
    # print("fc_mean")
    # print(fc_mean)
    
    
    if (force_constant) {
      this_instruction <- order_list[[i]]
      this_order <- this_instruction[["order"]]
      this_seasonal <- this_instruction[["seasonal"]]
      this_constant <- this_instruction[["mean_logical"]]
      # this_log <- this_instruction[["log_logical"]]
      
      this_d <- this_order[2]
      this_D <- this_seasonal[2]
      
      if (this_d + this_D >= 2) {
        t <- seq(1, length(this_arima$x) + diff_in_month)
        t_sq <- t^2
        
        t_fc <- seq(length(this_arima$x) + 1, length(this_arima$x) + diff_in_month)
        t_fc_sq <- t_fc^2
        
        this_fc <- forecast(this_arima, h = diff_in_month, xreg = t_fc_sq)
        fc_mean <- this_fc$mean  

      }
      
    } else {
      this_fc <- forecast(this_arima, h = diff_in_month)
      fc_mean <- this_fc$mean
    }
    
    # extended_monthly_series <- glue_x_mean(monthly_series, fc_mean)
    extended_monthly_series <- ts(data = c(na.omit(monthly_series), fc_mean),
                                  start = stats::start(na.omit(monthly_series)),
                                  frequency = 12)
    
    # print("extended_monthly_series")
    # print(extended_monthly_series)
    
    
    # print("tail(extended_monthly_series)")
    # print(tail(extended_monthly_series))
    
    fc_list_m[[i]] <- this_fc
    extended_m_ts_list[[i]] <- extended_monthly_series
    
  }
  
  names(fc_list_m) <- vec_of_names
  
  # print(vec_of_names)
  
  names(extended_m_ts_list) <- vec_of_names
  
  if (length(vec_of_names) == 1) {
    ext_monthly_series_mts <- extended_m_ts_list[[1]]
    dim(ext_monthly_series_mts) <- c(length(ext_monthly_series_mts), 1)
  } else {
    ext_monthly_series_mts <- reduce(extended_m_ts_list, ts.union)
  }
  
  colnames(ext_monthly_series_mts) <- vec_of_names
  
  # print("tail(ext_monthly_series_mts)")
  # print(tail(ext_monthly_series_mts))
  
  index_date <- as.Date(time(ext_monthly_series_mts))
  
  rgdp_start_year <- start_date_gdp[1]
  rgdp_start_quarter <- start_date_gdp[2]
  start_gdp_str <- paste0(rgdp_start_year, "-", rgdp_start_quarter, "/")
  
  ext_monthly_series_tbl <- as.tibble(ext_monthly_series_mts) 
  ext_monthly_series_tbl <- cbind(tibble(date = index_date), ext_monthly_series_tbl)
  
  ext_series_xts_monthly <- tk_xts(ext_monthly_series_tbl, date_var = date,
                                   silent = TRUE)
  
  
  
  ext_series_xts_quarterly <- apply.quarterly(ext_series_xts_monthly , 
                                              mean, na.rm = TRUE)
  ext_series_xts_quarterly <- ext_series_xts_quarterly[start_gdp_str]
  
  xts_q_start <- min(date(ext_series_xts_quarterly))
  
  ext_series_ts_quarterly <- tk_ts(ext_series_xts_quarterly, 
                                   start = c(year(xts_q_start), 
                                             quarter(xts_q_start)), 
                                   frequency = 4)
  
  
  # chop off any obs previous to the start of gdp
  
  return(list(quarterly_series_ts = ext_series_ts_quarterly,
              quarterly_series_xts = ext_series_xts_quarterly,
              monthly_series_ts = ext_monthly_series_mts,
              monthly_series_xts = ext_series_xts_monthly))
  
  # return(ext_series_ts_quarterly)
  
}


facet_rmse_all_h <- function(selected_models_tbl) {
  
  rmse_table_single_h <- selected_models_tbl %>% 
    select(variables, lags, model_function, rmse_h, rmse, horizon) %>%
    arrange(rmse_h, model_function, rmse) %>% 
    mutate(idx = 1:n()) %>% 
    group_by(horizon) %>% 
    mutate(id_in_h = 1:n())
  
  max_rmse <- max(rmse_table_single_h$rmse)
  labels <- c(rmse_1 = "RMSE h = 1", rmse_2 = "RMSE h = 2", rmse_3 = "RMSE h = 3" , rmse_4 = "RMSE h = 4",
              rmse_5 = "RMSE h = 5", rmse_6 = "RMSE h = 6", rmse_7 = "RMSE h = 7", rmse_8 = "RMSE h = 8")
  
  p <- ggplot(rmse_table_single_h, aes(x = id_in_h, y = rmse)) + 
    geom_point(aes(color = model_function), size = 2.2, alpha = 0.8) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    facet_wrap(~ rmse_h, labeller=labeller(rmse_h = labels)) + 
    theme_bw()  + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())
  
  
  return(p)
}


facet_rmse_all_h_oldversion <- function(selected_models_tbl) {
  
  rmse_table_single_h <- selected_models_tbl %>% 
    select(variables, lags, model_function, rmse_h, rmse, horizon) %>%
    arrange(rmse_h, model_function, rmse) %>% 
    mutate(idx = 1:n()) %>% 
    group_by(horizon) %>% 
    mutate(id_in_h = 1:n())
  
  
  max_rmse <- max(rmse_table_single_h$rmse)
  
  p <- ggplot(rmse_table_single_h, aes(x = id_in_h, y = rmse)) + 
    geom_point(aes(color = model_function), size = 2.2, alpha = 0.8) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    facet_wrap(~ rmse_h) + 
    theme_bw()  + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())
  
  return(p)
}

fc_yoy_from_fc_level <- function(fc_obj, isloglevel = FALSE, dodifflog = FALSE,
                                 freq = 4){
  x <- fc_obj$x
  xfc <- fc_obj$mean
  xandfc <- ts(data = c(x, xfc), frequency = freq, start = stats::start(x))
  
  
  if (isloglevel) {
    xandfc <- exp(xandfc)
  }
  
  
  if (dodifflog) {
    yoy_all <- diff(log(xandfc), lag = freq)
    
  } else {
    yoy_all <- make_yoy_ts(xandfc, freq = freq)
  }
  
  
  yoy_fc <- window(yoy_all, start = stats::start(xfc))
  
  
  yoy_all_xts <- tk_xts(tk_tbl(yoy_all), silent = TRUE) 
  xandfc_xts <- tk_xts(tk_tbl(xandfc), silent = TRUE) 
  
  
  yearly_ave_yoy_all_xts <- apply.yearly(yoy_all_xts, mean, na.rm = TRUE)
  yearly_total_all_xts <- apply.yearly(xandfc_xts, sum, na.rm = TRUE)
  yearly_total_growth_xts <- make_yoy_xts(yearly_total_all_xts, freq = 1)
  
  return(list(yoy_fc = yoy_fc, yearly_average_yoy = yearly_ave_yoy_all_xts, 
              yearly_total = yearly_total_all_xts, 
              yearly_growth_of_total = yearly_total_growth_xts))
}



fc_log2yoy <- function(model, rgdp_log_ts, fc_ts) {
  
  if (model == "VAR") {
    # we need to transform VARs that are not yoy into yoy
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


fcs_accu <- function(fc_mat, test_data_mat) {
  
  errors_mat <- test_data_mat - fc_mat
  rmse_vec <- sqrt(colMeans(errors_mat^2))
  mean_rmse <- mean(rmse_vec)
  return(mean_rmse)
}

fit_arimas <- function(y_ts, auto = FALSE, order_list = NULL, 
                       my_lambda = NULL,
                       my_biasadj = FALSE, this_arima_names = NULL,
                       include.constant = TRUE, do_stepwise = TRUE, 
                       do_approximation = FALSE,  force_constant = FALSE, 
                       take_log_before = FALSE, freq = 4, parallel = FALSE, 
                       num.cores = 2, print_comments_on_constant = FALSE,
                       method = "ML") {
  
  n_of_series <- ncol(y_ts)
  
  if (is.null(n_of_series)) {
    n_of_series <- 1
  }
  
  
  
  fit_arimas_list <- list_along(seq.int(1, n_of_series))
  
  for (i in seq.int(1, n_of_series)) {
    
    if (is.null(dim(y_ts))) {
      this_y <- y_ts
    } else {
      this_y <- y_ts[, i]
    }
    
    this_y <- na.omit(this_y)
    
    
    if (!auto) {
      this_instruction <- order_list[[i]]
      this_order <- this_instruction[["order"]]
      this_seasonal <- this_instruction[["seasonal"]]
      this_constant <- this_instruction[["mean_logical"]]
      is_log <- this_instruction[["log_logical"]]
      

      this_d <- this_order[2]
      this_D <- this_seasonal[2]

      if ( (this_D + this_d) >= 2) {
        
        if (print_comments_on_constant) {
          print(paste("In arima for", this_arima_names[i], ", D+d =", 
                      this_D + this_d, 
                      ". Stata's default would introduce a constant, R would not."))
        }
  
        if (force_constant) {
          if (print_comments_on_constant) {
            print("Using Stata default (i.e. include a constant even if D+D >=2) by differencing the series before estimation")
            print("This constant is included by way of specifying a vector of t^2 as xreg")
          }
          
          t <- seq(1, length(this_y))
          # print(t)
          t_sq <- t^2
          # print(t_sq)
          
          fit <- Arima(y = this_y, order = this_order, seasonal = this_seasonal,
                       include.constant =  FALSE, lambda = my_lambda, 
                       biasadj = my_biasadj, method = method, xreg = t_sq)
          
          
        } else {
          if (print_comments_on_constant) {
            print("Using R criteria (i.e. do not include a constant) if D+d >= 2")
          }
          
          
          fit <- Arima(y = this_y, order = this_order, seasonal = this_seasonal,
                       lambda = my_lambda, biasadj = my_biasadj,
                       include.constant = include.constant)
        }
      } else {
        
        fit <- Arima(y = this_y, order = this_order, seasonal = this_seasonal,
                     include.constant =  include.constant, lambda = my_lambda, 
                     biasadj = my_biasadj, method = method)
      }
    } else {
      
      
      fit <- auto.arima(y = this_y, lambda = my_lambda, biasadj = my_biasadj, max.d = 1,
                        max.D = 1,
                        stepwise = do_stepwise, 
                        approximation = do_approximation,
                        parallel = parallel, num.cores = num.cores,
                        )
      
    }
    
    fit_arimas_list[[i]] <- fit
    
  }
  
  names(fit_arimas_list) <- this_arima_names
  
  return(fit_arimas_list)
  # this_arimax <- try(Arima(training_y, order = y_order,
  #                          seasonal = y_seasonal,
  #                          xreg = training_x,
  #                          method = method))
  # 
  # class_this_arimax <- class(this_arimax)[1]
  # 
  # 
  # if (class_this_arimax == "try-error") {
  #   this_mssg <- paste0("For xreg variable ", vec_of_names[x], 
  #                       ", ML method failed in Arima. Switched to CSS-ML.")
  #   warning(this_mssg)
  #   new_method <-  "CSS-ML"
  #   this_arimax <- Arima(training_y, order = y_order,
  #                        seasonal = y_seasonal,
  #                        xreg = training_x,
  #                        method = new_method)
  # }
  # 
  
  
}

fit_VAR_Arima <- function(arima_rgdp_ts, model_function, variables, 
                          lags, order, seasonal, extended_x_data_ts) {
  if (model_function == "VAR") {
    
    fit <- vars::VAR(y = VAR_data[, variables], p = lags)
  } 
  
  if (model_function == "Arima") {
    
    fit <- my_arima_one_x(y_ts = arima_rgdp_ts, y_order = order, 
                          y_seasonal = seasonal, xreg_lags = lags, 
                          x_name = variables, xreg_data = extended_x_data_ts)
  } 
  return(fit)
}



follow_rec <- function(data_tbl_ts, table_of_recommendations) {
  
  rec_rows <- nrow(table_of_recommendations)
  
  rec_column <- "kpss_05_level"
  
  new_variables_list <- list_along(1:rec_rows)
  
  for (i in seq_len(rec_rows)) {
    
    this_rec <- table_of_recommendations[[i, rec_column]]
    this_variable <- table_of_recommendations[[i, "variable"]]
    this_variable_ts <- data_tbl_ts[, this_variable] 
    
    
    
    if (this_rec == "level") {
      new_variable_ts <- this_variable_ts
    }
    
    if (this_rec == "yoy") {
      new_variable_ts <- make_yoy_ts(this_variable_ts)
    }
    
    if (this_rec == "diff") {
      new_variable_ts <- base::diff(this_variable_ts)
    }
    
    if (this_rec == "diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts))
    }
    
    if (this_rec == "diff_diff") {
      new_variable_ts <- base::diff(this_variable_ts, differences = 2)
    }
    
    if (this_rec == "diff_diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts),
                                    differences = 2)
    }
    
    new_variables_list[[i]] <- new_variable_ts
    
    
  }
  
  new_data_ts <- reduce(new_variables_list, ts.union)
  colnames(new_data_ts) <- colnames(data_tbl_ts)
  
  return(new_data_ts)
  
}




forecast_VAR_Arima <- function(model_function, variables, lags, fit, 
                               mat_x_ext, h) {
  
  if (model_function == "VAR") {
    fc <- forecast(fit, h = h)
  } 
  
  if (model_function == "Arima") {
    if (variables == "rgdp") {
      fc <- forecast(object = fit, h = h)
    } else {
      fc <- forecast_from_arimax_obj(arimax_obj = fit, x_variable = variables, 
                                     mat_x_ext = mat_x_ext, lags = lags, h = h)
    }
  } 
  return(fc)
}


forecast_from_arimax_obj <- function(arimax_obj, x_variable, mat_x_ext, lags, h) {
  
  # arimax_model <- (arimax_obj$arimax)[[1]] 
  arimax_model <- arimax_obj 
  rgdp_in_arimax <-  arimax_model$x
  end_arimax <- stats::end(rgdp_in_arimax)
  maxtime_arimax <- max(time(rgdp_in_arimax))
  start_forecast <- c(year(as.yearqtr(0.25 + maxtime_arimax)),
                      quarter(as.yearqtr(0.25 + maxtime_arimax)))
  xreg_for_fc <- make_xreg_fc(variable_name = x_variable, mx_ext = mat_x_ext,
                              lags = lags,  start_fc = start_forecast, h = h)
  fc <- forecast(object = arimax_model, h = h, xreg = xreg_for_fc)
  return(fc)
  
}




forecast_one_xreg  <- function(arimax_fit, xreg_data, h, 
                               x_name, xreg_lags = NULL) {
  
  this_arimax <- arimax_fit
  this_series <-  xreg_data[ , x_name]
  n_obs <- length(this_series)
  max_xreg_lag <- max(xreg_lags)
  
  if (max_xreg_lag > 0) {
    
    xlagmat <- c()
    
    for (thislag in 0:max_xreg_lag) {
      xlagmat <- cbind(xlagmat, lag.xts(this_series, k = thislag))
    }
    
    colnames(xlagmat) <- paste0("xlag_", 0:max_xreg_lag)
    this_series <- xlagmat
  }
  
  
  if (is.null(ncol(this_series))) {
    series_for_fc <- this_series[(n_obs - h + 1):n_obs]
  } else {
    series_for_fc <- this_series[(n_obs - h + 1):n_obs, ]
  }
  
  # print(series_for_fc)
  # print(this_arimax)

  this_fc <- forecast(this_arimax, h = h, xreg = series_for_fc)
  
  return(this_fc)
}


forecast_xreg <- function(arimax_list, xreg_mts, h, 
                          vec_of_names = NULL, xreg_lags = NULL,
                          force.constant = FALSE) {
  
  n_vars <- length(arimax_list)

  
  # print("just entered forecast xreg")  
  # print("just entered forecast xreg")  
  # print("just entered forecast xreg")  
  # print("xreg_lags")
  # print(xreg_lags)
  # print("vec_of_names in forecast_xreg")
  # print(vec_of_names)
  # print("h")
  # print(h)
  # print("xreg_mts")
  # print(xreg_mts)
  # print("force.constant")
  # print(force.constant)
  
  
  if(is.null(dim(xreg_mts))) {
    dim(xreg_mts) <- c(length(xreg_mts), 1)
    colnames(xreg_mts) <- "xreg"
  }
  
  # print("colnames(xreg_mts)")
  # print(colnames(xreg_mts))
  

  
  fc_list <- list()
  
  for (i in seq.int(1, n_vars)) {
    
    this_arimax <- arimax_list[[i]]
    this_series <- xreg_mts[,i]
    this_xname <- colnames(xreg_mts)[i]
    this_arma_o <- this_arimax$arma
    this_d <- this_arma_o[6]
    this_D <- this_arma_o[7]
    
    fc_start_asyq <-  as.yearqtr(max(time(this_arimax$x)) + 0.25)
    fc_start_year <- year(fc_start_asyq)
    fc_start_quarter <- quarter(fc_start_asyq)
    fc_start <- c(fc_start_year, fc_start_quarter)
    
    fc_end_asyq <- as.yearqtr(max(time(this_arimax$x)) + 0.25*h)
    fc_end_year <- year(fc_end_asyq)
    fc_end_quarter <- quarter(fc_end_asyq)
    fc_end <- c(fc_end_year, fc_end_quarter)
    
    
    n_obs <- length(this_series)
    
    this_series_and_lags <-  map(seq(0,max(xreg_lags)),
                                 ~ lag.xts(this_series, k = .)) %>% 
      reduce(ts.union)
  
    this_series_and_lags_for_fc <- window(this_series_and_lags, 
                                          start = fc_start,
                                          end = fc_end)
    
    if (is.null(dim(this_series_and_lags_for_fc))) {
      dim(this_series_and_lags_for_fc) <- c(
        length(this_series_and_lags_for_fc), 1)
    }
    
    colnames(this_series_and_lags_for_fc) <- paste(
      this_xname, seq(0,max(xreg_lags)), sep = "_")


    
    # print("this_series_and_lags_for_fc")
    # print(this_series_and_lags_for_fc)

    
    if ( force.constant & (this_d + this_D) >= 2 ) {
      
      nam <- colnames(this_series_and_lags_for_fc)
      
      xtrend_fc <- seq(length(this_arimax$x) + 1, 
                       length(this_arimax$x) + 
                         nrow(this_series_and_lags_for_fc))^2
      
      this_series_and_lags_for_fc <- ts.union(xtrend_fc,
                                              this_series_and_lags_for_fc)
      
      # print("xtrend_fc")
      # print(xtrend_fc)
      
      colnames(this_series_and_lags_for_fc) <- c("sq_trend", nam)
      
    }
    
    
    # print("this_series_and_lags_for_fc with trend")
    # 
    # print("this_series_and_lags_for_fc")
    # print(this_series_and_lags_for_fc)
    
    # print("This is forecast xreg")
    # print("This is forecast xreg")
    # print("This is forecast xreg")
    # print("This is forecast xreg")
    # print("this_series")
    # print(this_series)
    # print("fc_start")
    # print(fc_start)
    # print("this_arimax")
    # print(this_arimax)
    # print("h")
    # print(h)
    # print("xreg for fc")
    # print(this_series_and_lags_for_fc)
    

  
    
    this_fc <- forecast(this_arimax, h = h, xreg = this_series_and_lags_for_fc)
    
    # print("this_fc")
    # print(this_fc)
    
    fc_list[[i]] <- this_fc
    
    # print("this_fc")
    # print(this_fc)
    
  }
  
  # print(vec_of_names)
  
  names(fc_list) <- vec_of_names
  return(fc_list)
  
}


from_diff_to_yoy_accu <- function(yoy_ts, diff_ts, level_ts, training_length,
                                  n_cv, h_max, cv_fcs_one_model, 
                                  return_all_ts = FALSE ) {
  
  undiff_stuff <- cv_obs_fc_back_from_diff(yoy_ts = yoy_ts, diff_ts = diff_ts,
                                           level_ts = level_ts,
                                           training_length = training_length,
                                           n_cv = n_cv, h_max = h_max,
                                           cv_fcs_one_model = cv_fcs_one_model)
  
  cv_test_sets_yoy <- undiff_stuff$test_obs_yoy
  cv_fc_yoy <- undiff_stuff$fcs_yoy
  
  cv_test_sets_yoy_mat <- reduce(cv_test_sets_yoy, rbind)
  cv_fcs_yoy_mat <- reduce(cv_fc_yoy, rbind)
  
  accu_yoy <- fcs_accu(fc_mat = cv_fcs_yoy_mat, test_data_mat = cv_test_sets_yoy_mat) 
  
  if (return_all_ts) {
    return(list(accu_yoy = accu_yoy, cv_test_sets_yoy = cv_test_sets_yoy,
                cv_fc_yoy = cv_fc_yoy))
  } else{
    return(accu_yoy)
  }
}


from_diff_to_lev_accu <- function(yoy_ts, diff_ts, level_ts, training_length,
                                  n_cv, h_max, cv_fcs_one_model, 
                                  return_all_ts = FALSE ) {
  
  undiff_stuff <- cv_obs_fc_back_from_diff(yoy_ts = yoy_ts, diff_ts = diff_ts,
                                           level_ts = level_ts,
                                           training_length = training_length,
                                           n_cv = n_cv, h_max = h_max,
                                           cv_fcs_one_model = cv_fcs_one_model)
  
  cv_test_sets_lev <- undiff_stuff$test_obs_level
  cv_fc_lev <- undiff_stuff$fcs_level
  
  cv_test_sets_lev_mat <- reduce(cv_test_sets_lev, rbind)
  cv_fcs_lev_mat <- reduce(cv_fc_lev, rbind)
  
  accu_lev <- fcs_accu(fc_mat = cv_fcs_lev_mat, test_data_mat = cv_test_sets_lev_mat) 
  
  if (return_all_ts) {
    return(list(accu_lev = accu_lev, cv_test_sets_lev = cv_test_sets_lev,
                cv_fc_lev = cv_fc_lev))
  } else{
    return(accu_yoy)
  }
  
}

get_arima_results <- function(country_name, read_results = FALSE, 
                              data_folder = "./data/excel/",
                              arima_rds_path = "data/sarimax_objects_",
                              h_max = 8, final_ext_horizon = c(2019, 12),
                              train_span = 16, number_of_cv = 8,
                              test_length = 8, use_demetra = TRUE, 
                              do_auto = FALSE, use_dm_force_constant = FALSE,
                              data_is_log_log = TRUE, lambda_0_in_auto = FALSE,
                              mean_logical_in_auto = FALSE, max_x_lag = 2,
                              external_data_path = "./data/external/external.xlsx",
                              arima_res_suffix = "foo") {
  
  if(read_results) {
    print("Reading previously estimated arima results")
    # rds_file_name = paste0("data/sarimax_objects_", country_name,".rds")
    rds_file_name = paste0(arima_rds_path, country_name,".rds")
    arima_res <- readRDS(file = rds_file_name)
    return(arima_res)
    
  } else {
    print("Estimating a new set of arima results")
    final_forecast_horizon <- final_ext_horizon
    
    # data_path <- paste0("./data/excel/", country_name,".xlsx")
    data_path <- paste0(data_folder, country_name,".xlsx")
    
    all_arima_data <- ts_data_for_arima(data_path = data_path, 
                                        external_data_path = external_data_path,
                                        all_logs = data_is_log_log)
    
    this_rgdp_ts <- all_arima_data[["rgdp_ts"]]
    this_internal_monthly_ts <- all_arima_data[["monthly_ts"]]
    this_external_monthly_ts <- all_arima_data[["external_monthly_ts"]]
    
    internal_monthly_names <- colnames(this_internal_monthly_ts)
    external_monthly_names <- colnames(this_external_monthly_ts)
    
    if (use_demetra) {
      do_auto <- FALSE
      demetra_output <- get_demetra_params(data_path)
      # print(demetra_output)
      demetra_output_external <- get_demetra_params(external_data_path)
      
      rgdp_order_list <- demetra_output[["rgdp_order_list"]][[1]]
      
      fit_arima_rgdp_list_dem <- fit_arimas(
        y_ts = this_rgdp_ts, order_list = demetra_output[["rgdp_order_list"]],
        this_arima_names = "rgdp")
      
      this_rgdp_arima <- fit_arima_rgdp_list_dem
      
      monthly_with_demetra_info <- names(demetra_output[["monthly_order_list"]])
      log_vec <- internal_monthly_names %in% monthly_with_demetra_info
      if (any(!log_vec)) {
        print("At least one of the variables in monthly does not have a DEMETRA line and will not be considered.")
        print("These variables are:")
        print(internal_monthly_names[!log_vec])
        internal_monthly_names <- internal_monthly_names[log_vec]
        this_internal_monthly_ts <- this_internal_monthly_ts[, internal_monthly_names]
      }
      
      if (use_dm_force_constant) {
        this_non_external <- "non_external_dm_s" 
        this_external <- "external_dm_s" 
        do_dm_strict <-  FALSE
      } else {
        this_non_external <- "non_external_dm_r" 
        this_external <- "external_dm_r" 
        do_dm_strict <-  TRUE
      }
    } else {
      do_auto <- TRUE
      fit_arima_rgdp_list_auto <- fit_arimas(y_ts = this_rgdp_ts, auto = TRUE,  
                                             this_arima_names = "rgdp", 
                                             my_lambda = NULL,
                                             do_approximation = TRUE)
      this_rgdp_arima <- fit_arima_rgdp_list_auto
      
      gdp_order <- get_order_from_arima(this_rgdp_arima)[[1]]
      rgdp_order <-  gdp_order[c("p", "d", "q")]
      rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
      
      rgdp_order_list <- list(order = rgdp_order, seasonal = rgdp_seasonal)
      
      do_dm_strict <-  TRUE
      this_non_external <- "non_external_auto_r" 
      this_external <- "external_auto_r"
      demetra_output <- NULL
      demetra_output_external <- NULL
    }
    
    rgdp_uncond_fc <- forecast(this_rgdp_arima[["rgdp"]], h = h_max)
    rgdp_uncond_fc_mean <- rgdp_uncond_fc$mean
    

    extended_data <- get_extended_monthly_variables(
      do_auto = do_auto,
      use_demetra = use_demetra,
      do_dm_strict = do_dm_strict,
      do_dm_force_constant = use_dm_force_constant,
      monthly_data_ts = this_internal_monthly_ts, 
      monthly_data_external_ts = this_external_monthly_ts,
      order_list = demetra_output,
      order_list_external = demetra_output_external,
      data_path = data_path,
      final_forecast_horizon = final_forecast_horizon)

    
    internal_mdata_ext_ts <- extended_data[[this_non_external]][["quarterly_series_ts"]]
    external_mdata_ext_ts <- extended_data[[this_external]][["quarterly_series_ts"]]
    
    mdata_ext_ts <- ts.union(internal_mdata_ext_ts, external_mdata_ext_ts)
    monthly_names <- c(internal_monthly_names, external_monthly_names)
    colnames(mdata_ext_ts) <- monthly_names
    
    cv_cond_uncond <- get_cv_obj_cond_uncond(y_ts = this_rgdp_ts, 
                                             xreg_ts = mdata_ext_ts,
                                             rgdp_arima = this_rgdp_arima,
                                             max_x_lag = max_x_lag,
                                             rgdp_order_list = rgdp_order_list,
                                             n_cv = number_of_cv, 
                                             test_length = test_length,
                                             data_is_log_log = data_is_log_log, 
                                             training_length = train_span,
                                             h_max = h_max)

    arimax_and_fcs <- get_arimax_and_fcs(y_ts = this_rgdp_ts, 
                                         xreg_ts = mdata_ext_ts,
                                         rgdp_arima = this_rgdp_arima,
                                         max_x_lag = max_x_lag,
                                         rgdp_order_list = rgdp_order_list,
                                         h_max = h_max)

    fcs_aggr_transf <- aggregate_and_transform_fcs(
      arimax_and_fcs, cv_cond_uncond, rgdp_ts = this_rgdp_ts, h_max = h_max,
      rgdp_uncond_fc_mean = rgdp_uncond_fc_mean)

    
    arima_res_1 <- fcs_aggr_transf
    arima_res_2 <- list(
      mdata_ext_ts = mdata_ext_ts,
      rgdp_ts_in_arima = this_rgdp_ts,
      all_raw_fcs = arimax_and_fcs$all_fcs,
      all_arimax = arimax_and_fcs$all_arimax,
      var_lag_order_season = arimax_and_fcs$var_lag_order_season,
      uncond_fc = rgdp_uncond_fc_mean,
      uncond_yoy_fc = fcs_aggr_transf$rgdp_uncond_yoy_fc_mean)
    
    arima_res <- c(arima_res_1, arima_res_2)
    
    rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
    saveRDS(object = arima_res, file = rds_file_name)
    
    return(arima_res)
  }
  
  
}




get_arima_results_old <- function(country_name, read_results = FALSE, 
                              data_folder = "./data/excel/",
                              arima_rds_path = "data/sarimax_objects_",
                              h_max = 8, final_ext_horizon = c(2020, 12),
                              train_span = 16, number_of_cv = 8) {
  
  if(read_results) {
    print("Reading previously estimated arima results")
    # rds_file_name = paste0("data/sarimax_objects_", country_name,".rds")
    rds_file_name = paste0(arima_rds_path, country_name,".rds")
    arima_res <- readRDS(file = rds_file_name)
    return(arima_res)
    
  } else {
    print("Estimating a new set of arima results")
    final_forecast_horizon <- final_ext_horizon
    # data_path <- paste0("./data/excel/", country_name,".xlsx")
    data_path <- paste0(data_folder, country_name,".xlsx")
    
    arima_res <- bsarimax_as_function(data_path = data_path, number_of_cv = number_of_cv,
                                      train_span = train_span, h_max = h_max,
                                      final_forecast_horizon = final_forecast_horizon)
    
    rds_file_name = paste0(arima_rds_path, country_name,".rds")
    saveRDS(object = arima_res, file = rds_file_name)
    
    return(arima_res)
  }
  
  
}



get_arimax_and_fcs <- function(y_ts, xreg_ts, rgdp_arima, max_x_lag,
                               rgdp_order_list, h_max, force.constant = FALSE,
                               data_is_log_log = FALSE) {
  
  gdp_order <- get_order_from_arima(rgdp_arima)[[1]]
  rgdp_order <-  gdp_order[c("p", "d", "q")]
  rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
  demetra_rgdp_mean_logical <- rgdp_order_list[["mean_logical"]]
  monthly_names <- colnames(xreg_ts)
  
  all_arimax_list <- list()
  
  for (i in 0:max_x_lag) {
    this_arimax <- my_arimax(y_ts = y_ts, xreg_ts = xreg_ts,  y_order = rgdp_order, 
                             y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
                             xreg_lags = 0:i,
                             y_include_mean = demetra_rgdp_mean_logical,
                             force.constant = force.constant)
    
    all_arimax_list[[i + 1]] <- this_arimax
  }
  
  names(all_arimax_list) <- paste0("arimax_", seq(0, length(all_arimax_list) - 1))
  
  all_arimax <- as_tibble(all_arimax_list) %>% 
    mutate(id_fc = monthly_names) %>% 
    gather(key = "type_arimax", value = "arimax", -id_fc) %>% 
    mutate(lag = as.integer(str_remove(type_arimax, "arimax_")), 
           armapar = map(arimax, c("arma")),
           arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
           arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
    )
  
  all_fc_list <- list()
  for (i in 0:max_x_lag) {
    this_fc <- forecast_xreg(all_arimax_list[[i+1]], xreg_ts, h = h_max, 
                             vec_of_names = monthly_names, xreg_lags = 0:i,
                             force.constant = force.constant )
    
    all_fc_list[[i + 1]] <- this_fc
  }
  
  names(all_fc_list) <- paste0("fc_", seq(0, length(all_arimax_list) - 1))
  
  
  all_fcs <- as_tibble(all_fc_list)   %>% 
    mutate(id_fc = monthly_names)  %>%
    gather(key = "type_fc", value = "fc", -id_fc) %>% 
    mutate(lag = as.integer(str_remove(type_fc, "fc_")),
           raw_rgdp_fc = map(fc, "mean")) %>% 
    mutate(armapar = map(fc, c("model", "arma")),
           arima_order = map(armapar, function(x) x[c(1, 6, 2)]),
           arima_seasonal = map(armapar, function(x) x[c(3, 7, 4)])  
    ) %>% 
    mutate(data_and_fc = map(raw_rgdp_fc, ~ts(data = c(y_ts, .), frequency = 4,
                                              start = stats::start(y_ts))),
           yoy_data_and_fc = map(data_and_fc, ~ make_yoy_ts( . , is_log = data_is_log_log)),
           yoy_raw_rgdp_fc = map2(yoy_data_and_fc, raw_rgdp_fc,
                                  ~ window(.x, start = stats::start(.y)))
    )
  
  # all_fcs1 <- all_fcs[1,]
  
  var_lag_order_season <- all_fcs %>% 
    dplyr::select(id_fc, lag, arima_order, arima_seasonal) %>% 
    rename(variable = id_fc, lag = lag)
  
  rgdp_var_lag_order_season <- tibble(
    variable = "rgdp", lag = 0, 
    arima_order = list(rgdp_order), arima_seasonal = list(rgdp_seasonal)) 
  
  var_lag_order_season <- rbind(rgdp_var_lag_order_season, var_lag_order_season)
  
  mat_of_raw_fcs <- reduce(all_fcs$raw_rgdp_fc, rbind) 
  
  return(list(
    all_arimax = all_arimax,
    all_fcs = all_fcs,
    var_lag_order_season = var_lag_order_season,
    mat_of_raw_fcs = mat_of_raw_fcs
  ))
}



get_cv_of_arimax <- function(y_ts, xreg_ts, y_order, y_seasonal, x_names, 
                             test_length = 8, n_cv = 8, training_length = 16, 
                             method = "ML", max_x_lag = 2, data_is_log_log = FALSE,
                             y_include_drift = TRUE, rgdp_order_list = NULL,
                             force.constant = FALSE) {
  
  # demetra_rgdp_mean_logical <-  rgdp_order_list[["mean_logical"]]
  
  list_cv_of_arimax <- list()
  
  print("in get_cv_of_arimax max_x_lag is")
  print(max_x_lag)
  
  for (i in 0:max_x_lag) {
    this_cv_arimax <- cv_arimax(y_ts = y_ts, 
                                xreg_ts = xreg_ts,
                                n_cv = n_cv,
                                training_length = training_length, 
                                h_max = test_length,
                                y_order = y_order, 
                                y_seasonal = y_seasonal, 
                                vec_of_names = x_names, 
                                method = method,
                                xreg_lags = 0:i, 
                                data_is_log_log = data_is_log_log, 
                                y_include_drift = y_include_drift,
                                force.constant = force.constant)
    
    list_cv_of_arimax[[i + 1]]  <- this_cv_arimax
  }
  
  return(list_cv_of_arimax)
  
}



get_cv_obj_cond_uncond <- function(y_ts, xreg_ts, rgdp_arima, max_x_lag, 
                                   rgdp_order_list, n_cv, test_length, 
                                   data_is_log_log, training_length, h_max,
                                   force.constant = FALSE) {
  
  gdp_order <- get_order_from_arima(rgdp_arima)[[1]]
  rgdp_order <-  gdp_order[c("p", "d", "q")]
  rgdp_seasonal <-  gdp_order[c("P", "D", "Q")]
  rgdp_mean_logical <- rgdp_order_list[["mean_logical"]]
  monthly_names <- colnames(xreg_ts)
  
  
  cv_arimax_0_to_2 <- get_cv_of_arimax(
    y_ts = y_ts, xreg_ts = xreg_ts, y_order = rgdp_order, 
    training_length = training_length, test_length = test_length,
    y_seasonal = rgdp_seasonal, x_names = monthly_names, 
    data_is_log_log = data_is_log_log, n_cv = n_cv,
    max_x_lag = max_x_lag, y_include_drift = rgdp_mean_logical, 
    force.constant = force.constant
  )
  
  cv_rgdp_arima_list <- cv_arima(y_ts = y_ts, h_max = h_max, n_cv = n_cv,
                        training_length = training_length, y_order = rgdp_order, 
                        y_seasonal = rgdp_seasonal, method = "ML",  
                        y_include_drift = rgdp_mean_logical, 
                        data_is_log_log = data_is_log_log,
                        force.constant = force.constant)
  
  # print("names(cv_rgdp_arima)")
  # print(names(cv_rgdp_arima))
  
  cv_rgdp_arima <- cv_rgdp_arima_list[["cv_errors"]]
  cv_rdgp_rmse <- compute_rmse(cv_rgdp_arima, h_max = h_max, n_cv = n_cv)

  cv_rgdp_arima_yoy <- cv_rgdp_arima_list[["cv_yoy_errors"]]
  cv_rdgp_rmse_yoy <- compute_rmse(cv_rgdp_arima_yoy, h_max = h_max, n_cv = n_cv)
  
  cv_rgdp_arima_logdiff <- cv_rgdp_arima_list[["cv_logdiff_errors"]]
  cv_rdgp_rmse_logdiff <- compute_rmse(cv_rgdp_arima_logdiff, h_max = h_max, n_cv = n_cv)
  
  cv_rgdp_arima_percent <- cv_rgdp_arima_list[["cv_percent_errors"]]
  cv_rdgp_rmse_percent <- compute_rmse(cv_rgdp_arima_percent, h_max = h_max, n_cv = n_cv)

  cv_rmse_each_h_rgdp <- cv_rdgp_rmse[["same_h_rmse"]] %>% 
  mutate(variable = "rgdp", lag = 0)
  
  cv_rmse_each_h_rgdp_yoy <- cv_rdgp_rmse_yoy[["same_h_rmse"]] %>% 
  mutate(variable = "rgdp", lag = 0)
  
  cv_rmse_each_h_rgdp_logdiff <- cv_rdgp_rmse_logdiff[["same_h_rmse"]] %>% 
    mutate(variable = "rgdp", lag = 0)
  
  cv_rmse_each_h_rgdp_percent <- cv_rdgp_rmse_percent[["same_h_rmse"]] %>% 
    mutate(variable = "rgdp", lag = 0)
  
  cv_allx <- map(cv_arimax_0_to_2, "cv_errors_all_pairs_yx")
  cv_allx_yoy <- map(cv_arimax_0_to_2, "cv_yoy_errors_all_pairs_yx")
  cv_allx_logdiff <- map(cv_arimax_0_to_2, "cv_logdiff_errors_all_pairs_yx")
  cv_allx_percent <- map(cv_arimax_0_to_2, "cv_percent_errors_all_pairs_yx")
  
  cv_rmse_list <- map(cv_allx,  ~ map(., compute_rmse, h_max = h_max, n_cv = n_cv))
  cv_rmse_list_yoy <- map(cv_allx_yoy, 
                          ~ map(., compute_rmse, h_max = h_max, n_cv = n_cv))
  cv_rmse_list_logdiff <- map(cv_allx_logdiff, 
                          ~ map(., compute_rmse, h_max = h_max, n_cv = n_cv))
  cv_rmse_list_percent <- map(cv_allx_percent, 
                          ~ map(., compute_rmse, h_max = h_max, n_cv = n_cv))

  cv_rmse_each_h <- map(cv_rmse_list,
                        ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
                          mutate(variable = monthly_names))
  cv_rmse_each_h_yoy <- map(cv_rmse_list_yoy,
                            ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
                              mutate(variable = monthly_names))
  cv_rmse_each_h_logdiff <- map(cv_rmse_list_logdiff,
                            ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
                              mutate(variable = monthly_names))
  cv_rmse_each_h_percent <- map(cv_rmse_list_percent,
                            ~ map(., "same_h_rmse") %>% reduce(., rbind) %>% 
                              mutate(variable = monthly_names))
  
  
  
  cv_all_x_rmse_each_h <- reduce(cv_rmse_each_h, rbind) %>% 
    mutate(lag =   reduce(
      map(seq(0,length(cv_allx) - 1), rep, length(cv_allx[[1]])),
      c))
  
  cv_all_x_rmse_each_h_yoy <- reduce(cv_rmse_each_h_yoy, rbind) %>% 
    mutate(lag = reduce(
      map(seq(0,length(cv_allx_yoy) - 1), rep, length(cv_allx_yoy[[1]])), c))
  
  cv_all_x_rmse_each_h_logdiff <- reduce(cv_rmse_each_h_logdiff, rbind) %>% 
    mutate(lag = reduce(
      map(seq(0,length(cv_allx_logdiff) - 1), rep, length(cv_allx_logdiff[[1]])), c))

  cv_all_x_rmse_each_h_percent <- reduce(cv_rmse_each_h_percent, rbind) %>% 
    mutate(lag = reduce(
      map(seq(0,length(cv_allx_percent) - 1), rep, length(cv_allx_percent[[1]])), c))
  
  return(list(
    cv_allx = cv_allx, 
    cv_allx_yoy = cv_allx_yoy,
    cv_allx_logdiff = cv_allx_logdiff,
    cv_allx_percent = cv_allx_percent,
    cv_rmse_each_h = cv_rmse_each_h, 
    cv_rmse_each_h_yoy = cv_rmse_each_h_yoy,
    cv_rmse_each_h_logdiff = cv_rmse_each_h_logdiff,
    cv_rmse_each_h_percent = cv_rmse_each_h_percent,
    cv_rmse_each_h_rgdp = cv_rmse_each_h_rgdp,
    cv_rmse_each_h_rgdp_yoy = cv_rmse_each_h_rgdp_yoy,
    cv_rmse_each_h_rgdp_logdiff = cv_rmse_each_h_rgdp_logdiff,
    cv_rmse_each_h_rgdp_percent = cv_rmse_each_h_rgdp_percent,
    cv_rgdp_arima = cv_rgdp_arima,
    cv_rgdp_arima_yoy = cv_rgdp_arima_yoy,
    cv_rgdp_arima_logdiff = cv_rgdp_arima_logdiff,
    cv_rgdp_arima_percent = cv_rgdp_arima_percent,
    cv_all_x_rmse_each_h = cv_all_x_rmse_each_h,
    cv_all_x_rmse_each_h_yoy = cv_all_x_rmse_each_h_yoy,
    cv_all_x_rmse_each_h_logdiff = cv_all_x_rmse_each_h_logdiff,
    cv_all_x_rmse_each_h_percent = cv_all_x_rmse_each_h_percent)
)
  
  
}


get_data <- function(country_name, data_path = "./data/excel/", 
                     data_transform = "level", apply_log = FALSE) {
  
  file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
  file_paths <- paste0(data_path, file_names)
  country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")
  
  general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                                      "month", "conf_emp", "conf_ibre", "ip_ine", 
                                      "vta_auto", "exist"))
  # to make the data work we have to delete "m2" for argentina, "imp_int", "imp_k" for Ecuador and 
  # "imp_consumer", "imp_intermediate", "imp_capital" for Mexico
  extra_vars_to_drop <- list(Argentina = c("m2", "ri", "", "", "", "", "", "", "", "", ""), 
                             Bolivia = c("igae", "", "", "", "", "", "", "", "", "", "", ""), 
                             Brasil = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Chile = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Colombia = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Ecuador = c("imp_int", "imp_k", "", "", "", "", "", "", "", "", "", ""), 
                             Mexico = c("imp_consumer", "imp_intermediate", "imp_capital", "", "", "", "", "", "", "", "", ""), 
                             Paraguay = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Peru = c("expec_demand", "", "", "", "", "", "", "", "", "", "", ""),
                             Uruguay = c("cred", "", "", "", "", "", "", "", "", "", "", ""))
  
  variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)
  
  data_qm_xts <- get_gdp_shaped_data(data_path = data_path, 
                                     list_variables_to_drop = variables_to_drop,
                                     only_complete_cases = TRUE,
                                     apply_log = apply_log)
  
  
  if (data_transform == "level") {
    data_qm_mts <- map(data_qm_xts, to_ts_q)
    if (country_name == "all") {
      return(data_qm_mts)
    } else {
      level_data_ts <- data_qm_mts[[country_name]]
      return(level_data_ts)
    }
    
  } 
  
  if (data_transform == "yoy") {
    data_qm_xts_yoy <- map(data_qm_xts, make_yoy_xts)
    data_qm_mts_yoy <- map(data_qm_xts_yoy, to_ts_q)
    if (country_name == "all") {
      return(data_qm_mts_yoy)
    } else {
      yoy_data_ts <- data_qm_mts_yoy[[country_name]]
      return(yoy_data_ts)
    }
    
  }
  
  if (data_transform == "diff_yoy") {
    data_qm_xts_yoy_diff <- map(data_qm_xts_yoy, diff.xts, na.pad = FALSE)
    data_qm_mts_yoy_diff <- map(data_qm_xts_yoy_diff, to_ts_q)
    if (country_name == "all") {
      return(data_qm_mts_yoy_diff)
    } else {
      diff_yoy_data_ts <- data_qm_mts_yoy_diff[[country_name]]
      return(diff_yoy_data_ts)
    }
    
  }
  
  
}



get_demetra_params <- function(data_path) {
  
  optimal_demetra <- read_excel(data_path, sheet = "optimal")
  optimal_demetra <- optimal_demetra %>% 
    separate(data = ., col = 1, into = c("freq_type", "id"), sep = " - ")
  
  optimal_demetra <- optimal_demetra %>% 
    rename(p_dm = P, q_dm = Q, d_dm = D, P_dm = BP, Q_dm = BQ, D_dm = BD)
  
  rgdp_demetra <- optimal_demetra %>% 
    filter(id == "rgdp") %>% select(- freq_type)
  
  rgdp_demetra_pdqPDQ <- rgdp_demetra %>% 
    select(p_dm, d_dm, q_dm, P_dm, D_dm, Q_dm)
  
  monthly_demetra <- optimal_demetra %>% 
    filter(id != "rgdp") %>% select(- freq_type)
  
  monthly_demetra$id <- as.factor(monthly_demetra$id) 
  
  monthly_demetra_pdqPDQ <- monthly_demetra %>% 
    select(id, p_dm, d_dm, q_dm, P_dm, D_dm, Q_dm)
  
  monthly_names <- monthly_demetra$id
  
  rgdp_order <- c(rgdp_demetra$p_dm, rgdp_demetra$d_dm, rgdp_demetra$q_dm)
  rgdp_seasonal <- c(rgdp_demetra$P_dm, rgdp_demetra$D_dm, rgdp_demetra$Q_dm)
  rgdp_inc_mean <- ifelse(rgdp_demetra$Mean == 1, TRUE, FALSE)
  rgdp_to_take_log <- ifelse(rgdp_demetra$Log == 1, TRUE, FALSE)
  
  
  this_instruction <- list(order = rgdp_order, seasonal = rgdp_seasonal,
                           mean_logical = rgdp_inc_mean, 
                           log_logical = rgdp_to_take_log)
  
  rgdp_order_list <- list()
  
  rgdp_order_list[[1]] <- this_instruction 
  
  monthly_demetra_order_list <- list_along(monthly_names)
  
  for (i in seq_along(monthly_names)) {
    
    this_order <- c(monthly_demetra$p_dm[i], monthly_demetra$d_dm[i],
                    monthly_demetra$q_dm[i])
    
    this_seasonal <- c(monthly_demetra$P_dm[i], monthly_demetra$D_dm[i],
                       monthly_demetra$Q_dm[i])
    
    inc_mean <- ifelse(monthly_demetra$Mean[i] == 1, TRUE, FALSE)
    
    to_take_log <- ifelse(monthly_demetra$Log[i] == 1, TRUE, FALSE)
    
    
    this_instruction <- list(order = this_order, seasonal = this_seasonal,
                             mean_logical = inc_mean, log_logical = to_take_log)
    
    monthly_demetra_order_list[[i]] <- this_instruction
  }
  
  names(monthly_demetra_order_list) <- monthly_names
  
  return(list(monthly_order_list = monthly_demetra_order_list,
              monthly_info_tbl = monthly_demetra,
              monthly_pdqPDQ = monthly_demetra_pdqPDQ,
              rgdp_order_list = rgdp_order_list,
              rgdp_info_tbl = rgdp_demetra,
              rgdp_pqdPDQ = rgdp_demetra_pdqPDQ) 
  )
}


get_extended_monthly_variables <- function(
  monthly_data_ts, monthly_data_external_ts, final_date, order_list, 
  order_list_external, data_path, use_demetra = TRUE,
  do_dm_force_constant = FALSE, do_dm_strict = TRUE, do_auto = FALSE,
  print_comments_on_constant = FALSE,
  final_forecast_horizon = c(2019, 12), method = "ML") {
  
  # print("final_forecast_horizon")
  # print(final_forecast_horizon)
  
  
  monthly_data_names <- colnames(monthly_data_ts)
  monthly_data_external_names <- colnames(monthly_data_external_ts)
  
  gdp_and_dates <- get_rgdp_and_dates(data_path)
  start_date_gdp <- gdp_and_dates[["gdp_start"]]
  
  # at first all NULL and changed only if condition is true
  fit_arima_monthly_list_demetra_stata_constants <- NULL
  fit_arima_external_monthly_list_demetra_stata_constants <- NULL
  mdata_ext_dem_stata  <- NULL
  mdata_ext_external_dem_stata <- NULL
    
  fit_arima_monthly_list_demetra_r_constants <- NULL
  fit_arima_external_monthly_list_demetra_r_constants <- NULL
  mdata_ext_dem_r <- NULL
  mdata_ext_external_dem_r <- NULL
  
  fit_arima_external_monthly_list_auto <- NULL
  fit_arima_monthly_list_auto <- NULL
  mdata_ext_auto_r <- NULL
  mdata_ext_external_auto_r <- NULL
  
  
  if (use_demetra) {
    if (do_dm_force_constant) {
       
      # print("names(order_list)")
      # print(names(order_list))
      # 
      print("order_list[[monthly_order_list]]")
      print(order_list[["monthly_order_list"]])
      print("method")
      print(method)
      
      fit_arima_monthly_list_demetra_stata_constants <- fit_arimas(
        y_ts = monthly_data_ts, order_list = order_list[["monthly_order_list"]],
        this_arima_names = monthly_data_names,  force_constant = TRUE, freq = 12,
        print_comments_on_constant = print_comments_on_constant, method = method)
      
      # print("fit_arima_monthly_list_demetra_stata_constants")
      # print(fit_arima_monthly_list_demetra_stata_constants)
      
      
      fit_arima_external_monthly_list_demetra_stata_constants <- fit_arimas(
        y_ts = monthly_data_external_ts, 
        order_list = order_list_external[["monthly_order_list"]],
        this_arima_names = monthly_data_external_names,  
        force_constant = FALSE, 
        freq = 12, method = method, include.constant = TRUE)
      

      mdata_ext_dem_stata <- extend_and_qtr(
        data_mts = monthly_data_ts, 
        final_horizon_date = final_forecast_horizon , 
        vec_of_names = monthly_data_names,
        fitted_arima_list = fit_arima_monthly_list_demetra_stata_constants,
        start_date_gdp = start_date_gdp,
        force_constant = TRUE,
        order_list = order_list[["monthly_order_list"]])
      
      mdata_ext_external_dem_stata <- extend_and_qtr(
        data_mts = monthly_data_external_ts,
        final_horizon_date = final_forecast_horizon,
        vec_of_names = monthly_data_external_names,
        fitted_arima_list = fit_arima_external_monthly_list_demetra_stata_constants,
        start_date_gdp = start_date_gdp,
        force_constant = FALSE,
        order_list = order_list_external[["monthly_order_list"]])
      
      
    } 
    
    
    if (do_dm_strict) {
      fit_arima_monthly_list_demetra_r_constants <- fit_arimas(
        y_ts = monthly_data_ts, order_list = order_list[["monthly_order_list"]],
        this_arima_names = monthly_data_names,  force_constant = FALSE, freq = 12,
        print_comments_on_constant = print_comments_on_constant, include.constant = TRUE)
      
      fit_arima_external_monthly_list_demetra_r_constants <- fit_arimas(
        y_ts = monthly_data_external_ts, 
        order_list = order_list_external[["monthly_order_list"]],
        this_arima_names = monthly_data_external_names,  
        force_constant = FALSE, freq = 12, include.constant = TRUE)
      

      
      mdata_ext_dem_r <- extend_and_qtr(
        data_mts = monthly_data_ts, 
        final_horizon_date = final_forecast_horizon , 
        vec_of_names = monthly_data_names, 
        fitted_arima_list = fit_arima_monthly_list_demetra_r_constants,
        start_date_gdp = start_date_gdp,
        order_list = order_list[["monthly_order_list"]])
      
      mdata_ext_external_dem_r <- extend_and_qtr(
        data_mts = monthly_data_external_ts, 
        final_horizon_date = final_forecast_horizon , 
        vec_of_names = monthly_data_external_names, 
        fitted_arima_list = fit_arima_external_monthly_list_demetra_r_constants,
        start_date_gdp = start_date_gdp,
        order_list = order_list_external[["monthly_order_list"]])
      
    } 
  }

  
  
  if (do_auto) {
    fit_arima_monthly_list_auto <- fit_arimas(
      y_ts = monthly_data_ts, auto = TRUE, my_lambda = NULL, do_approximation = TRUE,
      freq = 12, this_arima_names = monthly_data_names)
    
    fit_arima_external_monthly_list_auto <- fit_arimas(
      y_ts = monthly_data_external_ts, auto = TRUE, my_lambda = NULL, 
      do_approximation = TRUE, freq = 12, 
      this_arima_names = monthly_data_external_names)
    
    # print("fit_arima_external_monthly_list_auto")
    # print(fit_arima_external_monthly_list_auto)
    
    mdata_ext_auto_r <- extend_and_qtr(
      data_mts = monthly_data_ts, 
      final_horizon_date = final_forecast_horizon , 
      vec_of_names = monthly_data_names, 
      fitted_arima_list = fit_arima_monthly_list_auto,
      start_date_gdp = start_date_gdp,
      order_list = order_list[["monthly_order_list"]])
    
    mdata_ext_external_auto_r <- extend_and_qtr(
      data_mts = monthly_data_external_ts, 
      final_horizon_date = final_forecast_horizon , 
      vec_of_names = monthly_data_external_names, 
      fitted_arima_list = fit_arima_external_monthly_list_auto,
      start_date_gdp = start_date_gdp,
      order_list = order_list_external[["monthly_order_list"]])
    
  } 
  
  
  return(list(non_external_auto_r = mdata_ext_auto_r,
              external_auto_r = mdata_ext_external_auto_r,
              non_external_dm_r = mdata_ext_dem_r,
              external_dm_r = mdata_ext_external_dem_r,
              non_external_dm_s = mdata_ext_dem_stata,
              external_dm_s = mdata_ext_external_dem_stata,
              fit_arima_m_list_dm_s = fit_arima_monthly_list_demetra_stata_constants,
              fit_arima_e_list_dm_s = fit_arima_external_monthly_list_demetra_stata_constants,
              fit_arima_m_list_dm_r = fit_arima_monthly_list_demetra_r_constants,
              fit_arima_e_list_dm_r = fit_arima_external_monthly_list_demetra_r_constants,
              fit_arima_m_list_auto = fit_arima_monthly_list_auto,
              fit_arima_e_list_auto = fit_arima_external_monthly_list_auto))
}



get_gdp_start_end <- function(data) {
  
  na_omitted_rgdp  <- data %>% dplyr::select(date, rgdp) %>% 
    filter(!is.na(rgdp)) %>% 
    summarise(start_rgdp_date = min(date), end_rgdp_date = max(date))
  
  rgdp_start <-  na_omitted_rgdp[["start_rgdp_date"]]
  rgdp_end <-  na_omitted_rgdp[["end_rgdp_date"]]
  
  return(c(start = rgdp_start, end = rgdp_end))
  
}


get_gdp_shaped_data <- function(data_path, country = NULL, 
                                only_complete_cases = FALSE,
                                list_variables_to_drop = NULL,
                                apply_log = FALSE
) {
  
  suppressMessages(data_q_m_qm <- read_gather_qm_data(
    data_path = data_path, country = country)
  )
  
  
  data_qm <- data_q_m_qm[["countries_merged_q_m"]]
  
  country_names <- names(data_qm)
  
  rgdp_dates <- map(data_qm, get_gdp_start_end)
  
  data_qm_xts <- list_along(country_names)
  
  for (i in seq_along(country_names)) {
    
    this_qm_xts <- tk_xts(data_qm[[i]], date_var = date, silent = TRUE)
    # print(this_qm_xts)
    # print(glimpse(this_qm_xts))
    
    this_dates <- rgdp_dates[[i]]
    
    this_start <- this_dates[[1]]
    this_end <- this_dates[[2]]
    
    this_qm_xts <- chop_start_end_xts(this_qm_xts, this_start, this_end)
    
    data_qm_xts[[i]] <- this_qm_xts
    
  }
  
  names(data_qm_xts) <- country_names
  
  if (!is.null(list_variables_to_drop)) {
    data_qm_xts <- map2(data_qm_xts, list_variables_to_drop, drop_this_vars)
  }
  
  if (only_complete_cases) {
    # print("balanced data of all variables")
    data_qm_xts <- map(data_qm_xts, ~ .x[complete.cases(.x) , ])
  }
  
  if (apply_log) {
    data_qm_xts <- map(data_qm_xts, log)
  }
  
  return(data_qm_xts)
  
}



get_monthly_variables <- function(data_path, vars_to_eliminate = 
                                    c("hlookup")) {
  data <- read_excel(data_path, sheet = "monthly")
  
  for (variable in vars_to_eliminate) {
    data[, variable] <- NULL 
  }
  
  return(data)
}


get_external_variables <- function(external_data_path, vars_to_eliminate = 
                                    c("hlookup")) {
  data <- read_excel(data_path, sheet = "external")
  
  for (variable in vars_to_eliminate) {
    data[, variable] <- NULL 
  }
  
  return(data)
  
}



get_order_from_arima <- function(arima_obj, suffix = NULL, 
                                 this_arima_names = NULL) {
  
  len_obj <- length(arima_obj)
  
  order_names <- c("p", "q", "P", "Q", "freq", "d", "D")
  
  if (!is.null(suffix)) {
    order_names <- paste(order_names, suffix, sep = "_")
  }
  
  order_par_list <- list_along(seq.int(1, len_obj))
  
  for (i in seq.int(1, len_obj)) {
    
    this_arima <- arima_obj[[i]]
    arma_par <- this_arima[["arma"]]
    names(arma_par) <- order_names
    
    order_par_list[[i]] <- arma_par
    
  }
  
  
  names(order_par_list) <- this_arima_names
  
  return(order_par_list)
  
}



get_raw_data_ts <- function(country = NULL, data_path = "./data/excel/"){
  
  file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
  file_paths <- paste0(data_path, file_names)
  country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")  
  names(file_paths) <- country_names
  names(file_names) <- country_names
  
  general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                                      "month", "conf_emp", "conf_ibre", "ip_ine", 
                                      "vta_auto", "exist"))
  # to make the data work we have to delete "m2" for argentina, "imp_int", "imp_k" for Ecuador and 
  # "imp_consumer", "imp_intermediate", "imp_capital" for Mexico
  extra_vars_to_drop <- list(Argentina = c("emae", "", "", "", "", "", "", "", "", "", ""), 
                             Bolivia = c("igae", "", "", "", "", "", "", "", "", "", "", ""), 
                             Brasil = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Chile = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Colombia = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Ecuador = c("imp_int", "imp_k", "", "", "", "", "", "", "", "", "", ""), 
                             Mexico = c("imp_consumer", "imp_intermediate", "imp_capital", "", "", "", "", "", "", "", "", ""), 
                             Paraguay = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Peru = c("expec_demand", "", "", "", "", "", "", "", "", "", "", ""),
                             Uruguay = c("cred", "imp_nonpetro", "", "", "", "", "", "", "", ""))
  
  variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)
  
  if (!is.null(country)) {
    file_paths <- file_paths[country]
    file_names <- file_names[country]
    country_names <- country
  }
  
  if (length(country == 1)) {
    is_single_country <- TRUE
  } else {
    is_single_country <- FALSE
  }
  
  all_files_q <- list_along(country_names)
  all_files_m <- list_along(country_names)
  all_files_m_q <- list_along(country_names)
  countries_merged_q_m <- list_along(country_names)
  countries_merged_q_m_ts <- list_along(country_names)
  
  
  
  for (i in seq_along(country_names)) {
    
    this_q <- read_excel(file_paths[i], sheet = "quarterly")
    this_q <- as_tbl_time(this_q, index = date)
    this_q <- dplyr::select(this_q, -c(year, hlookup))
    
    this_country <- country_names[i]
    this_variables_to_drop <- variables_to_drop[[this_country]]
    
    
    if(country_names[i] == "Uruguay") {
      this_q[, "rm"] <- - this_q[, "rm"]
    }
    
    
    all_files_q[[i]] <- this_q
    
    this_m <- read_excel(file_paths[i], sheet = "monthly")
    this_m <- as_tbl_time(this_m, index = date)
    all_files_m[[i]] <- this_m
    
    this_m_q <- this_m  %>%
      collapse_by(period = "quarterly") %>%
      group_by(date) %>% transmute_all(mean) %>%
      distinct(date, .keep_all = TRUE) %>% 
      ungroup() 
    
    all_files_m_q[[i]] <- this_m_q
    
    m_and_q <- left_join(this_q, this_m_q, by = "date")
    
    # this_vars_to_drop <- variables_to_drop[[i]]
    m_and_q <- drop_this_vars(m_and_q, this_variables_to_drop)
    
    # m_and_q$year <- NULL
    # m_and_q$quarter <- NULL
    # m_and_q$month <- NULL
    # m_and_q$hlookup <- NULL
    # m_and_q$trim <- NULL
    
    
    maq_start <- first(tk_index(m_and_q))
    m_and_q_ts <- suppressWarnings(tk_ts(m_and_q, frequency = 4, 
                                         start = c(year(maq_start), quarter(maq_start))))
    
    countries_merged_q_m[[i]] <- m_and_q
    countries_merged_q_m_ts[[i]] <- m_and_q_ts
    
  }
  
  names(all_files_q) <- country_names
  names(all_files_m) <- country_names
  names(all_files_m_q) <- country_names
  names(countries_merged_q_m) <- country_names
  names(countries_merged_q_m_ts) <- country_names
  
  # countries_merged_q_m <- countries_merged_q_m %>% 
  #   dplyr::select(-c(year, quarter, month, hlookup))
  
  if (is_single_country) {
    return(countries_merged_q_m_ts[[1]])
  } else {
    return(countries_merged_q_m_ts)
  }
  
  
}





get_reco <- function(country_name, variable_name, data_transform) {
  
  level_data_ts <- get_data(country_name = country_name,
                            data_transform = data_transform, apply_log = FALSE)
  level_rgdp_ts <- level_data_ts[ , variable_name]
  this_series <- level_rgdp_ts
  
  stdata <- suppressWarnings(comb_ndiffs(level_rgdp_ts)) %>% 
    arrange(test, deter_part, alpha)
  
  stdata$country <- country_name
  
  unanim <- stdata %>% 
    mutate(unanimity = min(recommendation) == max(recommendation),
           unanimity = ifelse(unanimity, recommendation, NA)) %>% 
    dplyr::select(country, unanimity) %>% 
    unique()
  
  unanim_deter_level <- stdata %>%
    filter(deter_part == "level" ) %>% 
    mutate(unan_level = min(recommendation) == max(recommendation),
           unan_level = ifelse(unan_level, recommendation, NA)) %>% 
    select(country, unan_level) %>% 
    unique()
  
  unanim_05_deter_level <- stdata %>%
    filter(deter_part == "level", alpha == 0.05 ) %>% 
    mutate(unan_05_level = min(recommendation) == max(recommendation),
           unan_05_level = ifelse(unan_05_level, recommendation, NA)) %>% 
    select(country, unan_05_level) %>% 
    unique()
  
  unanim_kpss <- stdata %>% 
    filter(test == "kpss") %>% 
    mutate(unan_kpss = min(recommendation) == max(recommendation),
           unan_kpss = ifelse(unan_kpss, recommendation, NA)) %>% 
    select(country, unan_kpss) %>% 
    unique()
  
  unanim_kpss_level <- stdata %>% 
    filter(test == "kpss", deter_part == "level") %>% 
    mutate(unan_kpss_lev = min(recommendation) == max(recommendation),
           unan_kpss_lev = ifelse(unan_kpss_lev, recommendation, NA)) %>% 
    select(country, unan_kpss_lev) %>% 
    unique()
  
  kpss_reco <- stdata %>% 
    filter(test == "kpss", deter_part == "level", alpha == 0.05) %>%
    select(country, recommendation) %>% 
    rename(kpss_05_level = recommendation)
  
  country_recos <- left_join(unanim, unanim_deter_level, by = "country") %>% 
    left_join(unanim_05_deter_level, by = "country") %>% 
    left_join(unanim_kpss, by = "country") %>% 
    left_join(unanim_kpss_level, by = "country") %>% 
    left_join(kpss_reco, by = "country")
  
  country_recos$variable <- variable_name
  
  yoy_reco <- stdata %>% 
    filter(recommendation == "yoy")
  
  diff_yoy_reco <- stdata %>% 
    filter(recommendation == "diff_yoy")
  
  return(country_recos)
}



get_reco_from_sta <- function(stdata, variable_name) {
  
  unanim <- stdata %>% 
    mutate(unanimity = min(recommendation) == max(recommendation),
           unanimity = ifelse(unanimity, recommendation, NA)) %>% 
    dplyr::select(country, unanimity) %>% 
    unique()
  
  unanim_deter_level <- stdata %>%
    filter(deter_part == "level" ) %>% 
    mutate(unan_level = min(recommendation) == max(recommendation),
           unan_level = ifelse(unan_level, recommendation, NA)) %>% 
    select(country, unan_level) %>% 
    unique()
  
  unanim_05_deter_level <- stdata %>%
    filter(deter_part == "level", alpha == 0.05 ) %>% 
    mutate(unan_05_level = min(recommendation) == max(recommendation),
           unan_05_level = ifelse(unan_05_level, recommendation, NA)) %>% 
    select(country, unan_05_level) %>% 
    unique()
  
  unanim_kpss <- stdata %>% 
    filter(test == "kpss") %>% 
    mutate(unan_kpss = min(recommendation) == max(recommendation),
           unan_kpss = ifelse(unan_kpss, recommendation, NA)) %>% 
    select(country, unan_kpss) %>% 
    unique()
  
  unanim_kpss_level <- stdata %>% 
    filter(test == "kpss", deter_part == "level") %>% 
    mutate(unan_kpss_lev = min(recommendation) == max(recommendation),
           unan_kpss_lev = ifelse(unan_kpss_lev, recommendation, NA)) %>% 
    select(country, unan_kpss_lev) %>% 
    unique()
  
  kpss_reco <- stdata %>% 
    filter(test == "kpss", deter_part == "level", alpha == 0.05) %>%
    select(country, recommendation) %>% 
    rename(kpss_05_level = recommendation)
  
  country_recos <- left_join(unanim, unanim_deter_level, by = "country") %>% 
    left_join(unanim_05_deter_level, by = "country") %>% 
    left_join(unanim_kpss, by = "country") %>% 
    left_join(unanim_kpss_level, by = "country") %>% 
    left_join(kpss_reco, by = "country")
  
  country_recos$variable <- variable_name
  
  # yoy_reco <- stdata %>% 
  #   filter(recommendation == "yoy")
  # 
  # diff_yoy_reco <- stdata %>% 
  #   filter(recommendation == "diff_yoy")
  
  return(country_recos)
}


get_rmses_h_rakings_h <- function(data = cv_objects, h_max = 6){
  cv_errors <- data[["cv_errors"]]
  
  all_rmses <- map(cv_errors, function(x) sqrt(colMeans( (reduce(x, rbind))^2))  )
  all_rmses_tbl <- reduce(all_rmses, rbind)
  rmse_names <- paste0("rmse_", 1:h_max)
  colnames(all_rmses_tbl) <- rmse_names
  row.names(all_rmses_tbl) <- NULL
  
  
  for (r in seq_along(rmse_names)) {
    this_rmse <- rmse_names[r]
    rmse_vec <- all_rmses_tbl[, this_rmse]
    this_rank <- rank(rmse_vec)
    all_rmses_tbl <- cbind(all_rmses_tbl, this_rank)
    
  }
  
  ranking_names <- paste0("rank_", 1:h_max)
  rmse_and_rank_names <- c(rmse_names, ranking_names)
  colnames(all_rmses_tbl) <- rmse_and_rank_names
  
  rmse_each_h <- cbind(data, all_rmses_tbl)
  
  return(rmse_each_h)
  
}


get_sets_of_variables <- function(df, this_size, all_variables, 
                                  already_chosen, bt_factor,
                                  maxlag_ccm = 12) {
  
  len_already_chosen <- length(already_chosen)
  len_other_vbls <- this_size - len_already_chosen
  
  tiao_box_treshold <- 2 / sqrt(nrow(df))
  tresh <- bt_factor * tiao_box_treshold
  
  p_and_ccm_mat <- ccm(df, output = FALSE, lags = maxlag_ccm)
  
  ccm_mat <- p_and_ccm_mat$ccm
  ccm_mat_rgdp <- ccm_mat[1:ncol(df) ,]
  geq_cor <- abs(ccm_mat_rgdp) >= tresh
  geq_cor_row_sums <- rowSums(geq_cor) 
  geq_cor_variables <- geq_cor_row_sums >= 1
  
  passing_variables <- all_variables[geq_cor_variables]
  
  passing_not_alr_chosen <- passing_variables[!passing_variables %in% already_chosen]
  
  n_passing_vbls <- length(passing_not_alr_chosen)
  
  print(paste("We have", n_passing_vbls, "variables, to fill", len_other_vbls,
              "slots in the VAR.Total possible combinations :",
              choose(n_passing_vbls, len_other_vbls)))
  
  combinations <- combn(passing_not_alr_chosen, len_other_vbls)
  
}


get_fc_weights_one_h <- function(mat_cv_rmses_from_x, vec_cv_rmse_from_rgdp, pos, h_max = 8) {
  
  this_mat <- mat_cv_rmses_from_x
  this_vec <- vec_cv_rmse_from_rgdp
  
  this_mat <- this_mat %>% 
    filter(variable != "rgdp" )
  
  this_mat_num <- this_mat[, 1:h_max]
  this_vec_num <- this_vec[1, 1:h_max]
  
  # this_mat_num[3,] <= this_vec_num
  
  is_as_good_as_rgdp <- t(apply(this_mat_num, 1 , function(x) x <= this_vec_num))
  
  mean_val <- rowMeans(this_mat_num)
  
  one_vec <- cbind(this_mat[, pos] , is_as_good_as_rgdp[ ,pos], mean_val, this_mat[,c("variable", "lag")] ) 
  
  vars_and_names_no_rgdp <- this_mat[,c("variable", "lag")] %>% 
    filter(variable != "rgdp" )
  
  names(one_vec)[1] <- "this_h" 
  names(one_vec)[2] <- "is_as_good_as_rgdp"
  
  one_vec_filtered <- one_vec %>% arrange(variable, lag) %>% 
    group_by(variable) %>% 
    mutate(group_min = min(mean_val)) %>% 
    ungroup() %>% 
    filter(mean_val == group_min) %>% 
    mutate(filtered_rmse = ifelse(is_as_good_as_rgdp, this_h, NA),
           filtered_mse = filtered_rmse^2,
           inv_filtered_mse = 1/filtered_mse,
           sum_inv_filtered_mse = sum(inv_filtered_mse, na.rm = TRUE),
           fc_weight = ifelse(!is.na(filtered_rmse), inv_filtered_mse/ sum_inv_filtered_mse, NA)
    )
  
  names(one_vec_filtered)[1] <- paste0("h_", 1)
  # 
  # cv_all_weights <- map2(cv_all_rmse_each_h, cv_rmse_each_h_rgdp, function(x) x[,] <= cv_rdgp_rmse)
  
  full_var_lag_vec_weights <- left_join(vars_and_names_no_rgdp, one_vec_filtered, by = c("variable", "lag")) %>% 
    mutate(fc_weight = ifelse(is.na(fc_weight), 0, fc_weight)) %>% 
    dplyr::select(variable, lag, fc_weight)
  
  full_vec_weights <- full_var_lag_vec_weights$fc_weight
  
  return(full_var_lag_vec_weights)
  
}


get_rgdp_and_dates <- function(data_path, gdp_name = "rgdp", 
                               date_name = "date") {
  
  data <- read_excel(data_path, sheet = "quarterly") 
  
  data <- data[ !is.na(data[, gdp_name]) , ]
  
  start_year_gdp <- min(data$year)
  start_quarter_gdp <- min(data$quarter)
  
  end_year_gdp <- max(data$year)
  end_quarter_gdp <- max(data$quarter)
  
  gdp_data <- data[, gdp_name]
  
  return(list(gdp_data = gdp_data,
              gdp_start = c(start_year_gdp, start_quarter_gdp),
              gdp_end = c(end_year_gdp, end_quarter_gdp))
  )
  
}




get_weighted_fcs <- function(raw_fcs, mat_cv_rmses_from_x, vec_cv_rmse_from_rgdp) {
  
  n_h <- ncol(raw_fcs)
  
  weighted_fcs <- vector(mode = "numeric", length = n_h)
  
  for (pos in seq.int(1,n_h)) {
    
    this_raw <- raw_fcs[, pos]
    
    this_vec_weight <- get_fc_weights_one_h(mat_cv_rmses_from_x,
                                            vec_cv_rmse_from_rgdp, pos)
    
    this_weigthed_fc <- weighted.mean(this_raw, this_vec_weight$fc_weight)
    
    weighted_fcs[pos] <- this_weigthed_fc
  }
  
  return(weighted_fcs)
  
}



# glue_x_mean <- function(ts1, ts2, freq = 12) {
#   
#   ts1_new_length <- length(ts1) - length(ts2)
#   ts1_new_data <- ts1[1:ts1_new_length]
#   
#   ts12 <- ts( data = c(ts1_new_data, ts2), 
#               frequency = freq,
#               start = stats::start(ts1)
#   )
#   
#   return(ts12)
# }

list_of_rmse_plots <- function(selected_models_tbl) {
  
  theme_set(theme_classic())
  hzs <- unique(selected_models_tbl$horizon)
  list_of_plots <- list_along(hzs)
  
  for (i in hzs) {
    thiscb <- selected_models_tbl %>% 
      filter(horizon == i) %>% 
      group_by(horizon) %>% 
      mutate(short_name = fct_reorder(short_name, rmse))
    
    p <- ggplot(thiscb, 
                aes(x = short_name, y = rmse, col = model_function)) + 
      geom_point() + 
      geom_segment(aes(x = short_name, 
                       xend = short_name, 
                       y = min(rmse), 
                       yend = max(rmse)), 
                   linetype="dashed", 
                   size=0.1) +
      coord_flip() + 
      theme(axis.text.y = element_text(size = 8),
            axis.title.y = element_blank(),
            legend.title = element_blank()) +
      ggtitle(paste0("h = ", i))
    
    list_of_plots[[i]] <- p
  }
  
  return(list_of_plots)
}



logyoy <- function(logfc_ts, log_data_ts) {
  full_log_ts <- ts(c(log_data_ts, logfc_ts), frequency = 4,
                    start = stats::start(log_data_ts))
  
  diff_full_log_ts <- diff(full_log_ts, lag = 4)
  
  diff_log_fc <- window(diff_full_log_ts, start = stats::start(logfc_ts))
  
  return(diff_log_fc)
}

indiv_weigthed_fcs <- function(tbl_of_models_and_rmse, h, extended_x_data_ts,
                               rgdp_ts_in_arima, var_data, max_rank_h = NULL,
                               model_type = NULL, chosen_rmse_h = NULL) {
  
  if (!is.null(model_type)) {
    tbl_of_models_and_rmse <- tbl_of_models_and_rmse %>% 
      filter(model_function == model_type) %>% 
      group_by(rmse_h) %>% 
      mutate(rank_h = rank(rmse)) %>% 
      arrange(rmse_h, rank_h)
  }
  
  if (!is.null(chosen_rmse_h)) {
    tbl_of_models_and_rmse <- tbl_of_models_and_rmse %>% 
      filter(rmse_h == chosen_rmse_h) %>% 
      mutate(rank_h )
  }
  
  if (!is.null(max_rank_h)) {
    tbl_of_models_and_rmse <- tbl_of_models_and_rmse %>% 
      filter(rank_h <= max_rank_h)
  }
  
  
  tibble_fit_and_fcs <- tbl_of_models_and_rmse %>% 
    group_by(rmse_h) %>% 
    mutate(sum_invmse_h = sum(inv_mse),
           model_weight_h = inv_mse/sum_invmse_h,
           horizon = as.numeric(substr(rmse_h, 6, 6)),
           fit = pmap(list(model_function, variables, lags, arima_order, 
                           arima_seasonal),
                      ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                      lags = ..3, order = ..4, seasonal = ..5,
                                      extended_x_data_ts = extended_x_data_ts,
                                      arima_rgdp_ts = rgdp_ts_in_arima)),
           fc_obj = pmap(list(model_function, variables, lags, fit),
                         ~ forecast_VAR_Arima(model_function = ..1, 
                                              variables = ..2, lags = ..3,
                                              fit = ..4, h = h_max, 
                                              mat_x_ext = extended_x_data_ts)
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
  
  return(tibble_fit_and_fcs)
}



make_model_name <- function(variables, lags, model_function = NULL) {
  
  colap_variables <- paste(variables, collapse = "_")
  # print(colap_variables)
  
  if (is.null(model_function)) {
    short_name <- paste(colap_variables, lags, sep = "_")
    short_name <- str_remove(short_name, "rgdp_")
    model_name <- short_name
  } else {
    long_name <- paste(model_function, colap_variables, lags, sep = "_")
    long_name <- str_remove(long_name, "rgdp_")
    model_name <- long_name
  }
  return(model_name)
}


make_models_tbl_old <- function(arima_res, var_models_and_rmse, VAR_data, h_max,
                            ave_rmse_sel = FALSE) {
  
  rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
  rmse_level_sarimax <- arima_res$compare_rmse
  v_lags_order_season <- arima_res$var_lag_order_season 
  extended_x_data_ts <- arima_res$mdata_ext_ts
  rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
  
  
  rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
    left_join(v_lags_order_season, by = c("variable", "lag"))
  
  # cfa110 <- comb_fcs_all[1:10, ] %>% 
  #   mutate(short_name = map2(variables, lags,
  #                            ~ make_model_name(variables = .x, lags = .y)),
  #          long_name = pmap(list(variables, lags, model_function), 
  #                           ~ make_model_name(variables = ..1, lags = ..2, model_function = ..3))
  #   )
 
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



make_models_tblsemiold <- function(arima_res, var_models_and_rmse, VAR_data, h_max,
                            ave_rmse_sel = FALSE) {
  
  rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
  rmse_level_sarimax <- arima_res$compare_rmse
  v_lags_order_season <- arima_res$var_lag_order_season 
  extended_x_data_ts <- arima_res$mdata_ext_ts
  rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
  
  
  rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
    left_join(v_lags_order_season, by = c("variable", "lag"))
  
  # cfa110 <- comb_fcs_all[1:10, ] %>% 
  #   mutate(short_name = map2(variables, lags,
  #                            ~ make_model_name(variables = .x, lags = .y)),
  #          long_name = pmap(list(variables, lags, model_function), 
  #                           ~ make_model_name(variables = ..1, lags = ..2, model_function = ..3))
  #   )
  
  each_h_just_model_and_ave_rmse_var <- models_and_accu %>% 
    mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
    dplyr::select(- starts_with("rank"))
  

  each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
    mutate(model_function = "Arima") %>% 
    dplyr::select(variable, lag, starts_with("yoy"), arima_order, arima_seasonal, 
                  model_function) %>% 
    rename(variables = variable, lags = lag) %>% 
    rename_at(vars(starts_with("yoy_rmse")), funs(sub("yoy_rmse", "rmse", .)))
  
    # rename(rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, 
    #        rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, rmse_5 = yoy_rmse_5, 
    #        rmse_6 = yoy_rmse_6)
    # 
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
  
  # models_rmse_at_each_h_arima <- models_rmse_at_each_h_arima %>%
  #   mutate(short_name = map2(variables, lags,
  #                          ~ make_model_name(variables = .x, lags = .y)),
  #                          long_name = pmap(list(variables, lags, model_function),
  #                          ~ make_model_name(variables = ..1, lags = ..2,
  #                                            model_function = ..3))
  #                          )
  
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



make_models_tbl <- function(arima_res, var_models_and_rmse, VAR_data, h_max,
                            ave_rmse_sel = FALSE) {
  
  # rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
  # rmse_level_sarimax <- arima_res$compare_rmse
  # add an id variable to the rmse of the arimax models
  rmse_yoy_sarimax <- arima_res$compare_rmse_yoy %>% mutate(id = 1:n())
  rmse_level_sarimax <- arima_res$compare_rmse %>% mutate(id = 1:n())
  v_lags_order_season <- arima_res$var_lag_order_season 
  extended_x_data_ts <- arima_res$mdata_ext_ts
  rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
  
  
  rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
    left_join(v_lags_order_season, by = c("variable", "lag"))
  
  # cfa110 <- comb_fcs_all[1:10, ] %>% 
  #   mutate(short_name = map2(variables, lags,
  #                            ~ make_model_name(variables = .x, lags = .y)),
  #          long_name = pmap(list(variables, lags, model_function), 
  #                           ~ make_model_name(variables = ..1, lags = ..2, model_function = ..3))
  #   )
  
  each_h_just_model_and_ave_rmse_var <- models_and_accu %>% 
    mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
    dplyr::select(- starts_with("rank"))
  
  
  each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
    mutate(model_function = "Arima") %>% 
    dplyr::select(variable, lag, id, starts_with("yoy"), arima_order, arima_seasonal, 
                  model_function) %>% 
    rename(variables = variable, lags = lag) %>% 
    rename_at(vars(starts_with("yoy_rmse")), funs(sub("yoy_rmse", "rmse", .)))
  
  # rename(rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, 
  #        rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, rmse_5 = yoy_rmse_5, 
  #        rmse_6 = yoy_rmse_6)
  # 
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
  
  # models_rmse_at_each_h_arima <- models_rmse_at_each_h_arima %>%
  #   mutate(short_name = map2(variables, lags,
  #                          ~ make_model_name(variables = .x, lags = .y)),
  #                          long_name = pmap(list(variables, lags, model_function),
  #                          ~ make_model_name(variables = ..1, lags = ..2,
  #                                            model_function = ..3))
  #                          )
  
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





make_models_tbl_rm <- function(arima_res, var_models_and_rmse, VAR_data, h_max,
                            ave_rmse_sel = FALSE) {
  
  # rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
  # rmse_level_sarimax <- arima_res$compare_rmse
  # add an id variable to the rmse of the arimax models
  rmse_yoy_sarimax <- arima_res$compare_rmse_yoy %>% mutate(id = 1:n())
  rmse_level_sarimax <- arima_res$compare_rmse %>% mutate(id = 1:n())
  v_lags_order_season <- arima_res$var_lag_order_season 
  extended_x_data_ts <- arima_res$mdata_ext_ts
  rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
  
  
  rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
    left_join(v_lags_order_season, by = c("variable", "lag"))
  
  # cfa110 <- comb_fcs_all[1:10, ] %>% 
  #   mutate(short_name = map2(variables, lags,
  #                            ~ make_model_name(variables = .x, lags = .y)),
  #          long_name = pmap(list(variables, lags, model_function), 
  #                           ~ make_model_name(variables = ..1, lags = ..2, model_function = ..3))
  #   )
  
  each_h_just_model_and_ave_rmse_var <- models_and_accu %>% 
    mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
    dplyr::select(- starts_with("rank"))
  
  each_h_just_model_and_ave_rmse_var <- as_tibble(each_h_just_model_and_ave_rmse_var)
  
  
  each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
    mutate(model_function = "Arima") %>% 
    dplyr::select(variable, lag, id, starts_with("yoy"), arima_order, arima_seasonal, 
                  model_function) %>% 
    rename(variables = variable, lags = lag) %>% 
    rename_at(vars(starts_with("yoy_rmse")), funs(sub("yoy_rmse", "rmse", .)))
  
  each_h_just_model_and_ave_rmse_sarimax <- as_tibble(each_h_just_model_and_ave_rmse_sarimax)
  
  each_h_just_model_and_ave_rmse_sarimax <- each_h_just_model_and_ave_rmse_sarimax %>% 
    select(-id)
  
  # rename(rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, 
  #        rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, rmse_5 = yoy_rmse_5, 
  #        rmse_6 = yoy_rmse_6)
  # 
  if (ave_rmse_sel) {
    models_rmse_at_each_h_arima  <- each_h_just_model_and_ave_rmse_sarimax %>% 
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
    models_rmse_at_each_h_arima <- each_h_just_model_and_ave_rmse_sarimax %>% 
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
  
  # models_rmse_at_each_h_arima <- models_rmse_at_each_h_arima %>%
  #   mutate(short_name = map2(variables, lags,
  #                          ~ make_model_name(variables = .x, lags = .y)),
  #                          long_name = pmap(list(variables, lags, model_function),
  #                          ~ make_model_name(variables = ..1, lags = ..2,
  #                                            model_function = ..3))
  #                          )
  
  models_rmse_at_each_h_var <- each_h_just_model_and_ave_rmse_var %>% 
    gather(key = "rmse_h", value = "rmse", starts_with("rmse"))
  
  models_rmse_at_each_h <- rbind(models_rmse_at_each_h_var, 
                                 models_rmse_at_each_h_arima) %>% 
    mutate(inv_mse = 1/rmse^2) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) %>% 
    arrange(rmse_h, rank_h)
  
  
  return(models_rmse_at_each_h)
}




make_monthly_ts <- function(data, 
                            names_to_exclude = c("date", "year", "month")) {
  
  
  min_date <- min(date(data$date)) 

  start_year <- year(min_date)
  start_month <- month(min_date)
  
  all_names <- names(data)
  names_to_keep <- all_names[!str_detect(all_names, names_to_exclude)]
  
  data_ts <- tk_ts(data, frequency = 12, 
                   start = c(start_year, start_month),
                   silent = TRUE)
  
  data_ts <- data_ts[ , names_to_keep]
  
  # print("data_ts")
  # print(data_ts)
  
  if (is.null(dim(data_ts))) {
    dim(data_ts) <- c(length(data_ts), 1)
    colnames(data_ts) <- names_to_keep
  }
  
  
  return(data_ts)
}


make_recommendation <- function(seas, sta, sta_after_seas) {
  
  if (seas == 1 & sta_after_seas == 0) {
    recommendation <- "yoy"
  } 
  
  if (seas == 0 & sta_after_seas == 0) {
    recommendation <- "level"
  } 
  if (seas == 1 & sta_after_seas == 1) {
    recommendation <- "diff_yoy"
  } 
  if (seas == 0 & sta_after_seas == 1) {
    recommendation <- "diff"
  } 
  if (seas == 0 & sta_after_seas == 2) {
    recommendation <- "diff_diff"
  } 
  if (seas == 1 & sta_after_seas == 2) {
    recommendation <- "diff_diff_yoy"
  } 
  
  return(recommendation)
  
}





make_tables_and_plots_vbls_lags_sizes <- function(models_and_accu) {
  
  models_and_accu <- models_and_accu %>% 
    mutate(accu_lev = map(accu_lev, unlist),
           accu_yoy = map(accu_yoy, unlist))
  
  models_and_accu <- models_and_accu %>%  
    mutate(n_variables = map(variables, length))
  
  ma_diff_yoy <- models_and_accu %>% 
    filter(diff_ranking <= 50)
  
  ma_yoy <- models_and_accu %>% 
    filter(yoy_ranking <= 50)
  
  ma_level <- models_and_accu %>% 
    filter(level_ranking <= 50)
  
  vbls_diff <- reduce(ma_diff_yoy$variables, c) 
  count_vbls_diff <- vbls_diff %>% tibble(v = .) %>% 
    group_by(v) %>% summarise(n_diff = n())
  
  vbls_yoy <- reduce(ma_yoy$variables, c)
  count_vbls_yoy <- vbls_yoy %>% tibble(v = .) %>% 
    group_by(v) %>% summarise(n_yoy = n())
  
  vbls_level <- reduce(ma_level$variables, c)
  count_vbls_level <- vbls_level %>% tibble(v = .) %>% 
    group_by(v) %>% summarise(n_level = n())
  
  n_endo_diff <- reduce(ma_diff_yoy$n_variables, c)
  count_n_endo_diff <- n_endo_diff %>% tibble(size = .) %>% 
    group_by(size) %>% summarise(size_diff = n())
  
  n_endo_yoy <- reduce(ma_yoy$n_variables, c)
  count_n_endo_yoy <- n_endo_yoy %>% tibble(size = .) %>% 
    group_by(size) %>% summarise(size_yoy = n())
  
  n_endo_level <- reduce(ma_level$n_variables, c)
  count_n_endo_level <- n_endo_level %>% tibble(size = .) %>% 
    group_by(size) %>% summarise(size_level = n())
  
  lags_diff <- reduce(ma_diff_yoy$lags, c)
  count_lags_diff <- lags_diff %>% tibble(lag = .) %>% 
    group_by(lag) %>% summarise(lag_diff = n())
  
  lags_yoy <- reduce(ma_yoy$lags, c)
  count_lags_yoy <- lags_yoy %>% tibble(lag = .) %>% 
    group_by(lag) %>% summarise(lag_yoy = n())
  
  lags_level <- reduce(ma_level$lags, c)
  count_lags_level <- lags_level %>% tibble(lag = .) %>% 
    group_by(lag) %>% summarise(lag_level = n())
  
  
  vbls <- reduce(list(count_vbls_diff, count_vbls_yoy, count_vbls_level), 
                 full_join, by = "v")  %>% 
    gather(key = "group", value = "n", -v) %>%
    mutate(group = factor(group, levels = c( "n_level" , "n_yoy", "n_diff"))) %>% 
    arrange(group, desc(n)) %>% 
    mutate(v_order = row_number())
  
  g_vbls_facets <- ggplot(vbls, aes(x = v_order, y = n, fill = group)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    facet_wrap(~ group, scales = "free") + 
    ylab("Number of appearances in selected VARs") +
    xlab("variable") + 
    scale_x_continuous(
      breaks = vbls$v_order,
      labels = vbls$v,
      expand = c(0,0)
    ) +
    coord_flip()
  
  g_vbls_stacked <- ggplot(data = vbls, aes(x = fct_reorder2(v, group, n), 
                                            weight = n, fill = group)) + 
    geom_bar()  + coord_flip()
  
  
  
  endo <- reduce(list(count_n_endo_diff, count_n_endo_yoy, count_n_endo_level), 
                 full_join, by = "size") %>% 
    gather(key = "group", value = "freq", -size) %>%
    mutate(group = factor(group, levels = c( "size_level" , "size_yoy", "size_diff"))) %>% 
    arrange(group, desc(freq)) %>% 
    mutate(size_order = row_number())
  
  g_endo_facets <- ggplot(endo, aes(x = size_order, y = freq, fill = group)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    facet_wrap(~ group) + 
    ylab("Number of appearances in selected VARs") +
    xlab("size") + 
    scale_x_continuous(
      breaks = endo$size_order,
      labels = endo$size
    ) +
    coord_flip()
  
  g_endo_stacked <- ggplot(data = endo, aes(x = size, weight = freq, fill = group)) + 
    geom_bar(position = "dodge") 
  
  
  n_lags <- reduce(list(count_lags_diff, count_lags_yoy, count_lags_level), 
                   full_join, by = "lag") %>% 
    gather(key = "group", value = "freq", -lag) %>%
    mutate(group = factor(group, levels = c( "lag_level" , "lag_yoy", "lag_diff"))) %>% 
    arrange(group, desc(freq)) %>% 
    mutate(lag_order = row_number())
  
  g_lag_facets <- ggplot(n_lags, aes(x = lag_order, y = freq, fill = group)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    facet_wrap(~ group, scales = "free_y") + 
    ylab("Number of appearances in selected VARs") +
    xlab("lags") + 
    scale_x_continuous(
      breaks = n_lags$lag_order,
      labels = n_lags$lag
    ) +
    coord_flip()
  
  g_lag_stacked <- ggplot(data = n_lags, aes(x = lag, weight = freq, fill = group)) + 
    geom_bar(position = "dodge") 
  
  
  
  
  variables_table <- reduce(list(count_vbls_diff, count_vbls_yoy, count_vbls_level), 
                            full_join, by = "v") %>% 
    rename(variable = v,
           fcs_diff_yoy = n_diff,
           fcs_yoy = n_yoy,
           fcs_level = n_level) %>% 
    mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
           fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
           fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
    ) %>% 
    arrange(desc(fcs_diff_yoy))
  
  lags_table <- reduce(list(count_lags_diff, count_lags_yoy, count_lags_level), 
                       full_join, by = "lag") %>% 
    rename(fcs_diff_yoy = lag_diff,
           fcs_yoy = lag_yoy,
           fcs_level = lag_level) %>% 
    mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
           fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
           fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
    ) %>% 
    arrange(desc(fcs_diff_yoy))
  
  sizes_table <- reduce(list(count_n_endo_diff, count_n_endo_yoy, count_n_endo_level), 
                        full_join, by = "size") %>% 
    rename(fcs_diff_yoy = size_diff,
           fcs_yoy = size_yoy,
           fcs_level = size_level) %>% 
    mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
           fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
           fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
    ) %>% 
    arrange(desc(fcs_diff_yoy))
  
  
  
  g_diff_vs_yoy_best_diffs <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 50),
                                     aes(x = accu_diff_yoy, y = unlist(accu_yoy) )) + geom_point()
  
  g_diff_vs_lev_best_diffs <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 50),
                                     aes(x = accu_diff_yoy, y = unlist(accu_lev) )) + geom_point()
  
  g_diff_vs_yoy_best_yoys <- ggplot(data =  models_and_accu %>% filter(yoy_ranking <= 50),
                                    aes(x = unlist(accu_yoy), y = accu_diff_yoy)) + geom_point()
  
  g_lev_vs_yoy_best_lev <- ggplot(data =  models_and_accu %>% filter(level_ranking <= 50),
                                  aes(x = unlist(accu_lev), y = accu_diff_yoy)) + geom_point()
  
  g_best_diff_accu <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 50),
                             aes(x = accu_diff_yoy)) + geom_histogram(bins = 7)
  
  g_best_yoy_accu <- ggplot(data =  models_and_accu %>% filter(yoy_ranking <= 50),
                            aes(x = unlist(accu_yoy) )) + geom_histogram(bins = 7)
  
  g_best_lev_accu <- ggplot(data =  models_and_accu %>% filter(level_ranking <= 50),
                            aes(x = unlist(accu_lev) )) + geom_histogram(bins = 7)
  
  return(list(
    variables_table = variables_table, lags_table = lags_table, sizes_table = sizes_table,
    vbls_plot_facet = g_vbls_facets, lags_plot_facets = g_lag_facets, sizes_plot_facets = g_endo_facets, 
    vbls_plot_stacked = g_vbls_stacked, lags_plot_stacked = g_lag_stacked, sizes_plot_stacked = g_endo_stacked,
    accu_best_diffs_and_their_yoy = g_diff_vs_yoy_best_diffs, 
    accu_best_diffs_and_their_level = g_diff_vs_lev_best_diffs
  )
  )
  
}

make_test_dates_list <- function(ts_data, type = "tscv", n = 8, h_max = 6,
                                 timetk_idx = TRUE, training_length = 20,
                                 external_idx = NULL) {
  
  data_length <- nrow(ts_data)
  
  # if (timetk_idx) {
  #   date_time_index <- tk_index(ts_data, timetk_idx = timetk_idx)
  # } else {
  #   date_time_index <- external_idx
  # }
  # 
  date_time_index <- as.yearqtr(time(ts_data))
  
  list_of_positions <- list_along(seq(1:n))
  list_of_dates <- list_along(seq(1:n))
  list_of_year_quarter <- list_along(seq(1:n))
  
  if (type == "tscv") {
    
    for (i in seq.int(1:n)) {
      
      from_the_right <-  i - 1
      
      end_test_pos <- data_length - from_the_right 
      start_test_pos <- end_test_pos - h_max + 1
      end_training_pos <- start_test_pos - 1
      start_training_pos <- end_training_pos - training_length + 1
      
      
      end_test_date <- date_time_index[end_test_pos]
      start_test_date <- date_time_index[start_test_pos] 
      end_training_date <- date_time_index[end_training_pos]
      start_training_date <- date_time_index[start_training_pos]
      
      end_test_year <- year(end_test_date)
      start_test_year <- year(start_test_date) 
      end_training_year <- year(end_training_date) 
      start_training_year <- year(start_training_date)
      
      end_test_quarter <- quarter(end_test_date)
      start_test_quarter <- quarter(start_test_date) 
      end_training_quarter <- quarter(end_training_date) 
      start_training_quarter <- quarter(start_training_date)
      
      this_pos <- list(
        tra_s = start_training_pos, 
        tra_e = end_training_pos,
        tes_s = start_test_pos, 
        tes_e = end_test_pos)
      
      this_date <- list(
        tra_s = start_training_date, 
        tra_e = end_training_date,
        tes_s = start_test_date, 
        tes_e = end_test_date)
      
      this_yq <- list(
        tra_s = c(start_training_year, start_training_quarter),
        tra_e = c(end_training_year, end_training_quarter),
        tes_s = c(start_test_year, start_test_quarter),
        tes_e = c(end_test_year, end_test_quarter)
      )
      
      list_of_positions[[i]] <- this_pos
      list_of_dates[[i]] <- this_date
      list_of_year_quarter[[i]] <- this_yq
      
    }
    
    return(list(
      list_of_year_quarter = list_of_year_quarter,
      list_of_dates = list_of_dates,
      list_of_positions = list_of_positions)
    )
    
  }

}


make_xreg_fc <- function(variable_name, mx_ext, lags, start_fc, h) {
  this_x <- mx_ext[ , variable_name]
  x_lagmat <- c()
  x_lagmat <- this_x
  
  if(lags > 0) {
    for (i in 1:lags) {
      this_lag <- i
      x_lag <- lag.xts(this_x, k = this_lag)
      x_lagmat <- ts.union(x_lagmat, x_lag)
    }
  }
  
  x_lagmat_rest <- window(x_lagmat, start = start_fc)
  x_lagmat_for_fc <- subset(x_lagmat_rest, end = h)
  
  return(x_lagmat_for_fc)
}


make_yoy_xts <- function(df_xts, freq = 4) {
  new_xts <- diff.xts(df_xts, lag = freq, na.pad = FALSE)/lag.xts(
    df_xts, na.pad = FALSE, k = freq)
}


make_yoy_ts <- function(df_ts, freq = 4, is_log = FALSE) {
  
  if (is_log) {
    df_ts <- exp(df_ts)
  }
  
  new_ts <- base::diff(df_ts, lag = freq)/stats::lag(df_ts, k = -freq)
  
  return(new_ts)
}


my_arima_one_x <- function(y_ts, y_order, y_seasonal, xreg_lags, x_name,
                           xreg_data = NULL) {

  y_ts <- na.omit(y_ts)

  if (x_name == "rgdp") {
    this_arimax <- Arima(y = y_ts, order = y_order, 
                         seasonal = y_seasonal)
    return(this_arimax)
  }
  x_series <-  xreg_data[ , x_name]
  
  # n <- length(y_ts)
  
  y_time <- time(y_ts)
  x_time <- time(x_series)
  
  if (min(x_time) >= min(y_time)) {
    latest_start <- stats::start(x_series)
  } else {
    latest_start <- stats::start(y_ts)
  }
  
  if (max(x_time) <= max(y_time)) {
    earliest_end <- stats::end(x_series)
  } else {
    earliest_end <- stats::end(y_ts)
  }
  
  y_ts <- ts(y_ts, start = latest_start, end = earliest_end, frequency = 4)
  
  
  x_as_y <- window(x_series, start = stats::start(y_ts),
                   end = stats::end(y_ts),
                   frequency = 4)
  
  max_xreg_lag <- max(xreg_lags)
  xlagmat <- c()
  
  for (i in 0:max_xreg_lag) {
    xlagmat <- cbind(xlagmat, lag.xts(x_as_y, k = i))
  }
  
  # colnames(xlagmat) <- paste0("xlag_", 0:max_xreg_lag)
  x_as_y <- xlagmat
  
  # this_arimax <- Arima(y = y_ts, xreg = x_as_y, 
  #                      order = y_order, seasonal = y_seasonal,
  #                      method = "ML")
  
  # print("comienzo")
  this_arimax <- try(Arima(y = y_ts, xreg = x_as_y, 
                           order = y_order, seasonal = y_seasonal,
                           method = "ML"))
  
  class_this_arimax <- class(this_arimax)[1]
  
  if (class_this_arimax == "try-error") {
    this_mssg <- paste0("For xreg variable ", x_name, 
                        ", ML method failed in Arima. Switched to CSS-ML.")
    warning(this_mssg)
    new_method <-  "CSS-ML"
    this_arimax <- Arima(y = y_ts, xreg = x_as_y, 
                         order = y_order, seasonal = y_seasonal,
                         method = new_method)
  }
  
  # print("final")
  
  return(this_arimax)
  
  
}


my_arimax <- function(y_ts, xreg_ts, y_order, y_seasonal,
                      y_include_mean = TRUE, 
                      vec_of_names = NULL, s4xreg = FALSE,
                      xreg_lags = NULL,
                      method = "ML", force.constant = FALSE) {
  
  i = 1
  
  # print("in myarimax yts")
  # print(y_ts)
  # 
  # 
  # print("in myarimax xregts")
  # print(xreg_ts)
  # 
  
  y_ts <- na.omit(y_ts)
  
  y_time <- time(y_ts)
  
  n <- length(y_ts)
  
  x_names <- colnames(xreg_ts)
  
  this_d <- y_order[2]
  this_D <- y_seasonal[2]
  
  # print("this_d")
  # print(this_d)
  # print("this_D")
  # print(this_D)
  # print("force.constant")
  # print(force.constant)

  number_of_xregs <- ncol(as.matrix(xreg_ts))
  
  arimax_list <-  list()
  
  
  # print("x_names")
  # print(x_names)
  # print("number_of_xregs")
  # print(number_of_xregs)
  
  for (x_regressor in 1:number_of_xregs) {
    
    print("vec_of_names[x_regressor]")
    print(vec_of_names[x_regressor])
    
    
    if (is.null(ncol(xreg_ts))) {
      x_series <-  na.omit(xreg_ts)
    } else {
      x_series <-  na.omit(xreg_ts[ , x_regressor])
    }
    
    # print("colnames(x_series)")
    # print(colnames(x_series))
    
    x_series_and_lags <-  map(seq(0,max(xreg_lags)),
                              ~ lag.xts(x_series, k = .)) %>% 
      reduce(ts.union) %>% na.omit()
    
    # x_time <- time(x_series)
    x_time <- time(x_series_and_lags)
    
    # print("time(x_series_and_lags)")
    # print(time(x_series_and_lags))
    
    if (min(x_time) > min(y_time)) {
      latest_start <- stats::start(x_series)
    } else {
      latest_start <- stats::start(y_ts)
    }
    
    if (max(x_time) < max(y_time)) {
      earliest_end <- stats::end(x_series)
    } else {
      earliest_end <- stats::end(y_ts)
    }
    
    
    # print("latest_start")
    # print(latest_start)
    # 
    # print("earliest_end")
    # print(earliest_end)
    # 
    # print("pre-window")
    # print("y_ts")
    # print(y_ts)
    # print("x_series")
    # print(x_series)
    

    
    procrustean_y <- window(y_ts, start = latest_start, end = earliest_end, frequency = 4)
    
    
    procrustean_x <- window(x_series, start = latest_start, end = earliest_end,
                     frequency = 4)
    
    
    if (is.null(dim(procrustean_x))) {
          dim(procrustean_x) <- c(length(procrustean_x), 1)
        }
      
    # print("post-window")
    # print("procrustean_y")
    # print(procrustean_y)
    # print("procrustean_x")
    # print(procrustean_x)
    
    n_x <- length(procrustean_x)
    
    # print("n_x")
    # print(n_x)

    procrustean_x_and_lags <- map(seq(0,max(xreg_lags)),
                                  ~ lag.xts(procrustean_x, k = .)) %>% 
      reduce(ts.union)
    
    colnames(procrustean_x_and_lags) <- paste(x_names[x_regressor],
                                              0:max(xreg_lags), sep = "_")
    
    if (any(is.na(procrustean_x_and_lags))) {
      print(paste("Warning, xreg series",   vec_of_names[x_regressor],
          "is too short and lags introduced NAs"))
      
      
      
      procrustean_x_and_lags <- na.omit(procrustean_x_and_lags)
      procrustean_y <- window(procrustean_y, start = stats::start(procrustean_x_and_lags),
                              end = stats::end(procrustean_x_and_lags))
      
      print(paste("New number of rows:", nrow(procrustean_y), "and",
                  nrow(procrustean_x_and_lags)))
      print(paste("New start date:", as.yearqtr(stats::start(procrustean_y))))
      print(paste("New end date:", as.yearqtr(stats::end(procrustean_y))))
    }

    
    
    if ( force.constant & (this_d + this_D) >= 2) {
      
      print("Adding a deterministic quadractic trend")
      
      xtrend <- seq(1, length(procrustean_y))^2
      
      nam <- colnames(procrustean_x_and_lags)
      
      procrustean_x_and_lags <- ts.union(xtrend, procrustean_x_and_lags)
      
      colnames(procrustean_x_and_lags) <- c("sq_trend", nam)
    }
    

    this_arimax <- try(Arima(y = procrustean_y, xreg = procrustean_x_and_lags,
                             order = y_order,
                             seasonal = y_seasonal,
                             include.mean = y_include_mean,
                             method = method))
    class_this_arimax <- class(this_arimax)[1]
    if (class_this_arimax == "try-error") {
      this_mssg <- paste0("For xreg variable ", vec_of_names[x_regressor], 
                          ", ML method failed in Arima. Switched to CSS-ML.")
      warning(this_mssg)
      
      print("fooooo")
      new_method <-  "CSS-ML"
      this_arimax <- try(Arima(y = procrustean_y, xreg = procrustean_x_and_lags,
                           order = y_order,
                           seasonal = y_seasonal,
                           include.mean = y_include_mean,
                           method = new_method))
      
      class_this_arimax <- class(this_arimax)[1]
      print("classthisarima")
      print(class_this_arimax)
      if (class_this_arimax == "try-error") {
        print("mooooo")
        print("procrustean_y")
        print(procrustean_y)
        
        print("procrustean_x_and_lags")
        print(procrustean_x_and_lags)
        
        
        this_mssg <- paste0("For xreg variable ", vec_of_names[x_regressor], 
                            ", CSS-ML method failed in Arima. Switched to CSS.")
        warning(this_mssg)
        new_method <-  "CSS"
        this_arimax <- Arima(y = procrustean_y, xreg = procrustean_x_and_lags,
                             order = y_order,
                             seasonal = y_seasonal,
                             include.mean = y_include_mean,
                             method = new_method)
      }
      
    }
    
    
    arimax_list[[x_regressor]] <- this_arimax
    
  }
  
              
  names(arimax_list) <- vec_of_names
  
  return(arimax_list)
  
  
}


my_diff <- function(series, lag = 1, differences = 1) {
  if (differences == 0) {
    x_diff <- series
  } else {
    x_diff <- diff(series, lag = lag, differences = differences)
  }
  return(x_diff)
}



read_gather_qm_data <- function(data_path = "./data/pre_r_data/", 
                                country = NULL) {
  
  file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
  file_names 
  
  file_paths <- paste0(data_path, file_names)
  file_paths
  
  country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")
  
  names(file_paths) <- country_names
  names(file_names) <- country_names
  
  if (!is.null(country)) {
    file_paths <- file_paths[country]
    file_names <- file_names[country]
    country_names <- country
  }
  
  
  all_files_q <- list_along(country_names)
  all_files_m <- list_along(country_names)
  all_files_m_q <- list_along(country_names)
  countries_merged_q_m <- list_along(country_names)
  
  
  
  for (i in seq_along(country_names)) {
    
    this_q <- read_excel(file_paths[i], sheet = "quarterly")
    this_q <- as_tbl_time(this_q, index = date)
    this_q <- dplyr::select(this_q, -c(year, hlookup))
    
    if(country_names[i] == "Uruguay") {
      this_q[, "rm"] <- - this_q[, "rm"]
    }
    
    
    all_files_q[[i]] <- this_q
    
    this_m <- read_excel(file_paths[i], sheet = "monthly")
    this_m <- as_tbl_time(this_m, index = date)
    all_files_m[[i]] <- this_m
    
    this_m_q <- this_m  %>%
      collapse_by(period = "quarterly") %>%
      group_by(date) %>% transmute_all(mean) %>%
      distinct(date, .keep_all = TRUE) %>% 
      ungroup() 
    
    all_files_m_q[[i]] <- this_m_q
    
    countries_merged_q_m[[i]] <- left_join(this_q, this_m_q, by = "date")
    
  }
  
  names(all_files_q) <- country_names
  names(all_files_m) <- country_names
  names(all_files_m_q) <- country_names
  names(countries_merged_q_m) <- country_names
  
  return(list(countries_q = all_files_q, 
              countries_m = all_files_m, 
              countries_q_former_m = all_files_m_q,
              countries_merged_q_m = countries_merged_q_m))
  
}



single_plot_rmse_all_h <- function(selected_models_tbl) {
  
  rmse_table_single_h <- selected_models_tbl %>% 
    select(variables, lags, model_function, rmse_h, rmse, horizon) %>%
    arrange(rmse_h, model_function, rmse) %>% 
    mutate(idx = 1:n())
  
  max_rmse <- max(rmse_table_single_h$rmse)
  
  p <- ggplot(rmse_table_single_h, aes(x = idx, y = rmse)) + 
    geom_point(aes(color = model_function),
               size = 2.2, alpha = 0.8) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    geom_vline(xintercept =  c(1,31, 61, 91, 121, 151, 181, 211, 241), alpha = 0.3, 
               linetype = "dashed") +
    annotate("text", x = 0 + 15, y = 1.1*max_rmse, label = "h = 1", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = 30 + 15, y = 1.1*max_rmse, label = "h = 2", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = 60 + 15, y = 1.1*max_rmse, label = "h = 3", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = 90 + 15, y = 1.1*max_rmse, label = "h = 4", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = 120 + 15, y = 1.1*max_rmse, label = "h = 5", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = 150 + 15, y = 1.1*max_rmse, label = "h = 6", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = 180 + 15, y = 1.1*max_rmse, label = "h = 7", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = 210 + 15, y = 1.1*max_rmse, label = "h = 8", fontface = "bold", colour = "royalblue4") +
    theme_tufte() + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank()) +
    theme(axis.title = element_text(face = "bold"))
  
  # p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
  
  return(p)
}


single_plot_rmse_all_h_oldversion <- function(selected_models_tbl) {
  
  rmse_table_single_h <- selected_models_tbl %>% 
    select(variables, lags, model_function, rmse_h, rmse, horizon) %>%
    arrange(rmse_h, model_function, rmse) %>% 
    mutate(idx = 1:n())
  
  max_rmse <- max(rmse_table_single_h$rmse)
  
  p <- ggplot(rmse_table_single_h, aes(x = idx, y = rmse)) + 
    geom_point(aes(color = model_function),
               size = 2.2, alpha = 0.8) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    geom_vline(xintercept =  c(1,31, 61, 91, 121, 151), alpha = 0.3, 
               linetype = "dashed") +
    annotate("text", x = 0 + 15, y = 1.1*max_rmse, label = "h = 1") +
    annotate("text", x = 30 + 15, y = 1.1*max_rmse, label = "h = 2") +
    annotate("text", x = 60 + 15, y = 1.1*max_rmse, label = "h = 3") +
    annotate("text", x = 90 + 15, y = 1.1*max_rmse, label = "h = 4") +
    annotate("text", x = 120 + 15, y = 1.1*max_rmse, label = "h = 5") +
    annotate("text", x = 150 + 15, y = 1.1*max_rmse, label = "h = 6") +
    theme_tufte() + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())
  
  # p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
  
  return(p)
}



to_ts_q <- function(df_xts){
  
  yq_start <- first(index(df_xts))
  yq_end <- last(index(df_xts))
  
  start_year <- year(yq_start)
  start_quarter <- quarter(yq_start)
  this_start = c(start_year, start_quarter)
  
  end_year <- year(yq_end)
  end_quarter <- quarter(yq_end)
  this_end = c(end_year, end_quarter)
  
  this_ts <- tk_ts(df_xts, start = this_start,
                   frequency = 4, silent = TRUE)
  return(this_ts)
}


transform_cv <- function(list_series, series_name, current_form,
                         auxiliary_ts) {
  
  current_form <- current_form
  
  series_name <- series_name
  
  if (current_form == "diff_yoy") {
    len_initial_cond <- 1
  }
  
  new_series_list <- list_along(1:number_of_cv)
  
  
  for (td in seq_along(1:number_of_cv)) {
    
    this_test_data <- list_series[[td]]
    test_time <- time(this_test_data)
    start_test <- min(test_time)
    end_initial_cond <- start_test - 0.25
    start_initial_cond <- start_test - 0.25*len_initial_cond
    end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                              quarter(as.yearqtr(end_initial_cond))
    )
    start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                quarter(as.yearqtr(start_initial_cond))
    )
    initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                              end = end_initial_cond_y_q)
    
    if (current_form == "diff_yoy") {
      new_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
    }
    
    
    new_series_list[[td]] <- new_test_data
    
  }
  
  return(new_series_list)
  
}


ts_data_for_arima <- function(data_path, external_data_path, all_logs = FALSE) {
  
  
  gdp_and_dates <- get_rgdp_and_dates(data_path)
  
  
  monthly_data <- get_monthly_variables(data_path = data_path)
  monthly_ts <- make_monthly_ts(monthly_data)
  monthly_names <- colnames(monthly_ts)
  
  
  external_monthly_data <- get_monthly_variables(data_path = external_data_path)
  external_monthly_ts <- make_monthly_ts(external_monthly_data)
  external_monthly_names <- colnames(external_monthly_ts)
  
  
  rgdp_ts <- ts(data = gdp_and_dates[["gdp_data"]], 
                start = gdp_and_dates[["gdp_start"]], frequency = 4)
  
  if (all_logs) {
    monthly_ts  <- log(monthly_ts)
    external_monthly_ts  <- log(external_monthly_ts)
    rgdp_ts <- log(rgdp_ts)
  }
  
  ret_list <- list(
    monthly_ts = monthly_ts, external_monthly_ts = external_monthly_ts,
    rgdp_ts = rgdp_ts)
}



try_sizes_vbls_lags <- function(var_data, rgdp_yoy_ts, rgdp_level_ts, target_v, vec_size = c(3,4,5), 
                                vec_lags = c(1,2,3,4), pre_selected_v = "",
                                is_cv = FALSE, h_max = 5, n_cv = 8,
                                training_length = 16, maxlag_ccm = 8,
                                bt_factor = 1.4, return_cv = TRUE,
                                max_rank = 30,
                                rgdp_current_form = "yoy") {
  
  # print("in try_sizes_vbls_lags, has_timetk_idx(var_data)")
  # print(has_timetk_idx(var_data))
  
  len_size <-  length(vec_size)
  len_lag <- length(vec_lags)
  
  all_names <- colnames(var_data)
  
  # i, outer most loop: var size (number of edogenous variables), e.g. 3, then 4, then 5 variables
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags
  
  # I considered including a loop between i and j, loopig through several
  # choices of fixed or preselected variables but I think that makes the code less intuitive and 
  # is not a frequently used feature, so I discarded it. 
  
  results_all_models <- list_along(seq.int(1, len_size))
  fcs_var_all_sizes <- list_along(seq.int(1, len_size))
  
  var_fixed_size_fixed_vset_all_lags <- list_along(seq.int(1, len_lag))
  fcs_fixed_size_fixed_vset_all_lags <- list_along(seq.int(1, len_lag))
  
  model_number <- 0
  
  for (i in seq.int(1, len_size)) {
    this_size <- vec_size[i]
    
    already_chosen <- c(target_v, pre_selected_v)
    already_chosen <- already_chosen[already_chosen != ""]
    len_already_chosen <- length(already_chosen)
    len_other_vbls <- this_size - len_already_chosen
    
    
    sets_of_other_variables <- get_sets_of_variables(
      df = var_data, this_size = this_size, all_variables = all_names, 
      already_chosen = already_chosen, bt_factor = bt_factor,
      maxlag_ccm = maxlag_ccm)
    
    # print(class("sets_of_other_variables"))
    # print(class(sets_of_other_variables))
    # 
    # print("sets_of_other_variables")
    # print(sets_of_other_variables)
    
    
    
    # 
    #     if (this_size == 3) {
    #       sets_of_other_variables <- list(c("tot"), c("imp"), c("exp"))
    #     }
    #     
    #     if (this_size == 4) {
    #       sets_of_other_variables <- list(c("tot", "ip"), c("imp", "m1"))
    #     }
    
    # len_sets_of_vars <- length(sets_of_other_variables)
    len_sets_of_vars <- ncol(sets_of_other_variables)
    
    var_fixed_size_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
    
    for (j in seq.int(1, len_sets_of_vars)) {
      
      # vec_of_other_vbls <- sets_of_other_variables[[j]]
      vec_of_other_vbls <- sets_of_other_variables[,j]
      vbls_for_var <- c(already_chosen, vec_of_other_vbls)
      
      for (k in seq.int(1, len_lag)) {
        
        model_number <- model_number + 1
        this_lag <- vec_lags[k]
        
        sub_data = var_data[, vbls_for_var]
        
        sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
        
        this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                          external_idx = sub_data_tk_index, this_p = this_lag,
                          this_type = "const", h_max = h_max,
                          n_cv = n_cv, training_length = training_length)
        
        var_fixed_size_fixed_vset_all_lags[[k]] <- this_cv
        
      }
      
      est_var_this_vset <- var_fixed_size_fixed_vset_all_lags
      var_fixed_size_all_vset_all_lags[[j]] <- est_var_this_vset
      
    }
    
    est_var_this_size <- var_fixed_size_all_vset_all_lags
    results_all_models[[i]] <- est_var_this_size 
    
  }
  
  results_all_models <- flatten(flatten(results_all_models))
  column_names <- names(results_all_models[[1]])

  # transitory names to allow conversion to tibble (columns must be names)
  names(results_all_models) <- seq_along(results_all_models)

  # transpose tibble, ensure result is still a tibble
  results_all_models <- as_tibble(t(as_tibble(results_all_models)))
  names(results_all_models) <- column_names
  
  
  if (rgdp_current_form != "yoy") {
    if (rgdp_current_form == "diff_yoy") {
      
      auxiliary_ts <-  rgdp_yoy_ts
      results_all_models <- results_all_models %>% 
        rename(cv_test_data_diff_yoy = cv_test_data,
               cv_fcs_diff_yoy = cv_fcs)
      
      # print(cv_objects[["cv_test_data_diff_yoy"]])
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff_yoy, ~ transform_cv(list_series  = ., 
                                                series_name = "cv_test_data",
                                                current_form = rgdp_rec,
                                                auxiliary_ts = auxiliary_ts) ),
          cv_fcs = map(
            cv_fcs_diff_yoy,  ~ transform_cv(list_series  = .,
                                             series_name = "cv_fcs",
                                             current_form = rgdp_rec,
                                             auxiliary_ts = auxiliary_ts) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
    }
    
    if (rgdp_current_form == "diff") {
      auxiliary_ts <-  rgdp_level_ts
    }
    
    
    
  }
  
  
  

  results_all_models <- get_rmses_h_rakings_h(data = results_all_models,
                                              h_max = h_max)
  
  results_all_models <- results_all_models %>% 
    filter_at( vars(starts_with("rank")), any_vars(. <= max_rank)) %>% 
    mutate(cv_vbl_names = map(cv_vbl_names, 1),
           cv_lag = map(cv_lag, 1))
  

  print(paste("Tried", len_lag, "different choices of lags per each combination"))
  print(paste("Number of models analyzed:", model_number))
  print(paste("CV repetitions:", number_of_cv))
  print(paste("Total estimations and fcs:", number_of_cv*model_number))
  
  cv_objects <- results_all_models %>% dplyr::select(cv_vbl_names, cv_lag, cv_errors, cv_test_data,
                                                     cv_fcs) %>% 
    rename(variables = cv_vbl_names, lags = cv_lag)
  

  accu_rankings_models <- results_all_models %>% 
    dplyr::select(cv_vbl_names, cv_lag, 
                  starts_with("rmse"), starts_with("rank")) %>% 
    rename(variables = cv_vbl_names, lags = cv_lag)
  
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects))
  } else {
    return(list(accu_rankings_models = accu_rankings_models))
    
  }
  
}


un_yoy <- function(init_lev, vec_yoy) {
  
  n_init <- length(init_lev)
  n_yoy <- length(vec_yoy)
  y_vec <- vector(mode = "numeric", length = n_init + n_yoy)
  y_vec[1:n_init] <- init_lev
  
  for (i in seq_along(vec_yoy)) {
    
    this_y <- vec_yoy[i] + y_vec[ i ]
    
    y_vec[n_init + i] <- this_y
  }
  
  un_yoy_vec <- y_vec[(n_init + 1):(n_init + n_yoy)]
  
  return(un_yoy_vec)
}

un_yoy_ts <- function(init_lev, vec_yoy, freq = 4) {
  
  n_init <- length(init_lev)
  n_yoy <- length(vec_yoy)
  y_vec <- vector(mode = "numeric", length = n_init + n_yoy)
  y_vec[1:n_init] <- init_lev
  
  for (i in seq_along(vec_yoy)) {
    
    this_y <- vec_yoy[i] + y_vec[ i ]
    
    y_vec[n_init + i] <- this_y
  }
  
  un_yoy_vec <- y_vec[(n_init + 1):(n_init + n_yoy)]
  
  this_year <- as.integer(floor(time(vec_yoy)))
  
  # print("time(vec_yoy) in un_yoy_ts")
  # print(time(vec_yoy))
  
  
  # print("this_year in un_yoy_ts")
  # print(this_year)
  
  this_quarter <- as.integer(4 * (time(vec_yoy) - this_year + 0.25))
  
  # print("this_quarter in un_yoy_ts")
  # print(this_quarter)
  
  this_start <- c(first(this_year), first(this_quarter))
  # print("this_start in un_yoy_ts")
  # print(this_start)
  
  un_yoy_ts <- ts(un_yoy_vec, start = this_start, frequency = 4)
  return(un_yoy_ts)
}

un_yoy_ts <- function(init_lev, vec_yoy, freq = 4) {
  
  n_init <- length(init_lev)
  n_yoy <- length(vec_yoy)
  y_vec <- vector(mode = "numeric", length = n_init + n_yoy)
  y_vec[1:n_init] <- init_lev
  
  for (i in seq_along(vec_yoy)) {
    
    this_y <- vec_yoy[i] + y_vec[ i ]
    
    y_vec[n_init + i] <- this_y
  }
  
  un_yoy_vec <- y_vec[(n_init + 1):(n_init + n_yoy)]
  
  this_year <- as.integer(floor(time(vec_yoy)))
  
  # print("time(vec_yoy) in un_yoy_ts")
  # print(time(vec_yoy))
  
  
  # print("this_year in un_yoy_ts")
  # print(this_year)
  
  this_quarter <- as.integer(4 * (time(vec_yoy) - this_year + 0.25))
  
  # print("this_quarter in un_yoy_ts")
  # print(this_quarter)
  
  this_start <- c(first(this_year), first(this_quarter))
  # print("this_start in un_yoy_ts")
  # print(this_start)
  
  if (freq == 4) {
    un_yoy_ts <- ts(un_yoy_vec, start = this_start, frequency = freq)
  }
  
  if (freq == 12) {
    un_yoy_ts <- ts(un_yoy_vec, start = start(vec_yoy), frequency = freq)
  }
  
  
  
  return(un_yoy_ts)
}

un_diff_ts <- function(last_undiffed, diffed_ts) {
  undiffed <- as.numeric(last_undiffed) + cumsum(diffed_ts)
  
  this_year <- as.integer(floor(time(diffed_ts)))
  this_quarter <- as.integer(4 * (time(diffed_ts) - this_year + 0.25))
  undiffed_ts <- ts(undiffed, start = c(first(this_year), first(this_quarter)),
                    end = c(last(this_year), last(this_quarter)), frequency = 4)
  
  return(undiffed_ts)
}

un_diff <- function(last_undiffed, diffed_ts) {
  undiffed <- as.numeric(last_undiffed) + cumsum(diffed_ts)
  return(undiffed)
  
}


univariate_analysis <- function(rgdp_data, n_offset = 0, freq = 4, h_max = 8, 
                                tsCV_win = 40, do_auto_lambda = TRUE, 
                                do_demetra = TRUE, do_auto_biasadj = FALSE,
                                do_other_auto = FALSE, do_ets = FALSE) {
  
  
  
  
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
  
  yoy_data_ts <- make_yoy_ts(rgdp_data, freq = freq)
  
  
  p_base_lev <- autoplot(rgdp_data) + 
    labs(y = "GDP", x = "", title = "Real GDP forecast") +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  p_base_yoy <- autoplot(make_yoy_ts(rgdp_data, freq = freq)) +
    labs(y = "GDP", x = "", title = "YoY growth of GDP") +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  if (n_offset > 0) {
    p_base_lev <- p_base_lev + 
      autolayer(test_data, series = "test data") 
    
    p_base_yoy <- p_base_yoy + 
      autolayer(yoy_test_data, series = "test data") 
  }
  
  
  
  if (do_demetra) {
    demetra_output <- get_demetra_params(data_path)
    demetra_output_external <- get_demetra_params(external_data_path)
  }
  
  e_list <- NULL
  e_names <-  c()
  e_nw_list <- NULL
  e_nw_names <-  c()
  acc_all_training <- vector()
  acc_all_test <- vector()
  y_ave_all <- list()
  y_gt_all <- list()
  y_total_all <- list()
  fit_models <- list()
  
  
  # arima_1 : arima_logrgdp_list_dem, data is in logs, params from demetra
  # arima_2 : arima_logrgdp_auto_slow, data is in logs, auto.arima with stepwise FALSE and approximation FALSE
  # arima_3 : arima_rgdp_auto_slow, data is levels, auto.arima lambda = 0, with stepwise FALSE and approximation FALSE
  # arima_4 : arima_rgdp_auto_slow_badj, arima_3 with bias.adjust = TRUE
  # arima_5 : arima_yoyrgdp_auto_slow, arima_2 but data is in yoy growth
  
  
  if (do_demetra) {
    
    print("Fitting arima suggested by Demetra (model 1)")
    
    fit_arima_1 <- fit_arimas(
      y_ts = log(rgdp_data), order_list = demetra_output[["rgdp_order_list"]],
      this_arima_names = "rgdp")[[1]]
    
    fit_models[[length(fit_models) + 1]] <-  fit_arima_1
    
    farima_1 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_1$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_1$arma)[c(3, 7, 4)],
                     include.drift = TRUE), h = h)
    }
    
    e_1 <- tsCV(log(rgdp_data), farima_1, h = 8, window = tsCV_win)
    e_1_nw <- tsCV(log(rgdp_data), farima_1, h = 8)
    
    if(is.null(e_list)) {
      e_list <- list(e_1)
    } else {
      e_list[[length(e_list) + 1]] <-  e_1
    }
    
    if(is.null(e_nw_list)) {
      e_nw_list <- list(e_1_nw)
    } else {
      e_nw_list[[length(e_nw_list) + 1]] <-  e_1_nw
    }
    
    e_names <- c(e_names, "dm")
    names(e_list) <- e_names
    e_nw_names <- c(e_nw_names, "dm")
    names(e_nw_list) <- e_nw_names
    
    fc_arima_1 <- forecast(fit_arima_1, h = h_max)
    
    if (n_offset > 0) {
      acc_arima_1 <- accuracy(f = fc_arima_1, x = log(test_data))
      acc_all_training = rbind(acc_all_training, acc_arima_1[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_1[2,])
      
    } else {
      acc_arima_1 <- accuracy(f = fc_arima_1)
      acc_all_training = rbind(acc_all_training, acc_arima_1[1,])
    }
    
    difflog_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, dodifflog = TRUE, isloglevel = TRUE)
    yoy_fc_arima_1 <- fc_yoy_from_fc_level(fc_obj = fc_arima_1, dodifflog = FALSE, isloglevel = TRUE)
    
    y_ave_logdem_ldiff <- difflog_fc_arima_1[["yearly_average_yoy"]]
    y_ave_logdem <- yoy_fc_arima_1[["yearly_average_yoy"]]
    y_gt_logdem_ldiff <- difflog_fc_arima_1[["yearly_growth_of_total"]]
    y_gt_logdem <- yoy_fc_arima_1[["yearly_growth_of_total"]]
    y_total_logdem_ldiff <- difflog_fc_arima_1[["yearly_total"]]
    y_total_logdem <- yoy_fc_arima_1[["yearly_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_logdem))
    y_gt_all <- c(y_gt_all, list(y_gt_logdem))
    y_total_all <- c(y_total_all, list(y_total_logdem))
    
    y_tot_dm_2018 <- round(100*as.numeric(y_total_logdem["2018"]), digits = 2)
    y_tot_dm_2019 <- round(100*as.numeric(y_total_logdem["2019"]), digits = 2)
    y_ave_dm_2018 <- round(100*as.numeric(y_ave_logdem["2018"]), digits = 2)
    y_ave_dm_2019 <- round(100*as.numeric(y_ave_logdem["2019"]), digits = 2)
    gt_dm_2018 <- round(100*as.numeric(y_gt_logdem["2018"]), digits = 2)
    gt_dm_2019 <- round(100*as.numeric(y_gt_logdem["2019"]), digits = 2)
    
    p_base_lev <- p_base_lev + 
      autolayer(exp(fc_arima_1$mean), series = "demetra")
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_arima_1$yoy_fc, series = "demetra")
    
  }
  
  if (do_auto_lambda) {
    print("Fitting auto.arima (sw and apprx set to FALSE) to level data with lambda=0 (model 3)")
    
    fit_arima_3 <- fit_arimas(
      y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
      my_lambda = 0, my_biasadj = FALSE
    )[[1]]
    
    fit_models[[length(fit_models) + 1]] <-  fit_arima_3
    
    farima_3 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_3$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_3$arma)[c(3, 7, 4)],
                     include.drift = TRUE, lambda = 0), h = h)
    }
    
    e_3 <- tsCV(rgdp_data, farima_3, h = 8, window = tsCV_win)
    e_3_nw <- tsCV(rgdp_data, farima_3, h = 8)
    
    if(is.null(e_list)) {
      e_list <- list(e_3)
    } else {
      e_list[[length(e_list) + 1]] <-  e_3
    }
    
    if(is.null(e_nw_list)) {
      e_nw_list <- list(e_3_nw)
    } else {
      e_nw_list[[length(e_nw_list) + 1]] <-  e_3_nw
    }
    
    e_names <- c(e_names, "au")
    names(e_list) <- e_names
    e_nw_names <- c(e_nw_names, "au")
    names(e_nw_list) <- e_nw_names
    
    fc_arima_3 <- forecast(fit_arima_3, h = h_max)
    
    if (n_offset > 0) {
      acc_arima_3 <- accuracy(f = fc_arima_3, x = test_data)
      acc_all_training = rbind(acc_all_training, acc_arima_3[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_3[2,])
      
    } else {
      acc_arima_3 <- accuracy(f = fc_arima_3)
      acc_all_training = rbind(acc_all_training, acc_arima_3[1,])
    }
    
    
    yoy_fc_arima_3 <- fc_yoy_from_fc_level(fc_arima_3)
    
    y_ave_auto <- yoy_fc_arima_3[["yearly_average_yoy"]]
    y_gt_auto <- yoy_fc_arima_3[["yearly_growth_of_total"]]
    y_total_auto <- yoy_fc_arima_3[["yearly_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_auto))
    y_gt_all <- c(y_gt_all, list(y_gt_auto))
    y_total_all <- c(y_total_all, list(y_total_auto))
    
    gt_auto_2018 <- round(100*as.numeric(y_gt_auto["2018"]), digits = 2)
    gt_auto_2019 <- round(100*as.numeric(y_gt_auto["2019"]), digits = 2)
    y_tot_auto_2018 <- round(100*as.numeric(y_total_auto["2018"]), digits = 2)
    y_tot_auto_2019 <- round(100*as.numeric(y_total_auto["2019"]), digits = 2)
    y_ave_auto_2018 <- round(100*as.numeric(y_ave_auto["2018"]), digits = 2)
    y_ave_auto_2019 <- round(100*as.numeric(y_ave_auto["2019"]), digits = 2)
    
    p_base_lev <- p_base_lev + 
      autolayer(fc_arima_3, series = "auto", PI = FALSE)
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_arima_3$yoy_fc, series = "auto")
    
  }
  
  if (do_other_auto) {
    print("Fitting auto.arima to log data (sw and apprx set to FALSE) (model 2)")
    fit_arima_2 <- fit_arimas(
      y_ts = log(rgdp_data), include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]
    
    print("Fitting auto.arima (sw and apprx set to FALSE) to level data with lambda=0, bias-adjusted (model 4)")
    fit_arima_4 <- fit_arimas(
      y_ts = rgdp_data, include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp", 
      my_lambda = 0, my_biasadj = TRUE
    )[[1]]
    
    print("Fitting auto.arima to YoY growth data (sw and apprx set to FALSE) (model 5)")
    fit_arima_5 <- fit_arimas(
      y_ts = make_yoy_ts(rgdp_data, freq = freq), include.constant = TRUE, auto = TRUE, 
      do_stepwise = FALSE, do_approximation = FALSE, this_arima_names = "rgdp")[[1]]
    
    fit_models[[length(fit_models) + 1]] <-  fit_arima_2
    fit_models[[length(fit_models) + 1]] <-  fit_arima_4
    fit_models[[length(fit_models) + 1]] <-  fit_arima_5
    
    farima_2 <- function(x, h) {
      forecast(Arima(x, order = (fit_arima_2$arma)[c(1, 6, 2)],
                     seasonal = (fit_arima_2$arma)[c(3, 7, 4)],
                     include.drift = TRUE), h = h)
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
    
    e_2 <- tsCV(log(rgdp_data), farima_2, h = 8, window = tsCV_win)
    e_4 <- tsCV(rgdp_data, farima_4, h = 8, window = tsCV_win)
    e_5 <- tsCV(yoy_data_ts, farima_5, h = 8, window = tsCV_win)
    
    e_2_nw <- tsCV(log(rgdp_data), farima_2, h = 8)
    e_4_nw <- tsCV(rgdp_data, farima_4, h = 8)
    e_5_nw <- tsCV(yoy_data_ts, farima_5, h = 8)
    
    if (is.null(e_list)) {
      e_list <- list(e_2)
      e_list[[2]] <- e_4
      e_list[[3]] <- e_5
    } else {
      e_list[[length(e_list) + 1]] <-  e_2
      e_list[[length(e_list) + 1]] <-  e_4
      e_list[[length(e_list) + 1]] <-  e_5
    }
    
    if (is.null(e_nw_list)) {
      e_nw_list <- list(e_2_nw)
      e_nw_list[[2]] <- e_4_nw
      e_nw_list[[3]] <- e_5_nw
    } else {
      e_nw_list[[length(e_nw_list) + 1]] <-  e_2_nw
      e_nw_list[[length(e_nw_list) + 1]] <-  e_4_nw
      e_nw_list[[length(e_nw_list) + 1]] <-  e_5_nw
    }
    
    e_names <- c(e_names, c("lau", "aubj", "yoyau"))
    names(e_list) <- e_names
    e_nw_names <- c(e_nw_names, c("lau", "aubj", "yoyau"))
    names(e_nw_list) <- e_nw_names
    
    
    fc_arima_2 <- forecast(fit_arima_2, h = h_max) 
    fc_arima_4 <- forecast(fit_arima_4, h = h_max, lambda = 0, biasadj = TRUE) 
    fc_arima_5 <- forecast(fit_arima_5, h = h_max) 
    
    if (n_offset > 0) {
      acc_arima_2 <- accuracy(f = fc_arima_2, x = log(test_data))
      acc_arima_4 <- accuracy(f = fc_arima_4, x = test_data)
      acc_arima_5 <- accuracy(f = fc_arima_5, x = yoy_test_data)
      
      acc_all_training = rbind(acc_all_training, acc_arima_2[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_2[2,])
      acc_all_training = rbind(acc_all_training, acc_arima_4[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_4[2,])
      acc_all_training = rbind(acc_all_training, acc_arima_5[1,])
      acc_all_test = rbind(acc_all_test, acc_arima_5[2,])
      
    } else {
      acc_arima_2 <- accuracy(f = fc_arima_2)
      acc_arima_4 <- accuracy(f = fc_arima_4)
      acc_arima_5 <- accuracy(f = fc_arima_5)
      
      acc_all_training = rbind(acc_all_training, acc_arima_2[1,])
      acc_all_training = rbind(acc_all_training, acc_arima_4[1,])
      acc_all_training = rbind(acc_all_training, acc_arima_5[1,])
    }
    
    
    difflog_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, dodifflog = TRUE, isloglevel = TRUE)
    yoy_fc_arima_2 <- fc_yoy_from_fc_level(fc_obj = fc_arima_2, isloglevel = TRUE)
    yoy_fc_arima_4 <- fc_yoy_from_fc_level(fc_arima_4)
    
    y_ave_logauto_ldiff <- difflog_fc_arima_2[["yearly_average_yoy"]]
    y_ave_logauto <- yoy_fc_arima_2[["yearly_average_yoy"]]
    y_ave_auto_badj <- yoy_fc_arima_4[["yearly_average_yoy"]]
    
    y_gt_logauto_ldiff <- difflog_fc_arima_2[["yearly_growth_of_total"]]
    y_gt_logauto <- yoy_fc_arima_2[["yearly_growth_of_total"]]
    y_gt_auto_badj <- yoy_fc_arima_4[["yearly_growth_of_total"]]
    
    y_total_logauto_ldiff <- difflog_fc_arima_2[["yearly_total"]]
    y_total_logauto <- yoy_fc_arima_2[["yearly_total"]]
    y_total_auto_badj <- yoy_fc_arima_4[["yearly_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_logauto), list(y_ave_auto_badj))
    y_gt_all <- c(y_gt_all, list(y_gt_logauto), list(y_gt_auto_badj))
    y_total_all <- c(y_total_all, list(y_total_logauto), list(y_total_auto_badj))
    
    p_base_lev <- p_base_lev + 
      autolayer(exp(fc_arima_2$mean), series = "auto of log") + 
      autolayer(fc_arima_4, series = "auto with badj")
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_arima_2$yoy_fc, series = "auto of log") + 
      autolayer(yoy_fc_arima_4$yoy_fc, series = "auto with badj") + 
      autolayer(fc_arima_5, series = "auto of yoy", PI = FALSE)
  }
  
  if (do_ets) {
    print("Fitting auto ETS to level data (model 6)")
    fit_ets_6 <- ets(rgdp_data)
    
    print("Fitting auto ETS to level data, with lambda=0 and bias-adjusted (model 7)")
    fit_ets_7 <- ets(rgdp_data, lambda = 0, biasadj = TRUE)
    
    fit_models[[length(fit_models) + 1]] <-  fit_ets_6
    fit_models[[length(fit_models) + 1]] <-  fit_ets_7
    
    fets_6 <- function(x, h) {
      forecast(ets(x), h = h)
    }
    
    fets_7 <- function(x, h) {
      forecast(ets(x, lambda = 0, biasadj = TRUE), h = h)
    }
    
    e_6 <- tsCV(rgdp_ts, fets_6, h = 8, window = tsCV_win)
    e_7 <- tsCV(rgdp_ts, fets_7, h = 8, window = tsCV_win)
    e_6_nw <- tsCV(rgdp_ts, fets_6, h = 8)
    e_7_nw <- tsCV(rgdp_ts, fets_7, h = 8)
    
    if (is.null(e_list)) {
      e_list <- list(e_6)
      e_list[[2]] <- e_7
    } else {
      e_list[[length(e_list) + 1]] <-  e_6
      e_list[[length(e_list) + 1]] <-  e_7
    }
    
    if (is.null(e_nw_list)) {
      e_nw_list <- list(e_6_nw)
      e_nw_list[[2]] <- e_7_nw
    } else {
      e_nw_list[[length(e_nw_list) + 1]] <-  e_6_nw
      e_nw_list[[length(e_nw_list) + 1]] <-  e_7_nw
    }
    
    e_names <- c(e_names, c("ets", "etsbj"))
    names(e_list) <- e_names
    e_nw_names <- c(e_nw_names, c("ets", "etsbj"))
    names(e_nw_list) <- e_nw_names
    
    fc_ets_6 <- forecast(fit_ets_6, h = h_max) 
    fc_ets_7 <- forecast(fit_ets_7, h = h_max) 
    
    if (n_offset > 0) {
      acc_ets_6 <- accuracy(f = fc_ets_6, x = test_data)
      acc_ets_7 <- accuracy(f = fc_ets_7, x = test_data)
      
      acc_all_training = rbind(acc_all_training, acc_ets_6[1,])
      acc_all_test = rbind(acc_all_test, acc_ets_6[2,])
      acc_all_training = rbind(acc_all_training, acc_ets_7[1,])
      acc_all_test = rbind(acc_all_test, acc_ets_7[2,])
      
    } else {
      acc_ets_6 <- accuracy(f = fc_ets_6)
      acc_ets_7 <- accuracy(f = fc_ets_7)
      
      acc_all_training = rbind(acc_all_training, acc_ets_6[1,])
      acc_all_training = rbind(acc_all_training, acc_ets_7[1,])
    }
    
    yoy_fc_ets_6 <- fc_yoy_from_fc_level(fc_ets_6)
    yoy_fc_ets_7 <- fc_yoy_from_fc_level(fc_ets_7)
    
    y_ave_ets <- yoy_fc_ets_6[["yearly_average_yoy"]]
    y_ave_ets_badj <- yoy_fc_ets_7[["yearly_average_yoy"]]
    
    y_total_ets <- yoy_fc_ets_6[["yearly_total"]]
    y_total_ets_badj <- yoy_fc_ets_7[["yearly_total"]]
    
    y_gt_ets <- yoy_fc_ets_6[["yearly_growth_of_total"]]
    y_gt_ets_badj <- yoy_fc_ets_7[["yearly_growth_of_total"]]
    
    y_ave_all <- c(y_ave_all, list(y_ave_ets), list(y_ave_ets_badj))
    y_gt_all <- c(y_gt_all, list(y_gt_ets), list(y_gt_ets_badj))
    y_total_all <- c(y_total_all, list(y_total_ets), list(y_total_ets_badj))
    
    p_base_lev <- p_base_lev + 
      autolayer(fc_ets_6, series = "ets", PI = FALSE) + 
      autolayer(fc_ets_7, series = "ets lambda0", PI = FALSE)
    
    p_base_yoy <-  p_base_yoy + 
      autolayer(yoy_fc_ets_6$yoy_fc, series = "ets") + 
      autolayer(yoy_fc_ets_7$yoy_fc, series = "ets lambda0")
    
  }
  
  # print(paste("Computing CV errors (tsCV)) for each model, with and without a rolling window of", tsCV_win))
  
  cv_rmse <- map(e_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
  cv_mae <- map(e_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)
  
  cv_rmse_nw <- map(e_nw_list, ~ sqrt(colMeans(.^2, na.rm = TRUE))) %>% reduce(rbind)
  cv_mae_nw <- map(e_nw_list, ~ colMeans(abs(.), na.rm = TRUE)) %>% reduce(rbind)  
  
  if (is.null(dim(cv_rmse)) ) {
    dim(cv_rmse) <- c(1, length(cv_rmse))
    dim(cv_rmse_nw) <- c(1, length(cv_rmse_nw))
    dim(cv_mae) <- c(1, length(cv_mae))
    dim(cv_mae_nw) <- c(1, length(cv_mae_nw))
  }
  
  row.names(cv_mae) <- e_names
  row.names(cv_rmse) <- e_names
  row.names(cv_mae_nw) <- e_nw_names
  row.names(cv_rmse_nw) <- e_nw_names
  
  y_ave_all <- reduce(y_ave_all, cbind)
  y_gt_all <- reduce(y_gt_all, cbind)
  y_total_all <- reduce(y_total_all, cbind)
  
  return(list(fit_models = fit_models,
              yearly_total_y = y_total_all,
              growth_of_yearly_total_y = y_gt_all,
              yearly_average_yoy_growth = y_ave_all,
              accuracy_measures_training_set = acc_all_training,
              accuracy_measures_test_set = acc_all_test,
              plot_levels = p_base_lev,
              plot_yoy = p_base_yoy,
              cv_rmse = cv_rmse,
              cv_rmse_no_rolling_window = cv_rmse_nw,
              cv_mae = cv_mae,
              cv_mae_no_rolling_window = cv_mae_nw)
  )
  
}


var_fc_from_best <- function(rank_tibble, VAR_data, levQ, custom_h = 12) {
  
  end_time_vardata <- VAR_data %>% time %>% last
  start_time_fc <- end_time_vardata + 0.25
  start_year_fc <- floor(start_time_fc)
  start_quarter_fc <- as.integer(4*(start_time_fc - start_year_fc + 0.25))
  
  print("start_quarter_fc")
  print(start_quarter_fc)
  
  
  VARs_from_best_inputs <- rank_tibble %>%
    mutate(
      vfit = map2(
        variables, lags,
        function(x, y) vars::VAR(y = VAR_data[, x], p = y)
      ),
      fc = map(vfit, forecast, h = custom_h),
      fc_rgdp_mean = map(fc, c("forecast", "rgdp", "mean")),
      fc_rgdp_mean = map(fc_rgdp_mean, ts, 
                         start = c(start_year_fc, start_quarter_fc),
                         frequency = 4),
      ee1 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee1"),
      ee2 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee2"),
      bp1 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp1"),
      bp2 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp2")
    )
  
  # fc_with_ave <- add_average_fcs(VARs_from_best_inputs)
  # 
  # fc_with_ave <- fc_with_ave %>%
  #   mutate(
  #     ee1 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee1"),
  #     ee2 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee2"),
  #     bp1 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp1"),
  #     bp2 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp2")
  #   )
  
  
  fc_with_ave <- 1
  return(VARs_from_best_inputs)
  # return(list(
  #   var_fc_indiv = VARs_from_best_inputs,
  #   fcs_ave = fc_with_ave
  # ))
  
}


var_cv <- function(var_data, this_p, this_type = "const", n_cv = 8, h_max = 6, 
                   train_test_marks = NULL,
                   training_length = 20, timetk_idx = TRUE,
                   external_idx = NULL) {
  
  if (is.null(train_test_marks)) {
    train_test_dates <- make_test_dates_list(ts_data = var_data, 
                                             type = "tscv", n = n_cv, h_max = h_max, 
                                             training_length = training_length, 
                                             timetk_idx = timetk_idx, 
                                             external_idx = external_idx)
    
    train_test_dates <- train_test_dates[["list_of_year_quarter"]]
  }
  
  n <- nrow(var_data)
  
  cv_errors <- list_along(1:n_cv)
  cv_test_data <- list_along(1:n_cv)
  cv_fcs <- list_along(1:n_cv)
  cv_vbl_names <- list_along(1:n_cv)
  cv_lag <- list_along(1:n_cv)
  
  for (i in seq_along(1:n_cv)) {
    
    this_tra_s <- train_test_dates[[i]]$tra_s
    this_tra_e <- train_test_dates[[i]]$tra_e
    
    this_tes_s <- train_test_dates[[i]]$tes_s
    this_tes_e <- train_test_dates[[i]]$tes_e
    
    training_y <- window(var_data, 
                         start = this_tra_s,
                         end = this_tra_e)
    
    # print("nrow(training_y)")
    # print(nrow(training_y))
    
    training_rgdp <- training_y[ , "rgdp"]
    
    test_y <- window(var_data, 
                     start = this_tes_s,
                     end = this_tes_e)
    
    test_rgdp <- test_y[ , "rgdp"]
    
    # print("training_rgdp")
    # print(training_rgdp)
    # 
    # 
    # print("test_rgdp")
    # print(test_rgdp)
    
    
    this_var <- vars::VAR(y = training_y, p = this_p, type = this_type) 
    
    this_fc <- forecast(this_var, h = h_max)
    
    this_rgdp_fc_mean <- this_fc[["forecast"]][["rgdp"]][["mean"]]
    
    fc_error <- test_rgdp - this_rgdp_fc_mean
    
    vbl_names <- colnames(training_y)
    
    lag <- this_p
    
    cv_vbl_names[[i]] <- vbl_names
    cv_lag[[i]] <- lag
    cv_errors[[i]] <- fc_error
    cv_test_data[[i]] <- test_rgdp
    cv_fcs[[i]] <- this_rgdp_fc_mean
    
  }
  
  cv_test_data_mat <- reduce(cv_test_data, rbind)
  cv_fcs_mat <- reduce(cv_fcs, rbind)
  
  # eliminate pesky "out" of it
  dimnames(cv_test_data_mat) <- NULL
  dimnames(cv_fcs_mat) <- NULL
  
  mean_cv_rmse <- fcs_accu(cv_fcs_mat, cv_test_data_mat)
  
  return(list(cv_errors = cv_errors,
              cv_test_data = cv_test_data,
              cv_fcs = cv_fcs,
              mean_cv_rmse = mean_cv_rmse,
              cv_vbl_names = cv_vbl_names,
              cv_lag = cv_lag))
}


