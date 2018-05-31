library(MTS)
library(vars)
library(xts)
library(tibbletime)
library(tidyverse)
library(readxl)
library(timetk)
library(lubridate)
library(forecast)
library(gridExtra)
library(grid)
library(haven)


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



fcs_accu <- function(fc_mat, test_data_mat) {
  
  errors_mat <- test_data_mat - fc_mat
  rmse_vec <- sqrt(colMeans(errors_mat^2))
  mean_rmse <- mean(rmse_vec)
  return(mean_rmse)
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



# get_rmse_var_table_at_each_h_diff_yoy <- function(data = cv_objects, h_max = 6){
#   cv_errors <- data[["cv_errors"]]
#   
#   all_rmses <- map(cv_errors, function(x) sqrt(colMeans( (reduce(x, rbind))^2))  )
#   all_rmses_tbl <- reduce(all_rmses, rbind)
#   rmse_names <- paste0("rmse_", 1:h_max)
#   colnames(all_rmses_tbl) <- rmse_names
#   row.names(all_rmses_tbl) <- NULL
#   rmse_each_h <- cbind(data, all_rmses_tbl)
#   
#   # rmse_each_h <- rmse_each_h %>% 
#   #   select(-c(cv_errors))
#   
#   return(rmse_each_h)
#   
# }

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


my_diff <- function(series, lag = 1, differences = 1) {
  if (differences == 0) {
    x_diff <- series
  } else {
    x_diff <- diff(series, lag = lag, differences = differences)
  }
  return(x_diff)
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

make_yoy_xts <- function(df_xts) {
  new_xts <- diff.xts(df_xts, lag = 4, na.pad = FALSE)/lag.xts(
    df_xts, na.pad = FALSE, k = 4)
}


make_yoy_ts <- function(df_ts) {
  new_ts <- base::diff(df_ts, lag = 4)/stats::lag(df_ts, k = -4)
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

un_yoy_ts <- function(init_lev, vec_yoy) {
  
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


