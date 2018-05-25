library(xts)
library(tibbletime)
library(tidyverse)
library(readxl)
library(timetk)
library(lubridate)
library(forecast)

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

make_yoy_xts <- function(df_xts) {
  new_xts <- diff.xts(df_xts, lag = 4, na.pad = FALSE)/lag.xts(
    df_xts, na.pad = FALSE, k = 4)
}


make_yoy_ts <- function(df_ts) {
  new_ts <- diff.ts(df_ts, lag = 4)/lag.ts(df_xts, k = 4)
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


chop_start_end_xts <- function(df_xts, start_date, end_date) {
  
  start_str <- start_date
  end_str <- end_date
  
  subset_string <- paste0(start_str, "/", end_str)
  
  new_xts <- df_xts[subset_string]
  
  return(new_xts)
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


drop_this_vars <- function(df, vars_to_drop) {
  new_df <- df[,!(names(df) %in% vars_to_drop)]
}




comb_ndiffs <- function(this_series, do_stati_after_seas = TRUE, 
                        return_4_seas = FALSE, do_other_seas = FALSE) {
  
  tests_names <- c("kpss", "pp", "adf")
  tests_season_names <- c("seas", "ocsb", "hegy", "ch")
  tests_alpha <- c(0.01, 0.05, 0.1)
  tests_type <- c("level", "trend")
  
  if (do_stati_after_seas) {
    tests_of_stationarity <- as_tibble(
      expand.grid(tests_names, tests_alpha, tests_type,
                  stringsAsFactors = FALSE)) %>% 
      rename(test = Var1, alpha = Var2, deter_part = Var3) %>% 
      mutate(default_seas = map_dbl(alpha,
                                    ~ nsdiffs(x = this_series, alpha = .)),
             sta_result = pmap_dbl(list(test, alpha, deter_part),
                                   ~ ndiffs(x = this_series, alpha = ..2,
                                            test = ..1, type = ..3)),
             sta_result_after_seas = pmap_dbl(
               list(test, alpha, deter_part, default_seas),
               ~ ndiffs(x = diff(this_series, lag = ..4), 
                        alpha = ..2, test = ..1, type = ..3)),
             recommendation = pmap_chr(
               list(default_seas, sta_result, sta_result_after_seas),
               ~ make_recommendation(seas = ..1, sta = ..2, sta_after_seas = ..3)
                                     )
      )
    if(do_other_seas) {
      tests_of_seasonality <- as_tibble(
        expand.grid(tests_season_names, tests_alpha, stringsAsFactors = FALSE)) %>% 
        rename(test = Var1, alpha = Var2) %>% 
        mutate(seas_result = map2_dbl(test, alpha,
                                      suppressWarnings(
                                        ~ nsdiffs(x = this_series, alpha = .y,
                                                  test = .x)))
        )
    }
    
  } else {
    tests_of_stationarity  <- as_tibble(expand.grid(tests_names, tests_alpha, tests_type,
                                                        stringsAsFactors = FALSE)) %>% 
      rename(test = Var1, alpha = Var2, deter_part = Var3) %>% 
      mutate(default_seas = map_dbl(alpha,
                                    ~ suppressWarnings(
                                      nsdiffs(x = this_series, alpha = .))),
             sta_result = pmap_dbl(list(test, alpha, deter_part),
                                   ~ ndiffs(x = this_series, alpha = ..2,
                                            test = ..1, type = ..3))
      )
    
    if(do_other_seas) {
      tests_of_seasonality  <-  as_tibble(expand.grid(tests_season_names, tests_alpha, 
                                                      stringsAsFactors = FALSE)) %>% 
        rename(test = Var1, alpha = Var2) %>% 
        mutate(seas_result = map2_dbl(test, alpha,  
                                      ~ suppressWarnings(
                                        nsdiffs(x = this_series, alpha = .y,
                                                test = .x)))
        )
    }
    

  }
  
  if(! do_other_seas) {
    return(tests_of_stationarity)
  } else {
    if (return_4_seas) {
      return(list(stationarity = tests_of_stationarity, 
                  seas = tests_of_seasonality))
    } else {
      return(tests_of_stationarity)
    }
  }
  
  
  
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
  
  return(recommendation)
  
}


