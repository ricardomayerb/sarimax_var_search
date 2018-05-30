# source('./R/var_functions.R')
# source('./R/utils_vars.R')
# 
# data_path <- "./data/pre_r_data/"
# 
# file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
# file_paths <- paste0(data_path, file_names)
# country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")
# 
# general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
#                                     "month", "conf_emp", "conf_ibre", "ip_ine", 
#                                     "vta_auto", "exist"))
# # to make the data work we have to delete "m2" for argentina, "imp_int", "imp_k" for Ecuador and 
# # "imp_consumer", "imp_intermediate", "imp_capital" for Mexico
# extra_vars_to_drop <- list(Argentina = c("m2", "ri", "", "", "", "", "", "", "", "", ""), 
#                            Bolivia = c("igae", "", "", "", "", "", "", "", "", "", "", ""), 
#                            Brasil = c("", "", "", "", "", "", "", "", "", "", "", ""), 
#                            Chile = c("", "", "", "", "", "", "", "", "", "", "", ""), 
#                            Colombia = c("", "", "", "", "", "", "", "", "", "", "", ""), 
#                            Ecuador = c("imp_int", "imp_k", "", "", "", "", "", "", "", "", "", ""), 
#                            Mexico = c("imp_consumer", "imp_intermediate", "imp_capital", "", "", "", "", "", "", "", "", ""), 
#                            Paraguay = c("", "", "", "", "", "", "", "", "", "", "", ""), 
#                            Peru = c("", "", "", "", "", "", "", "", "", "", "", ""), 
#                            Uruguay = c("cred", "", "", "", "", "", "", "", "", "", "", ""))
# 
# variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)
# 
# data_qm_xts_log <- get_gdp_shaped_data(data_path = data_path, 
#                                        list_variables_to_drop = variables_to_drop,
#                                        only_complete_cases = TRUE,
#                                        apply_log = TRUE)
# 
# data_qm_mts_log <- map(data_qm_xts_log, to_ts_q)
# 
# data_qm_xts_log_yoy <- map(data_qm_xts_log, make_yoy_xts)
# data_qm_mts_log_yoy <- map(data_qm_xts_log_yoy, to_ts_q)
# 
# data_qm_xts_log_yoy_diff <- map(data_qm_xts_log_yoy, diff.xts, na.pad = FALSE)
# data_qm_mts_log_yoy_diff <- map(data_qm_xts_log_yoy_diff, to_ts_q)

# OK countries: bol, bra, chl, col, par, per, ury
# Singular CCM problems: arg, ecu, mex

# # this_country_name <- "Uruguay"  
# this_country_name <- "Chile"  
# this_country <- this_country_name
# level_data_ts <- data_qm_mts_log[[this_country]]
# yoy_data_ts <- data_qm_mts_log_yoy[[this_country]]
# diff_yoy_data_ts <- data_qm_mts_log_yoy_diff[[this_country]]

source('./R/utils_av.R')

country_name <- "Chile"

country_data_level_ts <- get_raw_data_ts(country = country_name)

# # this cuts the time of data testing in 40%
# country_data_level_ts <- na.omit(country_data_level_ts)

names_of_variables <- colnames(country_data_level_ts)

sta_reco_list <- list_along(names_of_variables)
stationarity_list <- list_along(names_of_variables)


for (j in seq_along(names_of_variables)) {
  this_variable <- names_of_variables[j]
  this_variable_ts <- country_data_level_ts[ , this_variable]
  tests_of_stationarity <- suppressWarnings(comb_ndiffs(this_variable_ts))
  tests_of_stationarity$country <- country_name
  tests_of_stationarity$variable <- this_variable
  
  reco <- get_reco_from_sta(tests_of_stationarity, this_variable)
  
  stationarity_list[[j]] <- tests_of_stationarity
  sta_reco_list[[j]] <- reco
  
}


names(stationarity_list) <- names_of_variables
names(sta_reco_list) <- names_of_variables

reco_all_variables <- reduce(sta_reco_list, rbind)
country_transformed_data <- follow_rec(country_data_level_ts, 
                                       reco_all_variables)

VAR_data_for_estimation  <- na.omit(country_transformed_data)



# excluded <- c("ri")
# position_exluded <- colnames(level_data_ts) %in% excluded
# level_data_ts  <- level_data_ts[, ! position_exluded]
# diff_yoy_data_ts  <- diff_yoy_data_ts[, ! position_exluded]
# yoy_data_ts  <- yoy_data_ts[, ! position_exluded]

# colnames(diff_yoy_data_ts)

variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)

this_bt <- 1.3

vec_max_lags <- c(2, 3, 4)
vec_n_varsize <- c(2, 3, 4, 5)
n_best <- 5
number_of_cv <- 8
fc_horizon <- 6
train_span <- 25

if (train_span+fc_horizon+number_of_cv > nrow(VAR_data_for_estimation)) {
  
  print("not enough obs")
  
  stop()
  
}
# one_time bt test, to get an intitial idea of the most important variables

target_rgdp <- c("rgdp")

# this_bt = 1.3 test. 120.3782 minutes

# vec_a_priori_variables <- c("rpc")
vec_a_priori_variables <- c("")

ret_cv = TRUE

# all VARs size 2, 0.32 minutes
tictoc::tic()
var_res_1 <- try_sizes_vbls_lags(vec_size = 2, 
                                 vec_lags = c(1,2,3,4,5),
                                 var_data = VAR_data_for_estimation, yoy_data = VAR_data_for_estimation,
                                 level_data = country_data_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c(""), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()

cv <- var_res_1$cv_objects

cv_one_model <- cv[1, ]
cv_one_model$cv_test_data
cv_one_model$cv_fcs

cv_one_model$cv_test_data[[1]][[1]]
cv_one_model$cv_fcs[[1]][[1]]

cv_one_model$cv_test_data[[1]][[2]]
cv_one_model$cv_fcs[[1]][[2]]

with_rmses <- get_rmse_var_table_at_each_h_diff_yoy(data = cv) %>% 
  mutate(model_type = "VAR")























# # all VARs size 2, 0.32 minutes
# tictoc::tic()
# var_res_1 <- try_sizes_vbls_lags(vec_size = 2, 
#                                  vec_lags = c(1,2,3,4,5),
#                                  var_data = VAR_data_for_estimation, yoy_data = VAR_data_for_estimation,
#                                  level_data = country_data_level_ts, 
#                                  target_v = target_rgdp,
#                                  pre_selected_v = c(""), 
#                                  is_cv = TRUE,
#                                  training_length = train_span,
#                                  h_max = fc_horizon, n_cv = number_of_cv,
#                                  bt_factor = 0, maxlag_ccm = 8,
#                                  return_cv = ret_cv)
# 
# tictoc::toc()


