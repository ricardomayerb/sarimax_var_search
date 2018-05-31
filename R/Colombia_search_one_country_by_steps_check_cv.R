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

country_name <- "Colombia"

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

this_bt <- 1.4

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

# this_bt = 1.5 test. 42.93967 minutes

# vec_a_priori_variables <- c("rpc")
vec_a_priori_variables <- c("")

ret_cv = TRUE

tictoc::tic()
var_res <- try_sizes_vbls_lags(vec_size = vec_n_varsize,
                               vec_lags = vec_max_lags,
                               var_data = VAR_data_for_estimation, yoy_data = VAR_data_for_estimation,
                               level_data = country_data_level_ts,
                               target_v = target_rgdp,
                               pre_selected_v = vec_a_priori_variables,
                               is_cv = TRUE,
                               training_length = train_span,
                               h_max = fc_horizon, n_cv = number_of_cv,
                               bt_factor = this_bt, maxlag_ccm = 8,
                               return_cv = ret_cv)

tictoc::toc()

models_and_accu <- var_res[["accu_rankings_models"]]
cv_objects <- var_res[["cv_objects"]]

# all VARs size 2, 16.59 sec
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

models_and_accu_1 <- var_res_1[["accu_rankings_models"]]
cv_objects_1 <- var_res_1[["cv_objects"]]

# all VARs size 3, 4.227333 mintutes
tictoc::tic()
var_res_2 <- try_sizes_vbls_lags(vec_size = 3, 
                                 vec_lags = c(2,3,4,5),
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

models_and_accu_2 <- var_res_2[["accu_rankings_models"]]
cv_objects_2 <- var_res_2[["cv_objects"]]

# all VARs size 4, 2 choices of lag, 22.6505 min
tictoc::tic()
var_res_3 <- try_sizes_vbls_lags(vec_size = 4, 
                                 vec_lags = c(2,3),
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

models_and_accu_3 <- var_res_3[["accu_rankings_models"]]
cv_objects_3 <- var_res_3[["cv_objects"]]

# or 1 pre_chosen size 4, 3 choices of lag, 3.651833 minutes

tictoc::tic()
var_res_4 <- try_sizes_vbls_lags(vec_size = 4, 
                                 vec_lags = c(1, 4, 5),
                                 var_data = VAR_data_for_estimation, yoy_data = VAR_data_for_estimation,
                                 level_data = country_data_level_ts,
                                 target_v = target_rgdp,
                                 pre_selected_v = c("imp"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()
models_and_accu_4 <- var_res_4[["accu_rankings_models"]]
cv_objects_4 <- var_res_4[["cv_objects"]]


# i choose 1 pre_chosen based on the top models i have seen before
# long but necessary for Colombia 172.11 min

tictoc::tic()
var_res_5 <- try_sizes_vbls_lags(vec_size = 5, 
                                 vec_lags = c(2, 3),
                                 var_data = VAR_data_for_estimation, yoy_data = VAR_data_for_estimation,
                                 level_data = country_data_level_ts,
                                 target_v = target_rgdp,
                                 pre_selected_v = c(), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()
models_and_accu_5 <- var_res_5[["accu_rankings_models"]]
cv_objects_5 <- var_res_5[["cv_objects"]]



# how to delete the same models? something with unique?
models_and_accu_12345 <- rbind(models_and_accu_1, models_and_accu_2, models_and_accu_3, models_and_accu_4, models_and_accu_5) %>% 
  mutate(rank_1 = rank(rmse_1), rank_2 = rank(rmse_2), rank_3 = rank(rmse_3), rank_4 = rank(rmse_4), rank_5 = rank(rmse_5), 
         rank_6 = rank(rmse_6))

# mmmmm maybe you could play around with "distinct" from dplyr and use variables + lags to determine uniqueness
# 
# foo <- myObjectWithCvStuff %>% distinct(variables, lags)
# ?distinct
# 
# models_and_accu_12345_test <- distinct(models_and_accu_12345$variables + models_and_accu_12345$lags)
# distinct((rbind(models_and_accu_12345$variables, models_and_accu_12345$lags)))
# foo <- models_and_accu_12345 %>% distinct(variables, lags)
# models_and_accu_12345_test <- distinct(models_and_accu_12345, variables, lags, .keep_all = FALSE)

# models_and_accu_1 <- var_res_1[["accu_rankings_models"]]

saveRDS(models_and_accu_12345, "./data/Colombia_by_step_12345.rds")


cv_objects_12345 <- rbind(cv_objects_1, cv_objects_2, cv_objects_3, cv_objects_4, cv_objects_5)

saveRDS(cv_objects_12345, "./data/Colombia_by_step_12345_cv_objects.rds")

