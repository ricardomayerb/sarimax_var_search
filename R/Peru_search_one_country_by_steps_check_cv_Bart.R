source('./R/utils_av.R')

country_name <- "Peru"

country_data_level_ts <- get_raw_data_ts(country = country_name)

rgdp_level_ts <- country_data_level_ts[,"rgdp"]
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)

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

saveRDS(VAR_data_for_estimation , "./data/VAR_data_Peru.rds")

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]

# excluded <- c("ri")
# position_exluded <- colnames(level_data_ts) %in% excluded
# level_data_ts  <- level_data_ts[, ! position_exluded]
# diff_yoy_data_ts  <- diff_yoy_data_ts[, ! position_exluded]
# yoy_data_ts  <- yoy_data_ts[, ! position_exluded]

variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)

this_bt <- 1.3

vec_max_lags <- c(1, 2, 3, 4)
vec_n_varsize <- c(4, 5)
n_best <- 5
number_of_cv <- 8
fc_horizon <- 7
# fc_horizon is set to 7 because 8 is too long for peru
train_span <- 25

if (train_span+fc_horizon+number_of_cv > nrow(VAR_data_for_estimation)) {
  
  print("not enough obs")
  
  stop()
  
}
# one_time bt test, to get an intitial idea of the most important variables

target_rgdp <- c("rgdp")

# this_bt = 1.5 test. 21.63733 minutes

# vec_a_priori_variables <- c("rpc")
vec_a_priori_variables <- c("")

ret_cv = TRUE

# one time test run
# tictoc::tic()
# var_res <- try_sizes_vbls_lags(vec_size = vec_n_varsize,
#                                vec_lags = vec_max_lags,
#                                var_data = VAR_data_for_estimation,
#                                rgdp_level_ts =rgdp_level_ts,
#                                target_v = target_rgdp,
#                                pre_selected_v = vec_a_priori_variables,
#                                is_cv = TRUE,
#                                training_length = train_span,
#                                h_max = fc_horizon, n_cv = number_of_cv,
#                                bt_factor = this_bt, maxlag_ccm = 8,
#                                return_cv = ret_cv,
#                                rgdp_current_form = rgdp_rec)
# 
# tictoc::toc()
# 
# 
# models_and_accu <- var_res[["accu_rankings_models"]]
# cv_objects <- var_res[["cv_objects"]]


# all VARs size 2, 0.32 minutes

tictoc::tic()
var_res_1 <- try_sizes_vbls_lags(vec_size = 2, 
                                 vec_lags = c(1,2,3,4,5),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c(""), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()

models_and_accu_1 <- var_res_1[["accu_rankings_models"]]
cv_objects_1 <- var_res_1[["cv_objects"]]

# all VARs size 3, 4.227333 mintutes
tictoc::tic()
var_res_2 <- try_sizes_vbls_lags(vec_size = 3, 
                                 vec_lags = c(1,2,3,4,5),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c(""), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()

models_and_accu_2 <- var_res_2[["accu_rankings_models"]]
cv_objects_2 <- var_res_2[["cv_objects"]]

# all VARs size 4, 2 choices of lag, 22.6505 min
tictoc::tic()
var_res_3 <- try_sizes_vbls_lags(vec_size = 4, 
                                 vec_lags = c(1,2),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c(""), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()

models_and_accu_3 <- var_res_3[["accu_rankings_models"]]
cv_objects_3 <- var_res_3[["cv_objects"]]


# or 1 pre_chosen size 4, 3 choices of lag, 3.651833 minutes

tictoc::tic()
var_res_4 <- try_sizes_vbls_lags(vec_size = 4, 
                                 vec_lags = c(3, 4),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("serv"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()
models_and_accu_4 <- var_res_4[["accu_rankings_models"]]
cv_objects_4 <- var_res_4[["cv_objects"]]

tictoc::tic()
var_res_4a <- try_sizes_vbls_lags(vec_size = 4, 
                                 vec_lags = c(3, 4),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("expec_indus"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()
models_and_accu_4a <- var_res_4a[["accu_rankings_models"]]
cv_objects_4a <- var_res_4a[["cv_objects"]]


# i know this one will take very look but the one time bt-test gives me the idea that it is necessary
# 182.87 min

tictoc::tic()
var_res_5 <- try_sizes_vbls_lags(vec_size = 5, 
                                 vec_lags = c(1, 2, 3),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("serv"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()
models_and_accu_5 <- var_res_5[["accu_rankings_models"]]
cv_objects_5 <- var_res_5[["cv_objects"]]

tictoc::tic()
var_res_5a <- try_sizes_vbls_lags(vec_size = 5, 
                                 vec_lags = c(2, 3),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("expec_indus"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()
models_and_accu_5a <- var_res_5a[["accu_rankings_models"]]
cv_objects_5a <- var_res_5a[["cv_objects"]]

models_and_accu_12345 <- rbind(models_and_accu_1, models_and_accu_2, models_and_accu_3, models_and_accu_4, models_and_accu_4a,
                               models_and_accu_5, models_and_accu_5a) %>% 
  mutate(rank_1 = rank(rmse_1), rank_2 = rank(rmse_2), rank_3 = rank(rmse_3), rank_4 = rank(rmse_4), rank_5 = rank(rmse_5), 
         rank_6 = rank(rmse_6))

# models_and_accu_1 <- var_res_1[["accu_rankings_models"]]

saveRDS(models_and_accu_12345, "./data/Peru_by_step_12345.rds")

cv_objects_12345 <- rbind(cv_objects_1, cv_objects_2, cv_objects_3, cv_objects_4, cv_objects_4a, cv_objects_5, cv_objects_5a)

saveRDS(cv_objects_12345, "./data/Peru_by_step_12345_cv_objects.rds")
