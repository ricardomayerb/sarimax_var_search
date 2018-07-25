source('./R/utils_av.R')

country_name <- "Bolivia"

country_data_level_ts <- get_raw_data_ts(country = country_name)
rgdp_level_ts <- country_data_level_ts[,"rgdp"]
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)



names_of_variables <- colnames(country_data_level_ts)


sta_reco_list <- list_along(names_of_variables)
stationarity_list <- list_along(names_of_variables)


for (j in seq_along(names_of_variables)) {
  this_variable <- names_of_variables[j]
  this_variable_ts <- country_data_level_ts[ , this_variable]
  this_variable_ts <- na.omit(this_variable_ts)
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
saveRDS(VAR_data_for_estimation , "./data/VAR_data_Bolivia.rds")

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]


variable_names <- colnames(VAR_data_for_estimation)
ncolumns <- ncol(VAR_data_for_estimation)

this_bt <- 1.5

vec_max_lags <- c(1, 2, 3, 4)
vec_n_varsize <- c(4, 5)
n_best <- 5
number_of_cv <- 8
fc_horizon <- 8
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
# tictoc::tic()
# var_res <- try_sizes_vbls_lags(vec_size = vec_n_varsize, 
#                                vec_lags = vec_max_lags,
#                                var_data = VAR_data_for_estimation,
#                                rgdp_yoy_ts = rgdp_yoy_ts,
#                                rgdp_level_ts = rgdp_level_ts, 
#                                target_v = target_rgdp,
#                                pre_selected_v = c(""), 
#                                is_cv = TRUE,
#                                training_length = train_span,
#                                h_max = fc_horizon, n_cv = number_of_cv,
#                                bt_factor = 0, maxlag_ccm = 8,
#                                return_cv = ret_cv,
#                                rgdp_current_form = rgdp_rec)
# 
# tictoc::toc()

# cv <- var_res_1$cv_objects
# models_ranking <- var_res_1$accu_rankings_models 
# 
# models_and_accu <- var_res[["accu_rankings_models"]]
# cv_objects <- var_res[["cv_objects"]]


# all VARs size 2, 16.59 sec
tictoc::tic()
var_res_1 <- try_sizes_vbls_lags(vec_size = 2, 
                                 vec_lags = c(1,2,3,4,5),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_yoy_ts = rgdp_yoy_ts,
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
                                 rgdp_yoy_ts = rgdp_yoy_ts,
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
                                 vec_lags = c(2,3),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_yoy_ts = rgdp_yoy_ts,
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


tictoc::tic()
var_res_4 <- try_sizes_vbls_lags(vec_size = 4, 
                                 vec_lags = c(1, 4, 5),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_yoy_ts = rgdp_yoy_ts,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("exp_hydrocarbon"), 
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
var_res_5 <- try_sizes_vbls_lags(vec_size = 5, 
                                 vec_lags = c(2, 3),
                                 var_data = VAR_data_for_estimation,
                                 rgdp_yoy_ts = rgdp_yoy_ts,
                                 rgdp_level_ts = rgdp_level_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("exp_hydrocarbon"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()

models_and_accu_5 <- var_res_5[["accu_rankings_models"]]
cv_objects_5 <- var_res_5[["cv_objects"]]

# how to delete the same models? something with unique?
models_and_accu_12345 <- rbind(models_and_accu_1, models_and_accu_2, models_and_accu_3, models_and_accu_4, models_and_accu_5) %>%
  mutate(rank_1 = rank(rmse_1), rank_2 = rank(rmse_2), rank_3 = rank(rmse_3), rank_4 = rank(rmse_4), rank_5 = rank(rmse_5), 
         rank_6 = rank(rmse_6), rank_7 = rank(rmse_7), rank_8 = rank(rmse_8))

# mmmmm maybe you could play around with "distinct" from dplyr and use variables + lags to determine uniqueness
# 
# foo <- myObjectWithCvStuff %>% distinct(variables, lags)
# ?distinct
# 
# foo <- models_and_accu_12345 %>% distinct(variables, lags)
# models_and_accu_12345_test <- distinct(models_and_accu_12345, variables, lags, .keep_all = FALSE)

# models_and_accu_1 <- var_res_1[["accu_rankings_models"]]

saveRDS(models_and_accu_12345, "./data/Bolivia_by_step_12345.rds")


cv_objects_12345 <- rbind(cv_objects_1, cv_objects_2, cv_objects_3, cv_objects_4, cv_objects_5)

saveRDS(cv_objects_12345, "./data/Bolivia_by_step_12345_cv_objects.rds")
