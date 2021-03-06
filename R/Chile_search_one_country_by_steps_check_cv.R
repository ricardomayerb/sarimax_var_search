source('./R/utils_av.R')

country_name <- "Chile"

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

rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]



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
                                 max_rank = 10,
                                 rgdp_current_form = rgdp_rec)

tictoc::toc()

cv <- var_res_1$cv_objects
models_ranking <- var_res_1$accu_rankings_models 

all_variables_all_h <- models_ranking %>% select(variables) %>% unlist() %>% 
  unique()
all_variables_all_h

variables_in_best_h1 <- models_ranking %>% 
  filter(rank_1 <= 30) %>% 
  select(variables) %>% 
  unlist()
print(table(variables_in_best_h1))

variables_in_best_h2 <- models_ranking %>% 
  filter(rank_2 <= 30) %>% 
  select(variables) %>% 
  unlist()
print(table(variables_in_best_h2))

variables_in_best_h3 <- models_ranking %>% 
  filter(rank_3 <= 30) %>% 
  select(variables) %>% 
  unlist()
print(table(variables_in_best_h3))

variables_in_best_h4 <- models_ranking %>% 
  filter(rank_4 <= 30) %>% 
  select(variables) %>% 
  unlist()
print(table(variables_in_best_h4))

variables_in_best_h5 <- models_ranking %>% 
  filter(rank_5 <= 30) %>% 
  select(variables) %>% 
  unlist()
print(table(variables_in_best_h5))




cv_one_model <- cv[1, ]
cv_one_model$cv_test_data
cv_one_model$cv_fcs




















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


