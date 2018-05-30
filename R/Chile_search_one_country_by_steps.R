source('./R/var_functions.R')
source('./R/utils_vars.R')

data_path <- "./data/excel/"

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
                           Peru = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                           Uruguay = c("cred", "", "", "", "", "", "", "", "", "", "", ""))

variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)

data_qm_xts_log <- get_gdp_shaped_data(data_path = data_path, 
                                       list_variables_to_drop = variables_to_drop,
                                       only_complete_cases = TRUE,
                                       apply_log = TRUE)

data_qm_mts_log <- map(data_qm_xts_log, to_ts_q)

data_qm_xts_log_yoy <- map(data_qm_xts_log, make_yoy_xts)
data_qm_mts_log_yoy <- map(data_qm_xts_log_yoy, to_ts_q)

data_qm_xts_log_yoy_diff <- map(data_qm_xts_log_yoy, diff.xts, na.pad = FALSE)
data_qm_mts_log_yoy_diff <- map(data_qm_xts_log_yoy_diff, to_ts_q)

# OK countries: bol, bra, chl, col, par, per, ury
# Singular CCM problems: arg, ecu, mex

# this_country_name <- "Uruguay"  
this_country_name <- "Chile"  
this_country <- this_country_name
level_data_ts <- data_qm_mts_log[[this_country]]
yoy_data_ts <- data_qm_mts_log_yoy[[this_country]]
diff_yoy_data_ts <- data_qm_mts_log_yoy_diff[[this_country]]

# excluded <- c("ri")
# position_exluded <- colnames(level_data_ts) %in% excluded
# level_data_ts  <- level_data_ts[, ! position_exluded]
# diff_yoy_data_ts  <- diff_yoy_data_ts[, ! position_exluded]
# yoy_data_ts  <- yoy_data_ts[, ! position_exluded]

colnames(diff_yoy_data_ts)

variable_names <- colnames(yoy_data_ts)
ncolumns <- ncol(yoy_data_ts)

this_bt <- 1.3

vec_max_lags <- c(2, 3, 4)
vec_n_varsize <- c(2, 3, 4, 5)
n_best <- 5
number_of_cv <- 8
fc_horizon <- 6
train_span <- 25

if (train_span+fc_horizon+number_of_cv > nrow(diff_yoy_data_ts)) {
  
  print("not enough obs")
  
  stop()
  
}
# one_time bt test, to get an intitial idea of the most important variables

target_rgdp <- c("rgdp")

# this_bt = 1.3 test. 120.3782 minutes

# vec_a_priori_variables <- c("rpc")
vec_a_priori_variables <- c("")

ret_cv = TRUE

# tictoc::tic()
# var_res <- try_sizes_vbls_lags(vec_size = vec_n_varsize,
#                                vec_lags = vec_max_lags,
#                                var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
#                                level_data = level_data_ts,
#                                target_v = target_rgdp,
#                                pre_selected_v = vec_a_priori_variables,
#                                is_cv = TRUE,
#                                training_length = train_span,
#                                h_max = fc_horizon, n_cv = number_of_cv,
#                                bt_factor = this_bt, maxlag_ccm = 8,
#                                return_cv = ret_cv)
# 
# tictoc::toc()
# 
# models_and_accu <- var_res[["accu_rankings_models"]]

# all VARs size 2, 0.32 minutes
tictoc::tic()
var_res_1 <- try_sizes_vbls_lags(vec_size = 2, 
                                 vec_lags = c(1,2,3,4,5),
                                 var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
                                 level_data = level_data_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c(""), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()


# all VARs size 3, 5.5305 mintutes
tictoc::tic()
var_res_2 <- try_sizes_vbls_lags(vec_size = 3, 
                                 vec_lags = c(2,3,4,5),
                                 var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
                                 level_data = level_data_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c(""), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()


# all VARs size 4, 2 choices of lag (3 and 4 seem the best choices for this size for Uruguay), 28.22 min

tictoc::tic()
var_res_3 <- try_sizes_vbls_lags(vec_size = 4, 
                                 vec_lags = c(2,3),
                                 var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
                                 level_data = level_data_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c(""), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()

# From var_res_3 it becomes clear that the best vars of size 4 are very diverse. So we have a dillemma. 
# i solved it with intrdocing 3 new rounds and using 3 different pre chosen variables, one in each round
# the variables are chosen based on their frequency: ri, imp_intermediate and m2

# 2.720333 min

### None of these rounds seem to improve the models in models_and_accu_3
# tictoc::tic()
# var_res_4 <- try_sizes_vbls_lags(vec_size = 4, 
#                                  vec_lags = c(1, 4),
#                                  var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
#                                  level_data = level_data_ts, 
#                                  target_v = target_rgdp,
#                                  pre_selected_v = c("ri"), 
#                                  is_cv = TRUE,
#                                  training_length = train_span,
#                                  h_max = fc_horizon, n_cv = number_of_cv,
#                                  bt_factor = 0, maxlag_ccm = 8,
#                                  return_cv = ret_cv)
# 
# tictoc::toc()
# 
# var_res_5 <- try_sizes_vbls_lags(vec_size = 4, 
#                                  vec_lags = c(1, 4),
#                                  var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
#                                  level_data = level_data_ts, 
#                                  target_v = target_rgdp,
#                                  pre_selected_v = c("imp_intermediate"), 
#                                  is_cv = TRUE,
#                                  training_length = train_span,
#                                  h_max = fc_horizon, n_cv = number_of_cv,
#                                  bt_factor = 0, maxlag_ccm = 8,
#                                  return_cv = ret_cv)
# 
# tictoc::toc()
# 
# var_res_6 <- try_sizes_vbls_lags(vec_size = 4, 
#                                  vec_lags = c(1, 4),
#                                  var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
#                                  level_data = level_data_ts, 
#                                  target_v = target_rgdp,
#                                  pre_selected_v = c("m2"), 
#                                  is_cv = TRUE,
#                                  training_length = train_span,
#                                  h_max = fc_horizon, n_cv = number_of_cv,
#                                  bt_factor = 0, maxlag_ccm = 8,
#                                  return_cv = ret_cv)
# 
# tictoc::toc()


# plus 1 pre_chosen (serv), size 5, 1(3 lags) lag 11.4775 min

# i choose 1 pre_chosen based on the top models i have seen before

tictoc::tic()
var_res_4 <- try_sizes_vbls_lags(vec_size = 5, 
                                 vec_lags = c(2,3),
                                 var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
                                 level_data = level_data_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("imp_intermediate"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()

tictoc::tic()
var_res_5 <- try_sizes_vbls_lags(vec_size = 5, 
                                 vec_lags = c(2),
                                 var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
                                 level_data = level_data_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("ri"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()

tictoc::tic()
var_res_6 <- try_sizes_vbls_lags(vec_size = 5, 
                                 vec_lags = c(2),
                                 var_data = diff_yoy_data_ts, yoy_data = yoy_data_ts,
                                 level_data = level_data_ts, 
                                 target_v = target_rgdp,
                                 pre_selected_v = c("manuf"), 
                                 is_cv = TRUE,
                                 training_length = train_span,
                                 h_max = fc_horizon, n_cv = number_of_cv,
                                 bt_factor = 0, maxlag_ccm = 8,
                                 return_cv = ret_cv)

tictoc::toc()

models_and_accu_1 <- var_res_1[["accu_rankings_models"]] 
models_and_accu_2 <- var_res_2[["accu_rankings_models"]]
models_and_accu_3 <- var_res_3[["accu_rankings_models"]]
models_and_accu_4 <- var_res_4[["accu_rankings_models"]]
models_and_accu_5 <- var_res_5[["accu_rankings_models"]]
models_and_accu_6 <- var_res_6[["accu_rankings_models"]]


models_and_accu_12345 <- rbind(models_and_accu_1, models_and_accu_2, models_and_accu_3, models_and_accu_4, models_and_accu_5,
                               models_and_accu_6) %>% 
  mutate(accu_lev = unlist(accu_lev),
         accu_yoy = unlist(accu_yoy)) %>% 
  dplyr::select(-c(diff_ranking, yoy_ranking, level_ranking)) %>% 
  arrange(accu_yoy) %>% 
  mutate(yoy_ranking = 1:n())


# models_and_accu_1 <- var_res_1[["accu_rankings_models"]]

saveRDS(models_and_accu_12345, "./data/Chile_by_step_12345.rds")

cv_objects_1 <- var_res_1[["cv_objects"]] 
cv_objects_2 <- var_res_2[["cv_objects"]]
cv_objects_3 <- var_res_3[["cv_objects"]]
cv_objects_4 <- var_res_4[["cv_objects"]]
cv_objects_5 <- var_res_5[["cv_objects"]]
cv_objects_6 <- var_res_6[["cv_objects"]]

cv_objects_12345 <- rbind(cv_objects_1, cv_objects_2, cv_objects_3, cv_objects_4, cv_objects_5, 
                          cv_objects_6) %>% 
  dplyr::select(-c(cv_test_data, cv_fcs))


saveRDS(cv_objects_12345, "./data/Chile_by_step_12345_cv_objects.rds")



#

# rm(var_res)
# 
# 
# yoy_data_ts_rgdp <- yoy_data_ts[, "rgdp"]
# n_yoy <- length(yoy_data_ts_rgdp)
# last_yoy_rgdp <- subset(yoy_data_ts_rgdp, start = n_yoy, end = n_yoy)
# last_yoy_rgdp
# 
# level_data_ts_rgdp <- level_data_ts[, "rgdp"]
# n_level <- length(level_data_ts_rgdp)
# last_level_rgdp <- subset(level_data_ts_rgdp, start = n_level-3, end = n_level)
# last_level_rgdp 
# 
# 
# tictoc::tic()
# est_and_fcs <- var_fc_from_best(rank_tibble = models_and_accu,
#                                 VAR_data = diff_yoy_data_ts, 
#                                 levQ = yoy_data_ts, custom_h = 7) %>% 
#   dplyr::select(-c(accu_diff_yoy, accu_yoy, accu_lev)) %>% 
#   mutate(fc_rgdp_mean_yoy = map(fc_rgdp_mean, 
#                                 function(x) un_diff_ts(last_yoy_rgdp, x)),
#          fc_rgdp_mean_level = map(fc_rgdp_mean_yoy, 
#                                   function(x) un_yoy_ts(last_level_rgdp, x)) 
#   )
# tictoc::toc()
# 
# 
# 
# fcs_r <- est_and_fcs$fc_rgdp_mean
# one_fc_diff <- fcs_r[[1]] 
# one_fc_diff
# 
# fcs_r_yoy <- est_and_fcs$fc_rgdp_mean_yoy
# one_fc_yoy <- fcs_r_yoy[[1]] 
# one_fc_yoy
# 
# fcs_r_level <- est_and_fcs$fc_rgdp_mean_level
# one_fc_level <- fcs_r_level[[1]] 
# one_fc_level
# 
# rgdp_diff_yoy_ts <- diff_yoy_data_ts[, "rgdp"]  
# rgdp_yoy_ts <- yoy_data_ts[, "rgdp"]  
# rgdp_level_ts <- level_data_ts[, "rgdp"]  
# 
# 
# plot_fcs_lev_yoy_diff <- function(lev_data, yoy_data, diff_yoy_data, lev_fc, 
#                                   yoy_fc, diff_yoy_fc) {
#   
#   
#   lev_variable <- lev_data[, "rgdp"]
#   yoy_variable <- yoy_data[, "rgdp"]
#   diff_yoy_variable <- diff_yoy_data[, "rgdp"]
#   
#   to_plot_lev <- tk_tbl(ts.union(lev_variable, lev_fc))
#   to_plot_yoy <- tk_tbl(ts.union(yoy_variable, yoy_fc))
#   to_plot_diff_yoy <- tk_tbl(ts.union(diff_yoy_variable, diff_yoy_fc))
#   
#   # lev_p <- ggplot(data = to_plot_lev, aes(x = index, y = lev_variable)) +
#   #   geom_line() +
#   #   scale_x_yearqtr()
#   
#   lev_p <- autoplot(lev_variable) +
#     autolayer(lev_fc, series = "(log) level") + 
#     ylab("log-level of real GDP")
#   
#   
#   yoy_p <- autoplot(yoy_variable) +
#     autolayer(yoy_fc, series = "YoY")  + 
#     ylab("YoY change")
#   
#   diff_yoy_p <- autoplot(diff_yoy_variable) +
#     autolayer(diff_yoy_fc, series = "diff YoY")   + 
#     ylab("diff of YoY changes")
#   
#   # all_p <- grid.arrange(lev_p, yoy_p, diff_yoy_p, ncol = 1)
#   all_p <- arrangeGrob(lev_p, yoy_p, diff_yoy_p, ncol = 1)
#   
#   
#   return(list(lev_p, yoy_p, diff_yoy_p, all_p))
#   
# }
# 
# foo <- plot_fcs_lev_yoy_diff(lev_data = level_data_ts, yoy_data = yoy_data_ts,
#                              diff_yoy_data = diff_yoy_data_ts, 
#                              lev_fc = one_fc_level,
#                              yoy_fc = one_fc_yoy, diff_yoy_fc = one_fc_diff)
# 
# walk(foo, print)
# 
# grid.draw(foo[[4]])
# 
# 
# # one_fc_yoy <- un_diff_ts(last_yoy_rgdp, one_fc_diff)
# # one_fc_yoy
# # 
# # one_fc_level <- un_yoy_ts(last_level_rgdp, one_fc_yoy)
# # one_fc_level
# # 
# # level_foo <- subset(level_data_ts_rgdp, start = n_level - 10, end = n_level)
# # level_foo
# # 
# # yoy_foo <- diff(level_foo, lag = 4, na.pad = FALSE)
# # yoy_foo
# # 
# # diff_yoy_foo <- diff(yoy_foo, lag = 1, na.pad = FALSE)
# # diff_yoy_foo
# # 
# # # supongamos 2016 q4 es la ultima obs y que 2017 q1 q2 q3 son fcs
# # level_foo
# # level_foo[8]
# # 
# # yoy_foo
# # yoy_foo[4]
# # 
# # diff_yoy_foo
# # diff_yoy_foo[3]
# # 
# # verify_un_diff <- un_diff(yoy_foo[4], diff_yoy_foo[4:6])
# # verify_un_diff
# # yoy_foo[5:7]
# # 
# # verify_un_yoy <- un_yoy(level_foo[5:8], verify_un_diff)
# # verify_un_yoy
# # level_foo[9:11]
# # 
# # this_year <- as.integer(floor(time(diff_yoy_foo)))
# # this_quarter <- as.integer(4*(time(diff_yoy_foo) - floor(time(diff_yoy_foo)) + 0.25))
# # this_year  
# # this_quarter
# # 
# # # level_foo[5] - level_foo[1]
# # # level_foo[11] - level_foo[7]
# # 
# # 
# # # cv_objects <- var_res %>% select(cv_vbl_names, cv_lag, cv_errors, cv_test_data,
# # #                                  cv_fcs) %>% 
# # #   rename(variables = cv_vbl_names, lags = cv_lag)
# # # 
# # # rmse_and_ranking <- var_res %>% 
# # #   select(cv_vbl_names, cv_lag, accu_diff_yoy, accu_yoy, accu_lev,
# # #          diff_ranking, yoy_ranking, level_ranking) %>% 
# # #   rename(variables = cv_vbl_names, lags = cv_lag)
# # 
# #   
