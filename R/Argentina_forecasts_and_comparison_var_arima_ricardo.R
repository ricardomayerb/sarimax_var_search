# source('./R/utils_av_older_version.R')
# source('./R/utils_av.R')
source('./R/utils_av_ricardo.R')
library(ggthemes)
library(ggThemeAssist)
library(RColorBrewer)
library(jpeg)
library(imager)

# Our own themes: to be moved to our own theme package
## Make our own theme
theme_cepal_fcs_line_graph <- theme_grey() +  
  theme(axis.text.x = element_text(angle = 25,  vjust = 0.5, hjust = 0.5)) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.title = element_text(face = "bold", 
                                  colour = "royalblue4"), axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(face = "bold", 
                                  colour = "royalblue4", hjust = 0.5, 
                                  vjust = 0), legend.text = element_text(face = "italic", 
                                                                         colour = "royalblue4"), 
        legend.title = element_text(face = "bold", colour = "royalblue4"), panel.background = element_rect(fill = "aliceblue"), 
        plot.background = element_rect(fill = "lightsteelblue2", colour = NA), legend.background = element_rect(fill = "aliceblue"), 
        legend.position = "bottom", legend.direction = "horizontal") + theme(panel.grid.major = element_line(linetype = "blank"), 
                                                                             panel.grid.minor = element_line(linetype = "blank")) + theme(axis.text.x = element_text(colour = "royalblue4"), 
                                                                                                                                          axis.text.y = element_text(colour = "royalblue4")) + theme(panel.grid.minor = element_line(linetype = "dashed")) + 
  theme(legend.text = element_text(size = 7))

theme_cepal_fcs_bar_plot <- theme_grey() +  
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.title = element_text(face = "bold", 
                                  colour = "royalblue4"), axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        plot.title = element_text(face = "bold", 
                                  colour = "royalblue4", hjust = 0.5, 
                                  vjust = 0), legend.text = element_text(face = "italic", 
                                                                         colour = "royalblue4"), 
        legend.title = element_text(face = "bold", colour = "royalblue4"), panel.background = element_rect(fill = "aliceblue"), 
        plot.background = element_rect(fill = "lightsteelblue2", colour = NA), legend.background = element_rect(fill = "aliceblue"), 
        legend.position = "bottom", legend.direction = "horizontal") + theme(panel.grid.major = element_line(linetype = "blank"), 
                                                                             panel.grid.minor = element_line(linetype = "blank")) + theme(axis.text.x = element_text(colour = "royalblue4"), 
                                                                                                                                          axis.text.y = element_text(colour = "royalblue4")) + theme(panel.grid.minor = element_line(linetype = "dashed")) + 
  theme(legend.text = element_text(size = 7))

# Create the CEPAL watermark
img <- jpeg::readJPEG("./watermark.jpg")
watermark_cepal <- matrix(rgb(img[,,1],img[,,2],img[,,3], img[,,3.5] * 0.4), nrow=dim(img)[1])

# Start of the Script

country_name <- "Argentina"

# Optional: Estimate (and Save) new Arimax RDS file



# # To run (and save) the arima script

set_manual_h <- TRUE
# final_forecast_horizon <- c(2019, 12)
h_max <- 8
h_max_arima <- h_max

arima_res_suffix <- "_dm_s"
use_demetra <- TRUE
use_dm_force_constant <- TRUE

# arima_res <- get_arima_results(
#   country_name = country_name, use_dm_force_constant = use_dm_force_constant,
#   arima_res_suffix = arima_res_suffix, use_demetra = use_demetra,
#   h_max = h_max, set_manual_h = set_manual_h)


# # Or, just load previously saved arima res objects
arima_res <- get_arima_results(country_name = country_name, read_results = TRUE,
  arima_res_suffix = arima_res_suffix)

extended_x_data_ts <- arima_res$mdata_ext_ts
rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
do.force.constant <- TRUE


# var_countries_and_gdp_transform <- tibble(
#   country = c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Ecuador",
#               "Mexico", "Paraguay", "Peru", "Uruguay"),
#   transformation = c("diff_yoy", "diff_yoy", "yoy", "yoy", "yoy",
#                      "diff", "yoy", "diff_yoy", "yoy", "diff_yoy"))
# 
# this_country_gdp_transform <- subset(x = var_countries_and_gdp_transform,
#                                      subset = country == country_name, 
#                                      select = transformation)$transformation


# Always check if this is true or false
arima_rgdp_is_log <- TRUE

if(arima_rgdp_is_log) {
  rgdp_level_ts <- exp(rgdp_ts_in_arima )
  
} else {
  rgdp_level_ts <- rgdp_ts_in_arima 
}




########################## IMPORT ARIMAX AND VAR FILES #############################
# Automatically imports the data matching the country_name

path_models_and_accu <- paste("./data/", country_name, "_by_step_12345.rds", sep = "")
path_cv_objects <- paste("./data/", country_name, "_by_step_12345_cv_objects.rds", sep = "")
path_VAR_data <- paste("./data/VAR_data_", country_name, ".rds", sep = "")
models_and_accu <- readRDS(path_models_and_accu)
cv_objects <- readRDS(path_cv_objects)
VAR_data <- readRDS(path_VAR_data)
VAR_data_rgdp <- VAR_data[, "rgdp"]
rgdp_yoy_VAR_timespan <-  window(make_yoy_ts(rgdp_level_ts), 
                                 start = start(VAR_data_rgdp),
                                 end = end(VAR_data_rgdp))

rgdp_yoy_VAR_timespan
# # check var data
# VAR_data_rgdp
# 
# 
# #lets diff yoy our level data
# arima_yoy_rgdp <- rgdp_level_ts %>% make_yoy_ts()
# arima_diff_yoy_rgdp <- arima_yoy_rgdp %>% diff()
# arima_diff_rgdp <- rgdp_level_ts %>% diff()



# # put them together
# ts.union(arima_diff_yoy_rgdp, VAR_data_rgdp)


# NEW: for negatively correlated models: add an id variable to the cv_objects and models accu of the var models
cv_objects <- cv_objects %>% mutate(id = 1:n())
models_and_accu <- models_and_accu %>% mutate(id = 1:n())

h_max_var <- 8

# Get the table with all models from both the VARs and ARIMAX
# I adjusted the make_model_tbl function in utils_av: each_h_just_model_and_ave_rmse_sarimax is adjusted so it selects the id variable
models_tbl <- make_models_tbl(
  arima_res = arima_res, var_models_and_rmse = models_and_accu, 
  VAR_data = VAR_data, h_max = h_max)



# ssel stands for "stata_selection" and what it does it to imitate stata-style selection 
models_tbl_ssel <- make_models_tbl(
  arima_res, var_models_and_rmse = models_and_accu, VAR_data = VAR_data,
  h_max = h_max, ave_rmse_sel = TRUE)



################################### In Sample Accuracy Comparison ################################

# I feel like, before going into the out of sample performance (i.e the forecasts), we should come up
# with a nice way of presenting the in sample performance of the individual models and the combined models. 
# We should produce a nice table and graphs (maybe the in sample forecasts in levels vis-a-vis realized level values) 
# that show the rmse's (from the cross-validation exercise) of the combined and individual models at each h. 
# Once we have a good idea about the VAR and ARIMAX in sample performance 
# it is time to look at the out of sample performance, i.e. the actual forecasts. 


######################################## Forecasts VARs ##########################################



# First have a look at the VAR models
VAR_fcs_all <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                  h_var = h_max_var, extended_x_data_ts = extended_x_data_ts,
                                  rgdp_ts_in_arima = rgdp_ts_in_arima,
                                  model_type = "VAR", max_rank_h = 30,
                                  var_data = VAR_data)

VAR_all_fit_fc_tbl <- VAR_fcs_all$info_fit_ifcs
VAR_all_wfc_yoy_ts <- VAR_fcs_all$w_fc_yoy_ts

rgdp_yoy_VAR_timespan


VAR_fcs_all_best_10 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                          h_var = h_max_var, extended_x_data_ts = extended_x_data_ts,
                                          rgdp_ts_in_arima = rgdp_ts_in_arima,
                                          model_type = "VAR", max_rank_h = 10,
                                          var_data = VAR_data)

VAR_10_fit_fc_tbl <- VAR_fcs_all_best_10$info_fit_ifcs
VAR_10_wfc_yoy_ts <- VAR_fcs_all_best_10$w_fc_yoy_ts


VAR_fcs_all_best_5 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                         h_var = h_max_var, extended_x_data_ts = extended_x_data_ts,
                                         rgdp_ts_in_arima = rgdp_ts_in_arima,
                                         model_type = "VAR", max_rank_h = 5,
                                         var_data = VAR_data)


VAR_5_fit_fc_tbl <- VAR_fcs_all_best_5$info_fit_ifcs
VAR_5_wfc_yoy_ts <- VAR_fcs_all_best_5$w_fc_yoy_ts






var_training_length <- 25
var_test_length <- h_max_var
n_cv <- 8



cv_of_ensemble <- function(var_training_length, 
                           n_cv, 
                           tbl_of_models_and_rmse, 
                           extended_x_data_ts, 
                           rgdp_ts_in_arima, 
                           var_data, 
                           rgdp_level_ts,
                           max_rank_h = NULL,
                           model_type = NULL, 
                           chosen_rmse_h = NULL,
                           force.constant = FALSE,
                           h_arima = NULL, 
                           h_var = NULL) {
  
  cv_yq_lists <- make_test_dates_list(var_data, n = n_cv, h_max = h_var,
                                      training_length = var_training_length)[["list_of_year_quarter"]]
  
  cv_test_data_list <- list_along(1:n_cv)
  cv_w_fcs_list <- list_along(1:n_cv)
  cv_error_yoy_list <- list_along(1:n_cv)
  
  for (i in 1:n_cv) {
    this_cv_yq_list <- cv_yq_lists[[i]]
    
    this_training_s <- this_cv_yq_list$tra_s
    this_training_e <- this_cv_yq_list$tra_e
    
    this_test_s <- this_cv_yq_list$tes_s
    this_test_e <- this_cv_yq_list$tes_e
    
    fcs_and_models <- indiv_weigthed_fcs_new(tbl_of_models_and_rmse = tbl_of_models_and_rmse,
                                             h_var = var_test_length, 
                                             extended_x_data_ts = extended_x_data_ts,
                                             rgdp_ts_in_arima = rgdp_ts_in_arima,
                                             model_type = model_type, 
                                             max_rank_h = max_rank_h,
                                             var_data = var_data, 
                                             var_start = this_training_s,
                                             var_end = this_training_e)
    
    w_ave_fc_yoy <- fcs_and_models$w_fc_yoy_ts
    
    rgdp_test_yoy_data <- window(make_yoy_ts(rgdp_level_ts, is_log = FALSE),
                                 start = this_test_s, end = this_test_e)
    
    cv_error_yoy <- rgdp_test_yoy_data - w_ave_fc_yoy
    
    cv_test_data_list[[i]] <- rgdp_test_yoy_data
    cv_w_fcs_list[[i]] <- w_ave_fc_yoy
    cv_error_yoy_list[[i]] <- cv_error_yoy

  }
  
  
  mat_cv_error_yoy <- matrix(
    reduce(cv_error_yoy_list, rbind), 
    nrow = n_cv, byrow = TRUE)
  
  ensemble_rmse <-   sqrt(colMeans(mat_cv_error_yoy^2))
  
  ensemble_rmse
  
  return(list(ensemble_rmse = ensemble_rmse,
              cv_error_yoy_list = cv_error_yoy_list,
              cv_test_data_list = cv_test_data_list,
              cv_w_fcs_list = cv_w_fcs_list))
  
}

tic()
cv_ensemble_VAR_5 <- cv_of_ensemble(var_training_length = var_training_length, 
               n_cv = 16,
               tbl_of_models_and_rmse = models_tbl, 
               extended_x_data_ts = extended_x_data_ts, 
               rgdp_ts_in_arima = rgdp_ts_in_arima,
               var_data = VAR_data,
               rgdp_level_ts = rgdp_level_ts,
               max_rank_h = 5,
               model_type = "VAR",
               h_var = h_max_var) 
toc()
rmse_ensemble_VAR_5 <- cv_ensemble_VAR_5$ensemble_rmse
rmse_ensemble_VAR_5


tic()
cv_ensemble_VAR_30 <- cv_of_ensemble(var_training_length = var_training_length, 
                                    n_cv = n_cv,
                                    tbl_of_models_and_rmse = models_tbl, 
                                    extended_x_data_ts = extended_x_data_ts, 
                                    rgdp_ts_in_arima = rgdp_ts_in_arima,
                                    var_data = VAR_data,
                                    rgdp_level_ts = rgdp_level_ts,
                                    max_rank_h = 30,
                                    model_type = "VAR",
                                    h_var = h_max_var) 
toc()
rmse_ensemble_VAR_30 <- cv_ensemble_VAR_30$ensemble_rmse
rmse_ensemble_VAR_30



cv_yq_lists <- make_test_dates_list(VAR_data, n = n_cv, h_max = var_test_length,
                                    training_length = var_training_length)[["list_of_year_quarter"]]

cv_test_data_list <- list_along(1:n_cv)
cv_w_fcs_list <- list_along(1:n_cv)
cv_error_yoy_list <- list_along(1:n_cv)

for (i in 1:n_cv) {
  this_cv_yq_list <- cv_yq_lists[[i]]
  
  this_training_s <- this_cv_yq_list$tra_s
  this_training_e <- this_cv_yq_list$tra_e
  
  this_test_s <- this_cv_yq_list$tes_s
  this_test_e <- this_cv_yq_list$tes_e
  
  fcs_and_models <- indiv_weigthed_fcs_new(tbl_of_models_and_rmse = models_tbl,
                                           h_var = var_test_length, 
                                           extended_x_data_ts = extended_x_data_ts,
                                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                                           model_type = "VAR", max_rank_h = 5,
                                           var_data = VAR_data, 
                                           var_start = this_training_s,
                                           var_end = this_training_e)
  
  w_ave_fc_yoy <- fcs_and_models$w_fc_yoy_ts
  
  rgdp_test_yoy_data <- window(make_yoy_ts(rgdp_level_ts, is_log = FALSE),
                               start = this_test_s, end = this_test_e)
  
  cv_error_yoy <- rgdp_test_yoy_data - w_ave_fc_yoy
  
  cv_test_data_list[[i]] <- rgdp_test_yoy_data
  cv_w_fcs_list[[i]] <- w_ave_fc_yoy
  cv_error_yoy_list[[i]] <- cv_error_yoy
  
  # print(paste("i =", i))
  # print("w_ave_fc_yoy")
  # print(w_ave_fc_yoy)
  # print("rgdp_test_yoy_data")
  # print(rgdp_test_yoy_data)
  # print("cv_error_yoy")
  # print(cv_error_yoy)
  
}


mat_cv_error_yoy <- matrix(
  reduce(cv_error_yoy_list, rbind), 
  nrow = n_cv, byrow = TRUE)

ensemble_rmse <-   sqrt(colMeans(mat_cv_error_yoy^2))

ensemble_rmse


this_cv_step <- 2
this_cv_yq_list <- cv_yq_lists[[this_cv_step]]
this_training_s <- this_cv_yq_list$tra_s
this_training_e <- this_cv_yq_list$tra_e

VAR_fcs_all_best_5_new <- indiv_weigthed_fcs_new(tbl_of_models_and_rmse = models_tbl,
                                         h_var = 6, extended_x_data_ts = extended_x_data_ts,
                                         rgdp_ts_in_arima = rgdp_ts_in_arima,
                                         model_type = "VAR", max_rank_h = 5,
                                         var_data = VAR_data, var_start = this_training_s,
                                         var_end = this_training_e)




VAR_5_fit_fc_tbl_new <- VAR_fcs_all_best_5_new$info_fit_ifcs
VAR_5_wfc_yoy_ts_new <- VAR_fcs_all_best_5_new$w_fc_yoy_ts


rgdp_test_yoy_data <- window(make_yoy_ts(rgdp_level_ts, is_log = FALSE),
                             start = this_cv_yq_list$tes_s, 
                             end = this_cv_yq_list$tes_e)

cv_error_yoy <- rgdp_test_yoy_data - VAR_5_wfc_yoy_ts_new



VAR_fcs_all_best_15 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                          h_var = h_max_var, extended_x_data_ts = extended_x_data_ts,
                                          rgdp_ts_in_arima = rgdp_ts_in_arima,
                                          model_type = "VAR", max_rank_h = 15,
                                          var_data = VAR_data)


VAR_fcs_all_best_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                          h_var =  h_max_var, extended_x_data_ts = extended_x_data_ts,
                                          rgdp_ts_in_arima = rgdp_ts_in_arima,
                                          model_type = "VAR", max_rank_h = 20,
                                          var_data = VAR_data)

rgdp_var <- rgdp_yoy_VAR_timespan
# drop the h = 8, 2020 Q1 forecast
VAR_fcs_all_best_5$w_fc_yoy_ts <- window(VAR_fcs_all_best_5$w_fc_yoy_ts, end = 2019.75)
VAR_fcs_all_best_10$w_fc_yoy_ts <- window(VAR_fcs_all_best_10$w_fc_yoy_ts, end = 2019.75)
VAR_fcs_all_best_15$w_fc_yoy_ts <- window(VAR_fcs_all_best_15$w_fc_yoy_ts, end = 2019.75)
VAR_fcs_all_best_20$w_fc_yoy_ts <- window(VAR_fcs_all_best_20$w_fc_yoy_ts, end = 2019.75)
VAR_fcs_all$w_fc_yoy_ts <- window(VAR_fcs_all$w_fc_yoy_ts, end = 2019.75)

start_ts_fcs_var_year <- year(as.yearqtr(last(time(rgdp_var)))) # use year from lubridate. Time gives you the time like 2017.5 etc
# last gives you the last observation. as.yearqtr turns it into 2017 Q3 form and year gives the year
start_ts_fcs_var_qtr <- quarter(as.yearqtr(last(time(rgdp_var)))) # use quarter from lubridate

fcs_all_VAR_best5_ts <- add_last_obs_to_fcs(VAR_fcs_all_best_5$w_fc_yoy_ts, rgdp_var)

fcs_all_VAR_best10_ts <- add_last_obs_to_fcs(VAR_fcs_all_best_10$w_fc_yoy_ts, rgdp_var)

fcs_all_VAR_best15_ts <- add_last_obs_to_fcs(VAR_fcs_all_best_15$w_fc_yoy_ts, rgdp_var)

fcs_all_VAR_best20_ts <- add_last_obs_to_fcs(VAR_fcs_all_best_20$w_fc_yoy_ts, rgdp_var)

fcs_all_VAR_ts <- add_last_obs_to_fcs(VAR_fcs_all$w_fc_yoy_ts, rgdp_var)

rgdp_var_and_fcs <- ts.union(rgdp_var, fcs_all_VAR_best5_ts, fcs_all_VAR_best10_ts, 
                             fcs_all_VAR_best15_ts, fcs_all_VAR_best20_ts,
                             fcs_all_VAR_ts)

plot_vars_title <- paste("GDP Forecasts Best VARs", country_name, sep = " ")
filename_var_plot <- paste(plot_vars_title, "png", sep = ".")
# directory_var_plot <- paste("./Plots", filename_var_plot, sep = "/")
plots_path = paste("./Plots/", country_name, "/", sep = "")

forecast_plot_best_vars <- autoplot(rgdp_var) + 
  autolayer(fcs_all_VAR_best5_ts, series="Best 5 VARs", linetype = 2, size = 1) +
  autolayer(fcs_all_VAR_best10_ts, series="Best 10 VARs", linetype = 3, size = 1) +
  autolayer(fcs_all_VAR_best15_ts, series="Best 15 VARs", linetype = 4, size = 1) +
  autolayer(fcs_all_VAR_best20_ts, series="Best 20 VARs", linetype = 5, size = 1) +
  autolayer(fcs_all_VAR_ts, series="Best 30 VARs", linetype = 6, size = 1) +
  scale_colour_brewer(palette = "Set2") +
  ggtitle(plot_vars_title) +
  guides(colour=guide_legend(title="Models:")) +
  theme_cepal_fcs_line_graph +
  scale_y_continuous(name = "Real GDP Growth YoY (%)", 
                     breaks=c(-0.03, -0.02, -0.01, 0.00, 0.01, 0.02, 0.03, 0.04, 
                              0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) + 
  scale_x_yearmon(name = "Year", n = 15, format = "%b  %Y") +
  annotation_custom(ymin= -0.04, ymax= -0.01, xmin = 2018.00, xmax = 2020.00, rasterGrob(watermark_cepal))

ggsave(filename = filename_var_plot, path = plots_path)

forecast_plot_best_vars

############################ Forecasts Arimax ##############################
# Second, have a look at the ARIMAX models
# Transform rgdp series from log level to yoy
rgdp_arimax <- make_yoy_ts(exp(rgdp_ts_in_arima))

# arimax_fcs_all <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
#                                   h = h_max_var, extended_x_data_ts = extended_x_data_ts,
#                                   rgdp_ts_in_arima = rgdp_ts_in_arima,
#                                   model_type = "Arima")


arimax_fcs_all <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                     h_arima = h_max_arima, h_var = h_max_var,
                                     extended_x_data_ts = extended_x_data_ts,
                                     rgdp_ts_in_arima = rgdp_ts_in_arima,
                                     model_type = "Arima", 
                                     force.constant = do.force.constant)

# summ_arimax_fcs_all <- arimax_fcs_all %>%
#   group_by(horizon) %>%
#   summarise(sum_one_h = reduce(one_model_w_fc, sum))
# 
# arimax_fcs_all_and_data_as_yoy <- diffyoy_2_yoy_data_and_fc(summ_tbl = summ_arimax_fcs_all, var_data = VAR_data,
#                                                          rgdp_level_ts = rgdp_level_ts)
# 
# rgdp_arimax <- arimax_fcs_all_and_data_as_yoy$yoy_rgdp_data
# arimax_fcs_all_yoy <- arimax_fcs_all_and_data_as_yoy$yoy_rgdp_fc
# summ_arimax_fcs_all$sum_one_h <- arimax_fcs_all_yoy 
# summ_arimax_fcs_all$sum_one_h <- as.numeric(summ_arimax_fcs_all$sum_one_h)



arimax_fcs_all_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                          h_arima = h_max_arima, h_var = h_max_var,
                                          extended_x_data_ts = extended_x_data_ts,
                                          rgdp_ts_in_arima = rgdp_ts_in_arima,
                                          model_type = "Arima", force.constant = do.force.constant)

# summ_arimax_fcs_all_ssel <- arimax_fcs_all_ssel %>% 
#   group_by(horizon) %>%
#   summarise(sum_one_h = reduce(one_model_w_fc, sum))

# arimax_fcs_all_ssel_and_data_as_yoy <- diffyoy_2_yoy_data_and_fc(summ_tbl = summ_arimax_fcs_all_ssel, var_data = VAR_data,
#                                                             rgdp_level_ts = rgdp_level_ts)

# arimax_fcs_all_ssel_yoy <- arimax_fcs_all_ssel_and_data_as_yoy$yoy_rgdp_fc
# summ_arimax_fcs_all_ssel$sum_one_h <- arimax_fcs_all_ssel_yoy
# summ_arimax_fcs_all_ssel$sum_one_h <- as.numeric(summ_arimax_fcs_all_ssel$sum_one_h)

# select similar start date for rgdp_arimax as for rgdp_var so that graph is comparable
# first_year_var_ts is used to set the start of arimax series equal to the var series. Besides that we do not use it
# first_year_var_ts <- year(as.yearqtr(first(time(rgdp_var))))
# first_qtr_var_ts <- quarter(as.yearqtr(first(time(rgdp_var))))
# rgdp_arimax <- window(rgdp_arimax, start = c(first_year_var_ts, first_qtr_var_ts))

# start_ts_fcs_arimax_year <- year(as.yearqtr(last(time(rgdp_arimax)))) 
# start_ts_fcs_arimax_qtr <- quarter(as.yearqtr(last(time(rgdp_arimax))))

# summ_arimax_fcs_all <- add_row_fcs_summary(summary_model = summ_arimax_fcs_all, rgdp_ts = rgdp_arimax)
# summ_arimax_fcs_all_ssel <- add_row_fcs_summary(summary_model = summ_arimax_fcs_all_ssel, rgdp_ts = rgdp_arimax)
# 
# fcs_all_arimax_ts <- turn_summary_in_ts(summary_model = summ_arimax_fcs_all, 
#                                        start_year_ts = start_ts_fcs_var_year, 
#                                        start_qtr_ts = start_ts_fcs_var_qtr)
# 
# fcs_all_arimax_ssel_ts <- turn_summary_in_ts(summary_model = summ_arimax_fcs_all_ssel, 
#                                         start_year_ts = start_ts_fcs_var_year, 
#                                         start_qtr_ts = start_ts_fcs_var_qtr)

arimax_fcs_all$w_fc_yoy_ts <- window(arimax_fcs_all$w_fc_yoy_ts, end = 2019.75)

# fcs_all_ssel_arimax_ts$w_fc_yoy_ts <- window(fcs_all_ssel_arimax_ts$w_fc_yoy_ts, end = 2019.75)
arimax_fcs_all_ssel$w_fc_yoy_ts <- window(arimax_fcs_all_ssel$w_fc_yoy_ts, end = 2019.75)

fcs_all_arimax_ts <- add_last_obs_to_fcs(arimax_fcs_all$w_fc_yoy_ts, rgdp_var)
fcs_all_ssel_arimax_ts <- add_last_obs_to_fcs(arimax_fcs_all_ssel$w_fc_yoy_ts, rgdp_var)


rgdp_var_and_fcs <- ts.union(rgdp_var, fcs_all_arimax_ts, fcs_all_ssel_arimax_ts)

# Two ways to combine the time-series. 1) Use autolayer, which adds a layer to an existing plot, or 2) ts.union the time-series and then plot them
plot_arimax_title <- paste("GDP Forecasts Best ARIMAX", country_name, sep = " ")
filename_arimax_plot <- paste(plot_arimax_title, "png", sep = ".")

# To turn a ts object into a date frame using tk_tbl
rgdp_var_and_fcs_df <- fcs_all_arimax_ts %>% tk_tbl() %>% as.data.frame()

forecast_plot_best_arimax <- autoplot(rgdp_var) + 
  autolayer(fcs_all_arimax_ts, series="Best 30 ARIMAX", linetype = 2, size = 1) +
  autolayer(fcs_all_ssel_arimax_ts, series="Best 30 ARIMAX Stata Selection", linetype = 3, size = 1) +
  ggtitle(plot_arimax_title) +
  scale_colour_brewer(palette = "Set2") +
  guides(colour=guide_legend(title="Models:")) +
  theme_cepal_fcs_line_graph +
  scale_y_continuous(name = "Real GDP Growth YoY (%)", 
                     breaks=c(-0.03, -0.02, -0.01, 0.00, 0.01, 0.02, 0.03, 0.04, 
                              0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) + 
  scale_x_yearmon(name = "Year", n = 15, format = "%b  %Y") +
  annotation_custom(ymin= -0.04, ymax= -0.01, xmin = 2018.00, xmax = 2020.00, rasterGrob(watermark_cepal))

ggsave(filename = filename_arimax_plot, path = plots_path)

forecast_plot_best_arimax

rgdp_arimax_and_fcs <- ts.union(rgdp_arimax, fcs_all_arimax_ts, fcs_all_ssel_arimax_ts)
# rgdp_arimax_and_fcs <- ts.union(rgdp_arimax, fcs_all_arimax_ts, fcs_best_20_arimax_ts, fcs_all_arimax_ssel_ts, fcs_best_20_arimax_ssel_ts)
rgdp_var_and_arimax_fcs <- ts.union(rgdp_var_and_fcs, rgdp_arimax_and_fcs)

plot_vars_arimax_title <- paste("GDP Forecasts Best VARs and ARIMAX", country_name, sep = " ")
filename_vars_arimax_plot <- paste(plot_vars_arimax_title, "png", sep = ".")

# png(filename = directory_var_arimax_plot)

#find a way to make this generic for all countries. Use paste function. "GDP Forecast Best VARs and ARIMAX: Chile"
forecast_plot_best_vars_arimax <- autoplot(rgdp_var) + 
  autolayer(fcs_all_arimax_ts, series="Best 30 ARIMAX", linetype = 2, size = 1) + 
  autolayer(fcs_all_ssel_arimax_ts, series="Best 30 ARIMAX Stata Selection", linetype = 3, size = 1) +
  autolayer(fcs_all_VAR_best20_ts, series="Forecasts Best 20 VARs", linetype = 4, size = 1) +
  autolayer(fcs_all_VAR_best5_ts, series="Forecasts Best 5 VARs", linetype = 5, size = 1) +
  ggtitle(plot_vars_arimax_title) +
  scale_colour_brewer(palette = "Set2") +
  guides(colour=guide_legend(title="Models:")) +
  theme_cepal_fcs_line_graph +
  scale_y_continuous(name = "Real GDP Growth YoY (%)", 
                     breaks=c(-0.03, -0.02, -0.01, 0.00, 0.01, 0.02, 0.03, 0.04, 
                              0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) + 
  scale_x_yearmon(name = "Year", n = 15, format = "%b  %Y") +
  annotation_custom(ymin= -0.04, ymax= -0.01, xmin = 2018.00, xmax = 2020.00, rasterGrob(watermark_cepal))


ggsave(filename = filename_vars_arimax_plot, path = plots_path)

print(forecast_plot_best_vars_arimax)
##################################### FORECASTS OF VARs AND ARIMAX COMBINED ##################################


# Get the best Forecasts out of all Models: all VARs and all ARIMAX
comb_fcs_all <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                   h_arima = h_max_arima, h_var = h_max_var,
                                   extended_x_data_ts = extended_x_data_ts,
                                   rgdp_ts_in_arima = rgdp_ts_in_arima,
                                   max_rank_h = 30, force.constant = do.force.constant,
                                   var_data = VAR_data)

comb_fcs_all$info_fit_ifcs

number_of_models_per_h <- comb_fcs_all$info_fit_ifcs %>% group_by(horizon) %>% 
  summarise(n_models = n(), 
            n_VAR = sum(model_function == "VAR"),
            n_ARIMA = sum(model_function == "Arima")
  )

number_of_models_per_h

# Ask Ricardo How to save these kind of plots? ggsave doesnt seem to work
# plot_rmse_all_h_title <- paste("RMSE at each hoirizon (h):", country_name, sep = " ")
# filename_rmse_all_h_plot <- paste(plot_rmse_all_h_title, "png", sep = ".")

# rast <- grid::rasterGrob(w, interpolate = T)

plot_rmse_all_h_title <- paste("RMSE at each hoirizon (h)", country_name, sep = " ")
filename_rmse_all_h_plot <- paste(plot_rmse_all_h_title, "png", sep = ".")

# the breaks are going to be different for each country, we have to do that manually
rmse_plot_all_h <- single_plot_rmse_all_h(comb_fcs_all$info_fit_ifcs) + 
  scale_y_continuous(name = "RMSE", 
                     breaks=c(0.0000, 0.0015, 0.0030, 0.0045, 0.0060, 0.0075,
                              0.0090, 0.0105, 0.0120, 0.0135)) + 
  theme(axis.title = element_text(colour = "royalblue4"), 
        axis.text = element_text(colour = "royalblue4"), 
        panel.background = element_rect(colour="aliceblue", fill = "aliceblue"), 
        plot.background = element_rect(colour="lightsteelblue2", fill = "lightsteelblue2"),
        legend.background = element_rect(colour="aliceblue", fill = "aliceblue")) +
  annotation_custom(xmin=210, xmax=240, ymin=-0.0025, ymax=0.0040, rasterGrob(watermark_cepal))

ggsave(filename = filename_rmse_all_h_plot, path = plots_path)

print(rmse_plot_all_h)

# path_watermark_cepal <- paste("./Watermark/watermark_cepal", ".jpeg", sep = "")
# m <- readJPEG("watermark_cepal.jpeg", native = FALSE)

facet_plot_rmse_all_h_title <- paste("Facet Plot RMSE at each horizon (h)", country_name, sep = " ")
filename_facet_rmse_all_h_plot <- paste(facet_plot_rmse_all_h_title, "png", sep = ".")

facet_rmse_plot_all_h <- facet_rmse_all_h(comb_fcs_all$info_fit_ifcs) + 
  scale_y_continuous(name = "RMSE", breaks=c(0.000, 0.0025, 0.005, 0.0075, 0.010, 0.0125))

facet_rmse_plot_all_h <- facet_rmse_plot_all_h + 
  theme(strip.text.x = element_text(size=8, face="bold", colour = "white"),
        strip.background = element_rect(colour="black", fill="cadetblue4")) +
  theme(axis.title = element_text(colour = "royalblue4", face = "bold"), 
        axis.text = element_text(colour = "royalblue4"), 
        panel.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "lightsteelblue2"), 
        legend.background = element_rect(fill = "aliceblue")) +
  annotation_custom(xmin=15, xmax=43, ymin=0.000, ymax=0.0030, rasterGrob(watermark_cepal))

# theme(axis.title = element_text(face = "bold")) + 
#   theme(axis.text = element_text(face = "bold"))
# annotate("text", x = 210 + 15, y = 1.1*max_rmse, label = "h = 8", fontface = "bold", colour = "royalblue4")

ggsave(filename = filename_facet_rmse_all_h_plot, path = plots_path)

print(facet_rmse_plot_all_h)

# Next step is this graph but also with the combined models and see how they perform (both VAR and ARIMAX)
rmse_plots_list <- list_of_rmse_plots(comb_fcs_all$info_fit_ifcs)
walk(rmse_plots_list, print)

filenames_h1_h8 <- c("Forecast Horizon h = 1", "Forecast Horizon h = 2", "Forecast Horizon h = 3", "Forecast Horizon h = 4",
                     "Forecast Horizon h = 5", "Forecast Horizon h = 6", "Forecast Horizon h = 7", "Forecast Horizon h = 8")

walk2(.x = rmse_plots_list, .y = filenames_h1_h8, 
      ~ ggsave(filename = paste0(.y, ".png"), plot = .x, path = plots_path))

rank_arima <- models_tbl_ssel %>% dplyr::filter(model_function == "Arima") %>% select(rmse_h, variables, short_name, rmse, rank_h)

# Average Forecasts of all models (all models are the best 30 at each h)
# summ_comb_fcs_all <- comb_fcs_all$info_fit_ifcs %>% 
#   group_by(horizon) %>%
#   summarise(sum_one_h = reduce(one_model_w_fc, sum))

comb_fcs_all_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                        h_arima = h_max_arima, h_var = h_max_var,
                                        extended_x_data_ts = extended_x_data_ts,
                                        rgdp_ts_in_arima = rgdp_ts_in_arima,
                                        max_rank_h = 30, force.constant = do.force.constant,
                                        var_data = VAR_data)

number_of_models_per_h_ssel <- comb_fcs_all_ssel$info_fit_ifcs %>% group_by(horizon) %>% 
  summarise(n_models = n(), 
            n_VAR = sum(model_function == "VAR"),
            n_ARIMA = sum(model_function == "Arima")
  )

number_of_models_per_h_ssel

# for consistency with stata i focus on ssel method

comb_fcs_ssel_best_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                            h_arima = h_max_arima, h_var = h_max_var,
                                            extended_x_data_ts = extended_x_data_ts,
                                            rgdp_ts_in_arima = rgdp_ts_in_arima,
                                            max_rank_h = 20, force.constant = do.force.constant,
                                            var_data = VAR_data)
# drop the h = 8 forecast, drop the 2020 Q1 forecast
comb_fcs_all_ssel$w_fc_yoy_ts <- window(comb_fcs_all_ssel$w_fc_yoy_ts, end = 2019.75)
comb_fcs_ssel_best_20$w_fc_yoy_ts <- window(comb_fcs_ssel_best_20$w_fc_yoy_ts, end = 2019.75)
# comb_fcs_ssel_best_20$w_fc_yoy_ts
fcs_combined_models <- add_last_obs_to_fcs(comb_fcs_all_ssel$w_fc_yoy_ts, rgdp_var)
fcs_combined_models_best_20 <- add_last_obs_to_fcs(comb_fcs_ssel_best_20$w_fc_yoy_ts, rgdp_var)

# fcs_combined_models <- turn_summary_in_ts(summary_model = summ_comb_fcs_all_ssel, 
#                                         start_year_ts = start_ts_fcs_var_year, 
#                                         start_qtr_ts = start_ts_fcs_var_qtr)
# 
# fcs_combined_models_best_20 <- turn_summary_in_ts(summary_model = summ_comb_fcs_ssel_best_20, 
#                                           start_year_ts = start_ts_fcs_var_year, 
#                                           start_qtr_ts = start_ts_fcs_var_qtr)


# summ_all_20 add an extra row with horizon = 0 and sum_one_h is the last observation of rgdp_var
# summ_comb_fcs_all_ssel <- summ_comb_fcs_all_ssel %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)
# summ_comb_fcs_ssel_best_20  <- summ_comb_fcs_ssel_best_20  %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)


# note that i usee the arimax summary now, i should replace that for VAR
# fcs_combined_models <- ts(data = summ_comb_fcs_all_ssel$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))
# fcs_combined_models_best_20 <- ts(data = summ_comb_fcs_ssel_best_20$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))
# rgdp_and_combined_fcs <- ts.union(rgdp_var, fcs_var_ts_best5, fcs_var_ts_best10, fcs_var_ts_best15, fcs_var_ts_best20)

# IMPORTANT: How can we combine the arimax and var models and their negative models. Because CV objects only contains the erros for the VARs
# What is the cv_objects equivalent for the arimax models?
# We have to insert here the way of combining negatively correlated models with the best models
# then we can graph how this way of combining models performs
# So for now this is only for the VAR models: Let's take the top 10 and find negative correlated models from the best 30
# VAR_fcs_all

best_30_vars_and_errors <- left_join(VAR_fcs_all$info_fit_ifcs, cv_objects, by = c("id")) %>% 
  select(variables.x, lags.x, id, rmse, horizon, cv_errors) %>% 
  rename(variables = variables.x, lags = lags.x)

# selected_model_list <- list()
# horizon_names <- c("h = 1", "h = 2", "h = 3", "h = 4", "h = 5", "h = 6", "h = 7", "h = 8")

best_10_VAR_models_plus_negative_corr_models <- generate_best_10_models_plus_negative_corr_models(
  models_and_errors = best_30_vars_and_errors, 
  all_best_models = VAR_fcs_all$info_fit_ifcs)
best_10_VAR_models_plus_negative_corr_models[["best_10_vars_plus_neg_corr_models"]]
# drop the h = 8 forecast 
summ_best10_vars_and_negative_corr_models <- best_10_VAR_models_plus_negative_corr_models[["best_10_vars_plus_neg_corr_models"]] %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum)) %>% 
  filter(grepl('1|2|3|4|5|6|7', horizon))

summ_best10_vars_and_negative_corr_models <- summ_best10_vars_and_negative_corr_models %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)

fcs_combined_neg_corr_vars <- ts(data = summ_best10_vars_and_negative_corr_models$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))

plot_combined_models_title <- paste("GDP Forecasts Combined Models", country_name, sep = " ")
filename_combined_models_plot <- paste(plot_combined_models_title, "png", sep = ".")
# p + scale_x_discrete(name ="Dose (mg)", 
#                      limits=c("2","1","0.5"))
# p <- autoplot(rgdp_var)
forecast_plot_best_combined <- autoplot(rgdp_var) +
  autolayer(fcs_all_VAR_best5_ts, series="Best 5 VARs", linetype = 2, size = 1) + 
  autolayer(fcs_all_ssel_arimax_ts, series="Best 30 ARIMAX Stata Selection", linetype = 3, size = 1) +
  autolayer(fcs_combined_models, series="Best 30 Combined Models Stata Selection", linetype = 4, size = 1) +
  autolayer(fcs_combined_models_best_20, series="Best 20 Combined Models Stata Selection", linetype = 5, size = 1) +
  autolayer(fcs_all_VAR_best20_ts, series="Best 20 VARs", linetype = 6, size = 1) +
  autolayer(fcs_combined_neg_corr_vars, series="Best 10 VARs and negatively correlated VARs", linetype = 2, size = 1) +
  ggtitle(plot_combined_models_title) +
  scale_colour_brewer(palette = "Set2") +
  guides(colour=guide_legend(title="Models:")) +
  theme_cepal_fcs_line_graph +
  scale_y_continuous(name = "Real GDP Growth YoY (%)", 
                     breaks=c(-0.03, -0.02, -0.01, 0.00, 0.01, 0.02, 0.03, 0.04, 
                              0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) + 
  scale_x_yearmon(name = "Year", n = 15, format = "%b  %Y") + 
  annotation_custom(ymin= -0.04, ymax= -0.01, xmin = 2018.00, xmax = 2020.00, rasterGrob(watermark_cepal))


ggsave(filename = filename_combined_models_plot, path = plots_path)

print(forecast_plot_best_combined)

################################### Forecasts Tables #####################################
# We have to create a nice table or graph with the different forecasts produced
# What I have here could be a decent starting point
# all_fcs <- ts.union(fcs_all_VAR_best5_ts, fcs_all_VAR_best10_ts, fcs_all_VAR_best15_ts, 
#                     fcs_all_VAR_best20_ts, fcs_all_arimax_ts, fcs_all_arimax_ssel_ts, fcs_combined_models,
#                     fcs_combined_models_best_20, fcs_combined_neg_corr_vars)
# 
# Economic_Growth_2018_best_5_vars <- mean(all_fcs[2:5, 1]); Economic_Growth_2018_best_5_vars
# Economic_Growth_2019_best_5_vars <- mean(all_fcs[6:9, 1]); Economic_Growth_2019_best_5_vars
# Economic_Growth_2018_best_10_vars <- mean(all_fcs[2:5, 2]); Economic_Growth_2018_best_10_vars
# Economic_Growth_2019_best_10_vars <- mean(all_fcs[6:9, 2]); Economic_Growth_2019_best_10_vars
# Economic_Growth_2018_best_15_vars <- mean(all_fcs[2:5, 3]); Economic_Growth_2018_best_15_vars
# Economic_Growth_2019_best_15_vars <- mean(all_fcs[6:9, 3]); Economic_Growth_2019_best_15_vars
# Economic_Growth_2018_best_20_vars <- mean(all_fcs[2:5, 4]); Economic_Growth_2018_best_20_vars
# Economic_Growth_2019_best_20_vars <- mean(all_fcs[6:9, 4]); Economic_Growth_2019_best_20_vars
# Economic_Growth_2018_best_30_arimax <- mean(all_fcs[2:5, 5]); Economic_Growth_2018_best_30_arimax
# Economic_Growth_2019_best_30_arimax <- mean(all_fcs[6:9, 5]); Economic_Growth_2019_best_30_arimax
# Economic_Growth_2018_best_30_arimax_ssel <- mean(all_fcs[2:5, 6]); Economic_Growth_2018_best_30_arimax_ssel
# Economic_Growth_2019_best_30_arimax_ssel <- mean(all_fcs[6:9, 6]); Economic_Growth_2019_best_30_arimax_ssel
# Economic_Growth_2018_best_30_combined_models <- mean(all_fcs[2:5, 7]); Economic_Growth_2018_best_30_combined_models
# Economic_Growth_2019_best_30_combined_models <- mean(all_fcs[6:9, 7]); Economic_Growth_2019_best_30_combined_models
# Economic_Growth_2018_best_20_combined_models <- mean(all_fcs[2:5, 8]); Economic_Growth_2018_best_20_combined_models
# Economic_Growth_2019_best_20_combined_models <- mean(all_fcs[6:9, 8]); Economic_Growth_2019_best_20_combined_models
# Economic_Growth_2018_combined_neg_corr_vars <- mean(all_fcs[2:5, 9]); Economic_Growth_2018_combined_neg_corr_vars
# Economic_Growth_2019_combined_neg_corr_vars <- mean(all_fcs[6:9, 9]); Economic_Growth_2019_combined_neg_corr_vars
# 
# Economic_growth_2018_tibble <- tibble(Economic_Growth_2018_best_5_vars, Economic_Growth_2018_best_10_vars, 
#                                Economic_Growth_2018_best_15_vars, Economic_Growth_2018_best_20_vars,
#                                Economic_Growth_2018_best_30_arimax, Economic_Growth_2018_best_30_arimax_ssel,
#                                Economic_Growth_2018_best_20_combined_models, Economic_Growth_2018_best_30_combined_models)
# Economic_growth_2019_tibble <- tibble(Economic_Growth_2019_best_5_vars, Economic_Growth_2019_best_10_vars, 
#                                       Economic_Growth_2019_best_15_vars, Economic_Growth_2019_best_20_vars,
#                                       Economic_Growth_2019_best_30_arimax, Economic_Growth_2019_best_30_arimax_ssel,
#                                       Economic_Growth_2019_best_20_combined_models, Economic_Growth_2019_best_30_combined_models)

# Annual Forecasts
final_fcs <- ts.union(rgdp_var, fcs_all_ssel_arimax_ts,
                      fcs_combined_models_best_20, fcs_combined_neg_corr_vars, fcs_all_VAR_best20_ts)

final_fcs_tbl_long <- final_fcs  %>% as.xts() %>% apply.yearly(FUN = mean) %>% tk_tbl() %>% mutate(index = year(index))  %>%
  rename(rgdp = rgdp_var, forecast_arimax = fcs_all_ssel_arimax_ts, forecast_best_20_combined = fcs_combined_models_best_20,
         forecast_best_10_and_negative_correlated = fcs_combined_neg_corr_vars, forecast_best_20_VARs = fcs_all_VAR_best20_ts) %>%  
  tidyr::gather(key = "model", value = "forecast", -index) %>% as.data.frame() %>% na.omit() 


# rgdp_ggplot <- final_fcs_tbl_long %>% dplyr::filter(grepl('rgdp', model))
# only_fcs_tbl <- final_fcs_tbl_long[13:18, ] # only select the forecasts and not rgdp
# Instead of using numbers we select the models by name using dplyr filter and grepl. 
# I do this because the position is different for each country whereas the names are constant. So in this way it works for each country
only_fcs_tbl <- final_fcs_tbl_long %>% dplyr::filter(grepl('forecast_arimax|forecast_best_20|forecast_best_10_and_negative_correlated|forecast_best_20_VARs', model))
only_fcs_tbl$forecast <- round(only_fcs_tbl$forecast, digits = 4)

plot_annual_forecast_title <- paste("Annual GDP Forecasts", country_name, sep = " ")
filename_annual_forecast_plot <- paste(plot_annual_forecast_title, "png", sep = ".")
# position dodge is used so that the bars are next to each other and not on top of each other
# figure out how to control spacing between observations
# figure out how to get values on the last bars of the forecasts
annual_forecast_bar_plot <- ggplot(final_fcs_tbl_long, aes(x = index, y = forecast, fill = model)) + 
  geom_col(width=0.8, position = "dodge") +
  geom_text(data = only_fcs_tbl, aes(label = forecast, fontface = "bold"), 
            position = position_dodge(width=0.9), size=2, hjust = 0, vjust = -0.5, angle = 60) +
  theme_cepal_fcs_bar_plot +
  scale_fill_brewer(palette = "Set3") + 
  ggtitle(plot_annual_forecast_title) +
  guides(fill=guide_legend(title="Time-Series:")) +
  scale_y_continuous(name = "Real GDP Growth YoY (%)", 
                     breaks=c(-0.03, -0.02, -0.01, 0.00, 0.01, 0.02, 0.03, 0.04, 
                              0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) + 
  scale_x_yearmon(name = "Year", n = 15, format = "%Y") +
  annotation_custom(ymin= -0.02, ymax= 0.00, xmin = 2018.00, xmax = 2020.00, rasterGrob(watermark_cepal))


ggsave(filename = filename_annual_forecast_plot, path = plots_path)

annual_forecast_bar_plot

### extract info from best-model tibbes

variables_info_VAR30 <- freq_n_of_variables(tbl_of_models = VAR_fcs_all)
variables_count_VAR30 <- variables_info_VAR30$variable_n_tbl
variables_count_VAR30 
variables_overall_freq_VAR30 <- variables_info_VAR30$all_variables_freq_table
variables_overall_freq_VAR30 
variables_by_h_freq_VAR30 <- variables_info_VAR30$tbl_with_freqs_per_h
variables_by_h_freq_VAR30 

variables_info_VAR20 <- freq_n_of_variables(tbl_of_models = VAR_fcs_all_best_20)
variables_count_VAR20 <- variables_info_VAR20$variable_n_tbl
variables_count_VAR20 
variables_overall_freq_VAR20 <- variables_info_VAR20$all_variables_freq_table
variables_overall_freq_VAR20 
variables_by_h_freq_VAR20 <- variables_info_VAR20$tbl_with_freqs_per_h
variables_by_h_freq_VAR20 

# rgdp_yoy_VAR_timespan <-  window(make_yoy_ts(rgdp_level_ts), 
#                                  start = start(VAR_data_rgdp),
#                                  end = end(VAR_data_rgdp))

# It is better to get the raw data for all variables because so times the montly variables already have the
# full next quarter
country_data_level_ts <- get_raw_data_ts(country = country_name)
rgdp_level_ts <- country_data_level_ts[,"rgdp"]
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)

# Top 10 Variables
VAR_data_imp_intermediate <- VAR_data[, "imp_intermediate"] #1 recommendation: yoy, monthly variable (so the next q might already be available)
imp_intermediate_level_ts <- country_data_level_ts[,"imp_intermediate"] %>% na.omit() 
imp_intermediate_yoy_ts <- window(make_yoy_ts(imp_intermediate_level_ts), 
                           start = start(VAR_data_rgdp), 
                           end = end(imp_intermediate_level_ts))


VAR_data_primario <- VAR_data[, "primario"] #2 recommendation: yoy, quarterly variable (so no info ahead of rgdp)
VAR_data_rgc <- VAR_data[, "rgc"] #3 recommendation: yoy, quarterly variable (so no info ahead of rgdp)

VAR_data_vta_elect <- VAR_data[, "vta_elect"] #4 recommendation: diff, monthly variable (so the next q might already be available)
vta_elect_level_ts <- country_data_level_ts[,"vta_elect"] %>% na.omit() 
vta_elect_yoy_ts <- window(make_yoy_ts(vta_elect_level_ts), 
                           start = start(VAR_data_rgdp), 
                           end = end(vta_elect_level_ts))

VAR_data_icc_capital <- VAR_data[, "icc_capital"] #5 recommendation: level, monthly variable (so the next q might already be available)
icc_capital_level_ts <- country_data_level_ts[,"icc_capital"] %>% na.omit() 
icc_capital_yoy_ts <- window(make_yoy_ts(icc_capital_level_ts), 
                                  start = start(VAR_data_rgdp), 
                                  end = end(icc_capital_level_ts))

VAR_data_icc_nacional <- VAR_data[, "icc_nacional"] #6 recommendation: level, monthly variable (so the next q might already be available)
icc_nacional_level_ts <- country_data_level_ts[,"icc_nacional"] %>% na.omit() 
icc_nacional_yoy_ts <- window(make_yoy_ts(icc_nacional_level_ts), 
                             start = start(VAR_data_rgdp), 
                             end = end(icc_nacional_level_ts))
VAR_data_ip <- VAR_data[, "ip"] #7 recommendation: yoy, monthly variable (so the next q might already be available)
ip_level_ts <- country_data_level_ts[,"ip"] %>% na.omit() 
ip_yoy_ts <- window(make_yoy_ts(ip_level_ts), 
                              start = start(VAR_data_rgdp), 
                              end = end(ip_level_ts))
VAR_data_serv <- VAR_data[, "serv"] #8 recommendation: diff yoy, quarterly variable
serv_level_ts <- country_data_level_ts[,"serv"] %>% na.omit() 
serv_yoy_ts <- window(make_yoy_ts(serv_level_ts), 
                    start = start(VAR_data_rgdp), 
                    end = end(serv_level_ts))

VAR_data_manuf <- VAR_data[, "manuf"] #9 recommendation: diff yoy, quarterly variable
manuf_level_ts <- country_data_level_ts[,"manuf"] %>% na.omit() 
manuf_yoy_ts <- window(make_yoy_ts(manuf_level_ts), 
                      start = start(VAR_data_rgdp), 
                      end = end(manuf_level_ts))
VAR_data_expectativa_inflacion <- VAR_data[, "expectativa_inflacion"] #10 recommendation: diff, monthly variable (so the next q might already be available)
expectativa_inflacion_level_ts <- country_data_level_ts[,"expectativa_inflacion"] %>% na.omit() 
expectativa_inflacion_yoy_ts <- window(make_yoy_ts(expectativa_inflacion_level_ts), 
                    start = start(VAR_data_rgdp), 
                    end = end(expectativa_inflacion_level_ts))
# 1 Imp_intermediate
rgdp_label_for_plot <- "real GDP"
this_series_name <-  "Imp intermediate"
y_label_yoy <- "YoY variation (%)" 
x_label <- ""
plot_imp_intermediate <- autoplot(100*imp_intermediate_yoy_ts, series = this_series_name) +
  autolayer(100*rgdp_var, series=rgdp_label_for_plot, linetype = 2, size = 1) + 
  xlab(x_label) + 
  ylab(y_label_yoy) 
# diff_imp_intermediate <- stats::lag(VAR_data_imp_intermediate, k=1)

imp_intermediate_yoy_ts_rgdp <- window(make_yoy_ts(imp_intermediate_level_ts), 
                                  start = start(VAR_data_rgdp), 
                                  end = end(VAR_data_rgdp))

# this assumes that imp are not in logs
seas_yr_growth(imp_intermediate_level_ts, year_1 = 2017, year_2 = 2018, is_log = FALSE, freq = 4) 

cor(imp_intermediate_yoy_ts_rgdp, VAR_data_rgdp)
print(plot_imp_intermediate)

# 2 Primario
plot_primario <- autoplot(VAR_data_primario) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_primario)

cor(VAR_data_primario, VAR_data_rgdp)

# 3 rgc
plot_rgc <- autoplot(VAR_data_rgc) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_rgc)

cor(VAR_data_rgc, VAR_data_rgdp)

# 4 vta_elect
plot_vta_elect <- autoplot(vta_elect_yoy_ts) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_vta_elect)
# set the time-series equal for the corr
vta_elect_yoy_ts_rgdp <- window(vta_elect_yoy_ts, 
                           start = start(VAR_data_rgdp), 
                           end = end(VAR_data_rgdp))
cor(vta_elect_yoy_ts_rgdp, VAR_data_rgdp)

# 5 icc_capital
plot_icc_capital <- autoplot(icc_capital_yoy_ts) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_icc_capital)
# set the time-series equal for the corr
icc_capital_yoy_ts_rgdp <- window(icc_capital_yoy_ts, 
                                start = start(VAR_data_rgdp), 
                                end = end(VAR_data_rgdp))
cor(icc_capital_yoy_ts_rgdp, VAR_data_rgdp)

# 6 icc_nacional
plot_icc_nacional <- autoplot(icc_nacional_yoy_ts) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_icc_nacional)
# set the time-series equal for the corr
icc_nacional_yoy_ts_rgdp <- window(icc_nacional_yoy_ts, 
                                  start = start(VAR_data_rgdp), 
                                  end = end(VAR_data_rgdp))
cor(icc_nacional_yoy_ts_rgdp, VAR_data_rgdp)


# 7 ip
plot_ip <- autoplot(ip_yoy_ts) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_ip)

cor(VAR_data_ip, VAR_data_rgdp)

# 8 serv
plot_serv <- autoplot(serv_yoy_ts) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_serv)

cor(serv_yoy_ts, VAR_data_rgdp)

# 9 manuf
plot_manuf <- autoplot(manuf_yoy_ts) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_manuf)

cor(manuf_yoy_ts, VAR_data_rgdp)

# 10 expectativa_inflacion_yoy_ts
plot_expectativa_inflacion_yoy_ts <- autoplot(expectativa_inflacion_yoy_ts) +
  autolayer(rgdp_var, series="rgdp", linetype = 2, size = 1)

print(plot_expectativa_inflacion_yoy_ts)
# set the time-series equal for the corr
expectativa_inflacion_yoy_ts_rgdp <- window(expectativa_inflacion_yoy_ts, 
                                   start = start(expectativa_inflacion_yoy_ts), 
                                   end = end(VAR_data_rgdp))
VAR_data_rgdp_expectativa_inflacion <- window(VAR_data_rgdp, 
                                              start = start(expectativa_inflacion_yoy_ts), 
                                              end = end(VAR_data_rgdp))
cor(expectativa_inflacion_yoy_ts_rgdp, VAR_data_rgdp_expectativa_inflacion)

