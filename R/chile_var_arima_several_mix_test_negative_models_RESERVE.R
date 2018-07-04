source('./R/utils_av.R')
library(ggthemes)
library(ggThemeAssist)
library(RColorBrewer)
library(jpeg)
library(imager)
# library(ggraptR)

# Functions: to be moved to utils_av
# add an extra row with horizon = 0 and sum_one_h is the last observation of rgdp_var
add_row_fcs_summary <- function(summary_model, rgdp_ts){
  summary_model %>% add_row(horizon = 0, sum_one_h = last(rgdp_ts)) %>% arrange(horizon)
}

turn_summary_in_ts <- function(summary_model, start_year_ts, start_qtr_ts, frequency = 4){
  ts(data = summary_model$sum_one_h, frequency = frequency, start = c(start_year_ts, start_qtr_ts))
}

generate_best_10_models_plus_negative_corr_models <- function(models_and_errors, all_best_models) {
  selected_model_list <- list()
  horizon_names <- c("h = 1", "h = 2", "h = 3", "h = 4", "h = 5", "h = 6", "h = 7", "h = 8")
  
  for (i in seq_along(horizon_names)){
    models_and_errors_h <- models_and_errors %>% filter(horizon == i) %>% 
      arrange(rmse) %>% 
      mutate(cv_errors_all_h_mat = map(cv_errors, ~ reduce(., rbind)),
             cv_errors_this_h = map(cv_errors_all_h_mat, ~ .[,i]))
    
    just_errors_h <- models_and_errors_h %>% select(cv_errors_this_h)
    
    errors_cv_h <- reduce(just_errors_h[[1]], rbind)
    row.names(errors_cv_h ) <- models_and_errors_h$id
    colnames(errors_cv_h) <- paste0("cv_", 1:8)
    errors_cv_h  <- t(errors_cv_h )
    errors_cv_h  <- as_data_frame(errors_cv_h)
    cor_error_h <- cor(errors_cv_h)
    neg_cor_error_1 <- cor_error_h[1, ] < 0
    min_cor_error_1 <-  min(min(cor_error_h[1, ] ))
    neg_cor_error_2 <- cor_error_h[2, ] < 0
    min_cor_error_2 <-  min(min(cor_error_h[2, ] ))
    neg_cor_error_3 <- cor_error_h[3, ] < 0
    min_cor_error_3 <-  min(min(cor_error_h[3, ] )) 
    neg_cor_error_4 <- cor_error_h[4, ] < 0
    min_cor_error_4 <-  min(min(cor_error_h[4, ] ))
    neg_cor_error_5 <- cor_error_h[5, ] < 0
    min_cor_error_5 <-  min(min(cor_error_h[5, ] ))
    neg_cor_error_6 <- cor_error_h[6, ] < 0
    min_cor_error_6 <-  min(min(cor_error_h[6, ] ))
    neg_cor_error_7 <- cor_error_h[7, ] < 0
    min_cor_error_7 <-  min(min(cor_error_h[7, ] ))
    neg_cor_error_8 <- cor_error_h[8, ] < 0
    min_cor_error_8 <-  min(min(cor_error_h[8, ] )) 
    neg_cor_error_9 <- cor_error_h[9, ] < 0
    min_cor_error_9 <-  min(min(cor_error_h[9, ] ))
    neg_cor_error_10 <- cor_error_h[10, ] < 0
    min_cor_error_10 <-  min(min(cor_error_h[10, ] ))
    a1 <- names(errors_cv_h)[neg_cor_error_1] 
    b1 <- names(errors_cv_h)[cor_error_h[1,] == min_cor_error_1]
    a2 <- names(errors_cv_h)[neg_cor_error_2] 
    b2 <- names(errors_cv_h)[cor_error_h[2,] == min_cor_error_2]
    a3 <- names(errors_cv_h)[neg_cor_error_3]
    b3 <- names(errors_cv_h)[cor_error_h[3,] == min_cor_error_3]
    a4 <- names(errors_cv_h)[neg_cor_error_4]
    b4 <- names(errors_cv_h)[cor_error_h[4,] == min_cor_error_4]
    a5 <- names(errors_cv_h)[neg_cor_error_5]
    b5 <- names(errors_cv_h)[cor_error_h[5,] == min_cor_error_5]
    a6 <- names(errors_cv_h)[neg_cor_error_6] 
    b6 <- names(errors_cv_h)[cor_error_h[6,] == min_cor_error_6]
    a7 <- names(errors_cv_h)[neg_cor_error_7] 
    b7 <- names(errors_cv_h)[cor_error_h[7,] == min_cor_error_7]
    a8 <- names(errors_cv_h)[neg_cor_error_8]
    b8 <- names(errors_cv_h)[cor_error_h[8,] == min_cor_error_8]
    a9 <- names(errors_cv_h)[neg_cor_error_9]
    b9 <- names(errors_cv_h)[cor_error_h[9,] == min_cor_error_9]
    a10 <- names(errors_cv_h)[neg_cor_error_10]
    b10 <- names(errors_cv_h)[cor_error_h[10,] == min_cor_error_10]
    
    selected_model_1 <- ifelse(sum(neg_cor_error_1)==0, "", b1 )
    selected_model_2 <- ifelse(sum(neg_cor_error_2)==0, "", b2 )
    selected_model_3 <- ifelse(sum(neg_cor_error_3)==0, "", b3 )
    selected_model_4 <- ifelse(sum(neg_cor_error_4)==0, "", b4 )
    selected_model_5 <- ifelse(sum(neg_cor_error_5)==0, "", b5 )
    selected_model_6 <- ifelse(sum(neg_cor_error_6)==0, "", b6 )
    selected_model_7 <- ifelse(sum(neg_cor_error_7)==0, "", b7 )
    selected_model_8 <- ifelse(sum(neg_cor_error_8)==0, "", b8 )
    selected_model_9 <- ifelse(sum(neg_cor_error_9)==0, "", b9 )
    selected_model_10 <- ifelse(sum(neg_cor_error_10)==0, "", b10 )
    
    selected_model_list[[i]] <- as.vector(c(selected_model_1, selected_model_2, selected_model_3, selected_model_4, 
                                            selected_model_5, selected_model_6, selected_model_7, selected_model_8, 
                                            selected_model_9, selected_model_10))
    
    # selected_model_list[[i]] <- horizon_names[i]
    
  }
  
  names(selected_model_list) <- horizon_names
  selected_model_list <- map(selected_model_list, ~ unique(as.numeric(.)) )
  
  rmse_h_vector <- c("rmse_1", "rmse_2", "rmse_3", "rmse_4", "rmse_5", "rmse_6", "rmse_7", "rmse_8")
  ten_best_vars_each_h <- list()
  
  for (i in 1:8) {
    h = rmse_h_vector[i]
    ten_best_vars <- all_best_models %>% filter(rmse_h == h) %>% mutate(rank_h = rank(rmse)) %>% arrange(rank_h) %>%
      filter(rank_h <= 10) %>% select(id)
    ten_best_vars <- as.vector(ten_best_vars$id)
    ten_best_vars <- as.numeric(ten_best_vars)
    
    ten_best_vars_each_h[[i]] <- ten_best_vars
    
  }
  
  names(ten_best_vars_each_h) <- horizon_names
  
  list_10_best_vars_plus_neg_corr_models <- map2(selected_model_list, ten_best_vars_each_h, ~ c(.x, .y))
  list_10_best_vars_plus_neg_corr_models <- map(list_10_best_vars_plus_neg_corr_models, unique)
  
  # drop the na values to precent issues later
  # must be able to map this but somehow i cant map
  for (i in 1:8){
    h <- horizon_names[i]
    df <- list_10_best_vars_plus_neg_corr_models[[h]]
    df <- df[!is.na(df)]
    list_10_best_vars_plus_neg_corr_models[[i]] <- df
  }
  
  tibble_10_best_vars_plus_neg_corr_models <- tibble()
  for (i in 1:8) {
    list_to_tibble <- tibble(horizon = i, id = list_10_best_vars_plus_neg_corr_models[[i]])
    tibble_10_best_vars_plus_neg_corr_models <- rbind(tibble_10_best_vars_plus_neg_corr_models, list_to_tibble)
  }
  
  best_10_vars_plus_neg_corr_models <- left_join(tibble_10_best_vars_plus_neg_corr_models,
                                                 all_best_models, by = c("horizon", "id")) %>%
    group_by(horizon) %>%
    mutate(sum_invmse_h = sum(inv_mse),
           model_weight_h = inv_mse/sum_invmse_h,
           w_fc = pmap(list(model_weight_h, fc_yoy, horizon),
                       ~ subset(..1 * ..2, start = ..3, end = ..3)
           )
    ) %>%
    ungroup()
  
  return(list(best_10_vars_plus_neg_corr_models = best_10_vars_plus_neg_corr_models, negative_correlated_models = selected_model_list))
  
}

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

country_name <- "Chile"

# Optional: Estimate (and Save) new Arimax RDS file

# final_forecast_horizon <- c(2020, 12)
# h_max = 6 # last rgdp data is 2017 Q4
# number_of_cv = 8
# train_span = 16
# arima_res2 <- get_arima_results(country_name = country_name, h_max = 6,
#                                number_of_cv = 8, train_span = 16,
#                                final_ext_horizon = final_forecast_horizon)

# Run Saved ARimax RDS File
arima_res <- get_arima_results(country_name = country_name, read_results = TRUE)

extended_x_data_ts <- arima_res$mdata_ext_ts
rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima


########################## IMPORT ARIMAX AND VAR FILES #############################
# Automatically imports the data matching the country_name
path_models_and_accu <- paste("./data/", country_name, "_by_step_12345.rds", sep = "")
path_cv_objects <- paste("./data/", country_name, "_by_step_12345_cv_objects.rds", sep = "")
path_VAR_data <- paste("./data/VAR_data_", country_name, ".rds", sep = "")
models_and_accu <- readRDS(path_models_and_accu)
cv_objects <- readRDS(path_cv_objects)
VAR_data <- readRDS(path_VAR_data)

# NEW: for negatively correlated models: add an id variable to the cv_objects and models accu of the var models
cv_objects <- cv_objects %>% mutate(id = 1:n())
models_and_accu <- models_and_accu %>% mutate(id = 1:n())

h_max <- 8

# Get the table with all models from both the VARs and ARIMAX
# I adjusted the make_model_tbl function in utils_av: each_h_just_model_and_ave_rmse_sarimax is adjusted so it selects the id variable
models_tbl <- make_models_tbl(
  arima_res = arima_res, var_models_and_rmse = models_and_accu, 
  VAR_data = VAR_data, h_max = h_max)

models_tbl <- models_tbl %>%
  mutate(short_name = map2(variables, lags,
                           ~ make_model_name(variables = .x, lags = .y)),
         long_name = pmap(list(variables, lags, model_function),
                          ~ make_model_name(variables = ..1, lags = ..2,
                                            model_function = ..3)),
         short_name = as_factor(unlist(short_name)),
         long_name = as_factor(unlist(long_name))
  ) 
  

# ssel stands for "stata_selection" and what it does it to imitate stata-style selection 
models_tbl_ssel <- make_models_tbl(
  arima_res, var_models_and_rmse = models_and_accu, VAR_data = VAR_data,
  h_max = h_max, ave_rmse_sel = TRUE)

models_tbl_ssel <- models_tbl_ssel %>%
  mutate(short_name = map2(variables, lags,
                           ~ make_model_name(variables = .x, lags = .y)),
         long_name = pmap(list(variables, lags, model_function),
                          ~ make_model_name(variables = ..1, lags = ..2,
                                            model_function = ..3))
  ) 




cv_from_models_tbl <- function(VAR_data, rgdp_ts_in_arima, tbl_of_models_and_rmse,
                               h, max_rank_h, test_length = NULL, training_span = 20,
                               n_cv = 10, model_type = NULL) {
  
  if (is.null(test_length)) {
    test_length <- h
  }
  
  cv_errors_list <-  list()
  separate_fcs_list <- list()
  ensemble_fcs_list <-  list()
  
  len_var_data <- nrow(VAR_data)
  len_arima_data <- length(rgdp_ts_in_arima)
  
  max_fixed_span_for_training <- min(len_var_data, len_arima_data) - n_cv - test_length + 1
  
  
  print(paste("Obs in VAR:", len_var_data))
  print(paste("Obs in Arima:", len_arima_data))
  print(paste("Obs in train set:", training_span))
  print(paste("Obs in test set:", test_length))
  print(paste("CV samples:", n_cv))
  print(paste("Max size of fixed training span :", max_fixed_span_for_training))
  
  
  for (i in 1:n_cv) {
    
    print(paste("Starting CV step", i, "of", n_cv))
    print("")
    
    var_train_end <- len_var_data - test_length - i + 1
    
    if (is.null(training_span)) {
      training_VAR_data <- subset(VAR_data, end = var_train_end)
    } else {
      training_VAR_data <- subset(VAR_data, end = var_train_end, 
                                  start = var_train_end - training_span + 1)
      
      # print(var_train_end - training_span + 1)
    }
    
    test_VAR_data <- subset(VAR_data, start = var_train_end + 1,
                            end = var_train_end + test_length)
    
    test_VAR_data <- test_VAR_data[, "rgdp"]
    
    arima_train_end <- len_arima_data - test_length - i + 1
    
    
    if (is.null(training_span)) {
      training_arima_data <- subset(rgdp_ts_in_arima, end = arima_train_end)
    } else {
      training_arima_data <- subset(rgdp_ts_in_arima, end = arima_train_end,
                                    start = arima_train_end - training_span + 1)
    }
    
    test_arima_data <- subset(rgdp_ts_in_arima, start = arima_train_end + 1,
                              end = arima_train_end + test_length)
    
    
    separate_fcs <- indiv_weigthed_fcs(tbl_of_models_and_rmse = tbl_of_models_and_rmse,
                                       h = 8,
                                       extended_x_data_ts = extended_x_data_ts, 
                                       rgdp_ts_in_arima = training_arima_data, 
                                       max_rank_h = max_rank_h,
                                       var_data = training_VAR_data,
                                       model_type = model_type)
    
    ensemble_fcs <- separate_fcs %>%
      group_by(horizon) %>%
      summarise(sum_one_h = reduce(one_model_w_fc, sum))
    
    ensemble_fcs_ts <- ts(ensemble_fcs$sum_one_h, 
                          start = start(test_VAR_data), 
                          frequency = 4)
    
    ensemble_cv_errors <-  test_VAR_data - ensemble_fcs_ts 
    
    cv_errors_list[[i]] <- ensemble_cv_errors
    separate_fcs_list[[i]] <- separate_fcs
    ensemble_fcs_list[[i]] <- ensemble_fcs_ts
    
  }
  
  # rows are cv trials and columns are forecast horizons
  matrix_of_cv_errors <- reduce(cv_errors_list, rbind)
  dimnames(matrix_of_cv_errors) <- NULL
  # matrix_of_cv_errors
  rmse_all_h <- sqrt(colMeans(matrix_of_cv_errors^2))
  mae_all_h <- colMeans(abs(matrix_of_cv_errors))
  
  return(list(cv_errors_list = cv_errors_list,
              matrix_of_cv_errors = matrix_of_cv_errors,
              rmse_all_h = rmse_all_h,
              mae_all_h = mae_all_h,
              separate_fcs_list = separate_fcs_list,
              ensemble_fcs_list = ensemble_fcs_list))
  
  
}

foo <- cv_from_models_tbl(VAR_data = VAR_data, rgdp_ts_in_arima = rgdp_ts_in_arima,
                          tbl_of_models_and_rmse = models_tbl,
                          h = h_max, max_rank_h = 20, test_length = NULL, 
                          training_span = 25, n_cv = 8)


foo$rmse_all_h
foo$matrix_of_cv_errors


foo1 <- cv_from_models_tbl(VAR_data = VAR_data, rgdp_ts_in_arima = rgdp_ts_in_arima,
                          tbl_of_models_and_rmse = models_tbl,
                          h = h_max, max_rank_h = 1, test_length = NULL, 
                          training_span = 25, n_cv = 8)


foovar1 <- cv_from_models_tbl(VAR_data = VAR_data, rgdp_ts_in_arima = rgdp_ts_in_arima,
                           tbl_of_models_and_rmse = models_tbl,
                           h = 8, max_rank_h = 1, test_length = NULL, 
                           training_span = 25, n_cv = 8, model_type = "VAR")



fooarima1 <- cv_from_models_tbl(VAR_data = VAR_data, rgdp_ts_in_arima = rgdp_ts_in_arima,
                              tbl_of_models_and_rmse = models_tbl,
                              h = 8, max_rank_h = 1, test_length = NULL, 
                              training_span = 25, n_cv = 8, model_type = "Arima")


foo5 <- cv_from_models_tbl(VAR_data = VAR_data, rgdp_ts_in_arima = rgdp_ts_in_arima,
                           tbl_of_models_and_rmse = models_tbl,
                           h = h_max, max_rank_h = 5, test_length = NULL, 
                           training_span = 25, n_cv = 8)


foovar5 <- cv_from_models_tbl(VAR_data = VAR_data, rgdp_ts_in_arima = rgdp_ts_in_arima,
                              tbl_of_models_and_rmse = models_tbl,
                              h = 8, max_rank_h = 5, test_length = NULL, 
                              training_span = 25, n_cv = 8, model_type = "VAR")


# with n_cv = 20 at the last iteration (i.e. farthest back CV dates) ine_ip introduces an NA and arimax fails
fooarima5 <- cv_from_models_tbl(VAR_data = VAR_data, rgdp_ts_in_arima = rgdp_ts_in_arima,
                                tbl_of_models_and_rmse = models_tbl,
                                h = 8, max_rank_h = 5, test_length = NULL, 
                                training_span = 25, n_cv = 8, model_type = "Arima")


b1r <- as_tibble(rbind(foo1$rmse_all_h,  fooarima1$rmse_all_h,   foovar1$rmse_all_h))
b1m <- as_tibble(rbind(foo1$mae_all_h,  fooarima1$mae_all_h,   foovar1$mae_all_h))

b5r <- as_tibble(rbind(foo5$rmse_all_h,  fooarima5$rmse_all_h,   foovar5$rmse_all_h))
b5m <- as_tibble(rbind(foo5$mae_all_h,  fooarima5$mae_all_h,   foovar5$mae_all_h))

b1r
b5r
b1m
b5m


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
                                h = h_max, extended_x_data_ts = extended_x_data_ts,
                                rgdp_ts_in_arima = rgdp_ts_in_arima,
                                model_type = "VAR", max_rank_h = 30)

summ_VAR_fcs_all <- VAR_fcs_all %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

VAR_fcs_all_best_10 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                   h = h_max, extended_x_data_ts = extended_x_data_ts,
                                   rgdp_ts_in_arima = rgdp_ts_in_arima,
                                   model_type = "VAR", max_rank_h = 10)

summ_VAR_fcs_all_best_10 <- VAR_fcs_all_best_10 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

VAR_fcs_all_best_5 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                  h = h_max, extended_x_data_ts = extended_x_data_ts,
                                  rgdp_ts_in_arima = rgdp_ts_in_arima,
                                  model_type = "VAR", max_rank_h = 5)

summ_VAR_fcs_all_best_5 <- VAR_fcs_all_best_5 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

VAR_fcs_all_best_15 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                   h = h_max, extended_x_data_ts = extended_x_data_ts,
                                   rgdp_ts_in_arima = rgdp_ts_in_arima,
                                   model_type = "VAR", max_rank_h = 15)

summ_VAR_fcs_all_best_15 <- VAR_fcs_all_best_15 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

VAR_fcs_all_best_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                   h = h_max, extended_x_data_ts = extended_x_data_ts,
                                   rgdp_ts_in_arima = rgdp_ts_in_arima,
                                   model_type = "VAR", max_rank_h = 20)

summ_VAR_fcs_all_best_20 <- VAR_fcs_all_best_20 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

rgdp_var <- VAR_data[ ,1]
# autoplot(rgdp_var)

start_ts_fcs_var_year <- year(as.yearqtr(last(time(rgdp_var)))) # use year from lubridate. Time gives you the time like 2017.5 etc
# last gives you the last observation. as.yearqtr turns it into 2017 Q3 form and year gives the year
start_ts_fcs_var_qtr <- quarter(as.yearqtr(last(time(rgdp_var)))) # use quarter from lubridate

summ_VAR_fcs_all <- add_row_fcs_summary(summary_model = summ_VAR_fcs_all, rgdp_ts = rgdp_var)
summ_VAR_fcs_all_best_5 <- add_row_fcs_summary(summary_model = summ_VAR_fcs_all_best_5, rgdp_ts = rgdp_var)
summ_VAR_fcs_all_best_10 <- add_row_fcs_summary(summary_model = summ_VAR_fcs_all_best_10, rgdp_ts = rgdp_var)
summ_VAR_fcs_all_best_15 <- add_row_fcs_summary(summary_model = summ_VAR_fcs_all_best_15, rgdp_ts = rgdp_var)
summ_VAR_fcs_all_best_20 <- add_row_fcs_summary(summary_model = summ_VAR_fcs_all_best_20, rgdp_ts = rgdp_var)

fcs_all_VAR_best5_ts <- turn_summary_in_ts(summary_model = summ_VAR_fcs_all_best_5, 
                                       start_year_ts = start_ts_fcs_var_year, 
                                       start_qtr_ts = start_ts_fcs_var_qtr)

fcs_all_VAR_best10_ts <- turn_summary_in_ts(summary_model = summ_VAR_fcs_all_best_10, 
                                        start_year_ts = start_ts_fcs_var_year, 
                                        start_qtr_ts = start_ts_fcs_var_qtr)

fcs_all_VAR_best15_ts <- turn_summary_in_ts(summary_model = summ_VAR_fcs_all_best_15, 
                                        start_year_ts = start_ts_fcs_var_year, 
                                        start_qtr_ts = start_ts_fcs_var_qtr)

fcs_all_VAR_best20_ts <- turn_summary_in_ts(summary_model = summ_VAR_fcs_all_best_20, 
                                        start_year_ts = start_ts_fcs_var_year, 
                                        start_qtr_ts = start_ts_fcs_var_qtr)

fcs_all_VAR_ts <- turn_summary_in_ts(summary_model = summ_VAR_fcs_all, 
                                     start_year_ts = start_ts_fcs_var_year, 
                                     start_qtr_ts = start_ts_fcs_var_qtr)

rgdp_var_and_fcs <- ts.union(rgdp_var, fcs_all_VAR_best5_ts, fcs_all_VAR_best10_ts, 
                             fcs_all_VAR_best15_ts, fcs_all_VAR_best20_ts,
                             fcs_all_VAR_ts)

plot_vars_title <- paste("GDP Forecasts Best VARs", country_name, sep = " ")
filename_var_plot <- paste(plot_vars_title, "png", sep = ".")
# directory_var_plot <- paste("./Plots", filename_var_plot, sep = "/")
plots_path = paste("./Plots/", country_name, "/", sep = "")

forecast_plot_best_vars <- autoplot(rgdp_var, size = 0.8) + 
  autolayer(fcs_all_VAR_best5_ts, series="Best 5 VARs", linetype = 2, size = 0.8) + 
  autolayer(fcs_all_VAR_best10_ts, series="Best 10 VARs", linetype = 2, size = 0.8) +
  autolayer(fcs_all_VAR_best15_ts, series="Best 15 VARs", linetype = 2, size = 0.8) +
  autolayer(fcs_all_VAR_best20_ts, series="Best 20 VARs", linetype = 2, size = 0.8) +
  autolayer(fcs_all_VAR_ts, series="Best 30 VARs", linetype = 2, size = 0.8) +
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

arimax_fcs_all <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                  h = h_max, extended_x_data_ts = extended_x_data_ts,
                                  rgdp_ts_in_arima = rgdp_ts_in_arima,
                                  model_type = "Arima")

summ_arimax_fcs_all <- arimax_fcs_all %>%
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

arimax_fcs_all_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                       h = h_max, extended_x_data_ts = extended_x_data_ts,
                                       rgdp_ts_in_arima = rgdp_ts_in_arima,
                                       model_type = "Arima")

summ_arimax_fcs_all_ssel <- arimax_fcs_all_ssel %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

# I leave arimax_best_20 out because somehow horizon 5 drops out and i do not know why

# arimax_fcs_best_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
#                                               h = h_max, extended_x_data_ts = extended_x_data_ts,
#                                               rgdp_ts_in_arima = rgdp_ts_in_arima,
#                                               model_type = "Arima", max_rank_h = 20)
# 
# summ_arimax_fcs_best_20 <- arimax_fcs_best_20 %>% 
#   group_by(horizon) %>%
#   summarise(sum_one_h = reduce(one_model_w_fc, sum))
# 
# arimax_fcs_ssel_best_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
#                                        h = h_max, extended_x_data_ts = extended_x_data_ts,
#                                        rgdp_ts_in_arima = rgdp_ts_in_arima,
#                                        model_type = "Arima", max_rank_h = 20)
# 
# summ_arimax_fcs_ssel_best_20 <- arimax_fcs_ssel_best_20 %>% 
#   group_by(horizon) %>%
#   summarise(sum_one_h = reduce(one_model_w_fc, sum))

# select similar start date for rgdp_arimax as for rgdp_var so that graph is comparable
# first_year_var_ts is used to set the start of arimax series equal to the var series. Besides that we do not use it
first_year_var_ts <- year(as.yearqtr(first(time(rgdp_var))))
first_qtr_var_ts <- quarter(as.yearqtr(first(time(rgdp_var))))
# rgdp_arimax <- window(rgdp_arimax, start = c(first_year_var_ts, first_qtr_var_ts))

start_ts_fcs_arimax_year <- year(as.yearqtr(last(time(rgdp_arimax)))) 
start_ts_fcs_arimax_qtr <- quarter(as.yearqtr(last(time(rgdp_arimax))))

summ_arimax_fcs_all <- add_row_fcs_summary(summary_model = summ_arimax_fcs_all, rgdp_ts = rgdp_arimax)
summ_arimax_fcs_all_ssel <- add_row_fcs_summary(summary_model = summ_arimax_fcs_all_ssel, rgdp_ts = rgdp_arimax)

fcs_all_arimax_ts <- turn_summary_in_ts(summary_model = summ_arimax_fcs_all, 
                                       start_year_ts = start_ts_fcs_var_year, 
                                       start_qtr_ts = start_ts_fcs_var_qtr)

fcs_all_arimax_ssel_ts <- turn_summary_in_ts(summary_model = summ_arimax_fcs_all_ssel, 
                                        start_year_ts = start_ts_fcs_var_year, 
                                        start_qtr_ts = start_ts_fcs_var_qtr)

# Two ways to combine the time-series. 1) Use autolayer, which adds a layer to an existing plot, or 2) ts.union the time-series and then plot them
plot_arimax_title <- paste("GDP Forecasts Best ARIMAX", country_name, sep = " ")
filename_arimax_plot <- paste(plot_arimax_title, "png", sep = ".")

rgdp_var_and_fcs_df <- fcs_all_arimax_ts %>% tk_tbl() %>% as.data.frame()

forecast_plot_best_arimax <- autoplot(rgdp_var, size = 0.8) + 
  autolayer(fcs_all_arimax_ts, series="Best 30 ARIMAX", linetype = 2, size = 0.8) +
  autolayer(fcs_all_arimax_ssel_ts, series="Best 30 ARIMAX Stata Selection", linetype = 2, size = 0.8) +
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

rgdp_arimax_and_fcs <- ts.union(rgdp_arimax, fcs_all_arimax_ts, fcs_all_arimax_ssel_ts)
# rgdp_arimax_and_fcs <- ts.union(rgdp_arimax, fcs_all_arimax_ts, fcs_best_20_arimax_ts, fcs_all_arimax_ssel_ts, fcs_best_20_arimax_ssel_ts)
rgdp_var_and_arimax_fcs <- ts.union(rgdp_var_and_fcs, rgdp_arimax_and_fcs)

plot_vars_arimax_title <- paste("GDP Forecasts Best VARs and ARIMAX", country_name, sep = " ")
filename_vars_arimax_plot <- paste(plot_vars_arimax_title, "png", sep = ".")

# png(filename = directory_var_arimax_plot)

#find a way to make this generic for all countries. Use paste function. "GDP Forecast Best VARs and ARIMAX: Chile"
forecast_plot_best_vars_arimax <- autoplot(rgdp_var) + 
  autolayer(fcs_all_arimax_ts, series="Best 30 ARIMAX", linetype = 2, size = 1) + 
  autolayer(fcs_all_arimax_ssel_ts, series="Best 30 ARIMAX Stata Selection", linetype = 3, size = 1) +
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

##################################### FORECASTS OF VARs AND ARIMAX COMBINED ##################################

# Get the best Forecasts out of all Models: all VARs and all ARIMAX
comb_fcs_all <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                            h = h_max, extended_x_data_ts = extended_x_data_ts,
                            rgdp_ts_in_arima = rgdp_ts_in_arima,
                            max_rank_h = 30)

number_of_models_per_h <- comb_fcs_all %>% group_by(horizon) %>% 
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
rmse_plot_all_h <- single_plot_rmse_all_h(comb_fcs_all) + 
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

facet_plot_rmse_all_h_title <- paste("Facet Plot RMSE at each hoirizon (h)", country_name, sep = " ")
filename_facet_rmse_all_h_plot <- paste(facet_plot_rmse_all_h_title, "png", sep = ".")

facet_rmse_plot_all_h <- facet_rmse_all_h(comb_fcs_all) + 
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
rmse_plots_list <- list_of_rmse_plots(comb_fcs_all)
walk(rmse_plots_list, print)

# Average Forecasts of all models (all models are the best 30 at each h)
summ_comb_fcs_all <- comb_fcs_all %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

comb_fcs_all_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                 h = h_max, extended_x_data_ts = extended_x_data_ts,
                                 rgdp_ts_in_arima = rgdp_ts_in_arima,
                                 max_rank_h = 30)

number_of_models_per_h_ssel <- comb_fcs_all_ssel %>% group_by(horizon) %>% 
  summarise(n_models = n(), 
            n_VAR = sum(model_function == "VAR"),
            n_ARIMA = sum(model_function == "Arima")
  )

number_of_models_per_h_ssel

summ_comb_fcs_all_ssel <- comb_fcs_all_ssel %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

# for consistency with stata i focus on ssel method

comb_fcs_ssel_best_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                    h = h_max, extended_x_data_ts = extended_x_data_ts,
                                    rgdp_ts_in_arima = rgdp_ts_in_arima,
                                    max_rank_h = 20)

summ_comb_fcs_ssel_best_20 <- comb_fcs_ssel_best_20 %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))

summ_comb_fcs_all_ssel <- add_row_fcs_summary(summary_model = summ_comb_fcs_all_ssel, rgdp_ts = rgdp_var)
summ_comb_fcs_ssel_best_20 <- add_row_fcs_summary(summary_model = summ_comb_fcs_ssel_best_20, rgdp_ts = rgdp_var)

fcs_combined_models <- turn_summary_in_ts(summary_model = summ_comb_fcs_all_ssel, 
                                        start_year_ts = start_ts_fcs_var_year, 
                                        start_qtr_ts = start_ts_fcs_var_qtr)

fcs_combined_models_best_20 <- turn_summary_in_ts(summary_model = summ_comb_fcs_ssel_best_20, 
                                          start_year_ts = start_ts_fcs_var_year, 
                                          start_qtr_ts = start_ts_fcs_var_qtr)


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

best_30_vars_and_errors <- left_join(VAR_fcs_all, cv_objects, by = c("id")) %>% 
  select(variables.x, lags.x, id, rmse, horizon, cv_errors) %>% 
  rename(variables = variables.x, lags = lags.x)

# selected_model_list <- list()
# horizon_names <- c("h = 1", "h = 2", "h = 3", "h = 4", "h = 5", "h = 6", "h = 7", "h = 8")

best_10_models_plus_negative_corr_models <- generate_best_10_models_plus_negative_corr_models(
                                                  models_and_errors = best_30_vars_and_errors, 
                                                  all_best_models = VAR_fcs_all)

summ_best10_vars_and_negative_corr_models <- best_10_models_plus_negative_corr_models[["best_10_vars_plus_neg_corr_models"]] %>% 
  group_by(horizon) %>% 
  summarise(sum_one_h = reduce(w_fc, sum))

summ_best10_vars_and_negative_corr_models <- summ_best10_vars_and_negative_corr_models %>% add_row(horizon = 0, sum_one_h = last(rgdp_var)) %>% arrange(horizon)

fcs_combined_neg_corr_vars <- ts(data = summ_best10_vars_and_negative_corr_models$sum_one_h, frequency = 4, start = c(start_ts_fcs_var_year, start_ts_fcs_var_qtr))

plot_combined_models_title <- paste("GDP Forecasts Combined Models", country_name, sep = " ")
filename_combined_models_plot <- paste(plot_combined_models_title, "png", sep = ".")
# p + scale_x_discrete(name ="Dose (mg)", 
#                      limits=c("2","1","0.5"))
# p <- autoplot(rgdp_var)
 forecast_plot_best_combined <- autoplot(rgdp_var, size = 0.8) +
  autolayer(fcs_all_VAR_best5_ts, series="Best 5 VARs", linetype = 2, size = 0.8) + 
  autolayer(fcs_all_arimax_ssel_ts, series="Best 30 ARIMAX Stata Selection", linetype = 2, size = 0.8) +
  autolayer(fcs_combined_models, series="Best 30 Combined Models Stata Selection", linetype = 2, size = 0.8) +
  autolayer(fcs_combined_models_best_20, series="Best 20 Combined Models Stata Selection", linetype = 2, size = 0.8) +
  autolayer(fcs_all_VAR_best20_ts, series="Best 20 VARs", linetype = 2, size = 0.8) +
  autolayer(fcs_combined_neg_corr_vars, series="Best 10 VARs and negatively correlated VARs", linetype = 2, size = 0.8) +
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
final_fcs <- ts.union(rgdp_var, fcs_all_arimax_ssel_ts,
                    fcs_combined_models_best_20, fcs_combined_neg_corr_vars)

final_fcs_tbl_long <- final_fcs  %>% as.xts() %>% apply.yearly(FUN = mean) %>% tk_tbl() %>% mutate(index = year(index))  %>%
  rename(rgdp = rgdp_var, forecast_arimax = fcs_all_arimax_ssel_ts, forecast_best_20 = fcs_combined_models_best_20,
         forecast_best_10_and_negative_correlated = fcs_combined_neg_corr_vars) %>%  
  tidyr::gather(key = "model", value = "forecast", -index) %>% as.data.frame() %>% na.omit() 

# only_fcs_tbl <- final_fcs_tbl_long[13:18, ] # only select the forecasts and not rgdp
# Instead of using numbers we select the models by name using dplyr filter and grepl. 
# I do this because the position is different for each country whereas the names are constant. So in this way it works for each country
only_fcs_tbl <- final_fcs_tbl_long %>% dplyr::filter(grepl('forecast_arimax|forecast_best_20|forecast_best_10_and_negative_correlated', model))
only_fcs_tbl$forecast <- round(only_fcs_tbl$forecast, digits = 3)

plot_annual_forecast_title <- paste("Annual GDP Forecasts", country_name, sep = " ")
filename_annual_forecast_plot <- paste(plot_annual_forecast_title, "png", sep = ".")
# position dodge is used so that the bars are next to each other and not on top of each other
# figure out how to control spacing between observations
# figure out how to get values on the last bars of the forecasts
annual_forecast_bar_plot <- ggplot(final_fcs_tbl_long, aes(x = index, y = forecast, fill = model)) + 
  geom_col(width=0.8, position = "dodge") +
  geom_text(data = only_fcs_tbl, aes(label = forecast, fontface = "bold"), 
            position = position_dodge(width=1), size=2, vjust = -1) +
  theme_cepal_fcs_bar_plot +
  scale_fill_brewer(palette = "BrBG") + 
  ggtitle(plot_annual_forecast_title) +
  guides(fill=guide_legend(title="Time-Series:")) +
  scale_y_continuous(name = "Real GDP Growth YoY (%)", 
                     breaks=c(-0.03, -0.02, -0.01, 0.00, 0.01, 0.02, 0.03, 0.04, 
                              0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) + 
  scale_x_yearmon(name = "Year", n = 15, format = "%Y") +
  annotation_custom(ymin= -0.02, ymax= 0.00, xmin = 2018.00, xmax = 2020.00, rasterGrob(watermark_cepal))
  

ggsave(filename = filename_annual_forecast_plot, path = plots_path)

annual_forecast_bar_plot



