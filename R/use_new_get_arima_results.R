source('./R/utils_av.R')


country_name <- "Ecuador"



# this is the "stata" option (demetra + always include a constant even if D+d = 2). Approx 20 sec in my PC. ARG 30 sec
tic()
arima_res <- get_arima_results(country_name = country_name, 
                               use_demetra = TRUE, 
                               use_dm_force_constant = TRUE, 
                               arima_res_suffix = "_dm_force_const", 
                               is_log_log = TRUE)
toc()


# this is the "demetra" option (demetra, which does not include a constant when D+d = 2). Approx 20 sec in my PC
tic()
arima_res <- get_arima_results(country_name = country_name, 
                               use_demetra = TRUE, 
                               use_dm_force_constant = FALSE, 
                               arima_res_suffix = "_dm_do_not_force_const", 
                               is_log_log = TRUE)
toc()


# this uses auto.arima for everything. Approx 220 sec in my PC, ARG 480 sec.
tic()
arima_res <- get_arima_results(country_name = country_name,
                                    use_demetra = FALSE,
                                    use_dm_force_constant = FALSE,
                                    arima_res_suffix = "_auto_r",
                                    is_log_log = TRUE)
toc()







# if you are curious these are the default values (h_max, n_cv etc)
  
  # get_arima_results <- function(country_name, read_results = FALSE, 
  #                               data_folder = "./data/excel/",
  #                               arima_rds_path = "data/sarimax_objects_",
  #                               h_max = 8, final_ext_horizon = c(2019, 12),
  #                               train_span = 16, number_of_cv = 8,
  #                               test_length = 8, use_demetra = TRUE, 
  #                               do_auto = FALSE, use_dm_force_constant = FALSE,
  #                               is_log_log = TRUE, lambda_0_in_auto = FALSE,
  #                               mean_logical_in_auto = TRUE, max_x_lag = 2,
  #                               external_data_path = "./data/external/external.xlsx",
  #                               arima_res_suffix = "foo") 
  # 
  # 