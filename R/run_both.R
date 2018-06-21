source('./R/utils_av.R')

data_path <- "./data/excel/Chile.xlsx"
external_data_path <- "./data/external/external.xlsx"
final_forecast_horizon <- c(2019, 12)
h_max = 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16

arimax_data <- function(data_path, external_data_path, monthly_param_list = NULL,
                        external_monthly_param_list = NULL, do_auto = TRUE, 
                        auto_stepwise = FALSE, auto_bias_adj = TRUE,
                        do_box_cox = FALSE, auto_lambda = 0) {
  
}





