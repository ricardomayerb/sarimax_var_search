source('./R/utils_av_ricardo.R')
library(scales)

arima_res_suffix <- "_foo"
arima_rds_path = "data/sarimax_objects_"
country_name <- "Peru"

data_path <- paste0("./data/excel/", country_name, ".xlsx")
external_data_path <- "./data/external/external.xlsx"
final_forecast_horizon <- c(2019, 12)
is_log_log <- TRUE

gdp_and_dates <- get_rgdp_and_dates(data_path)


all_arima_data <- ts_data_for_arima(data_path = data_path, 
                                    external_data_path = external_data_path,
                                    all_logs = is_log_log)
names(all_arima_data)

foo <- all_arima_data$lead_q
fooe <- all_arima_data$lead_q_external
foo
fooe
