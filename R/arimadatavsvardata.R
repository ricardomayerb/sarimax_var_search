source('./R/utils_av.R')


final_forecast_horizon <- c(2019, 12)
h_max = 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16

data_path <- "./data/excel/Chile.xlsx"

gdp_and_dates <- get_rgdp_and_dates(data_path)

monthly_data <- get_monthly_variables(data_path = data_path)

monthly_ts <- make_monthly_ts(monthly_data)
monthly_ts  <- log(monthly_ts)
full_monthly_ts  <- monthly_ts

monthly_names <- colnames(monthly_ts)

arima_level_rgdp_ts <- ts(data = gdp_and_dates[["gdp_data"]], 
              start = gdp_and_dates[["gdp_start"]], frequency = 4)
arima_log_rgdp_ts <- log(rgdp_ts)


country_data_level_ts <- get_raw_data_ts()
all_rgdp_level_ts <- country_data_level_ts %>% 
  map(~ .[, "rgdp"]) %>% reduce(ts.union)
colnames(all_rgdp_level_ts) <- names(country_data_level_ts)

var_level_rgdp_ts <- na.omit(all_rgdp_level_ts[, "Chile"])

var_level_rgdp_ts - arima_level_rgdp_ts

var_yoy_rgdp_ts <- make_yoy_ts(var_level_rgdp_ts)
arima_yoy_rgdp_ts <- make_yoy_ts(exp(arimloa_g_rgdp_ts))



