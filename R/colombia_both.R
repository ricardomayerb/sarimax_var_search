source('./R/utils_av.R')
library(tictoc)


#### bsarima part -------------- 

final_forecast_horizon <- c(2019, 12)
h_max = 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16

country_name <- "Colombia"
data_path <- paste0("./data/excel/", country_name,".xlsx")
m_analysis_path <- paste0("data/", country_name,"_m_analysis_rgdp.xlsx")
rds_file_name = paste0("data/sarimax_objects_", country_name,".rds")

tic()
myres <- bsarimax_as_function(data_path = data_path, number_of_cv = number_of_cv,
                              train_span = train_span, h_max = h_max,
                              final_forecast_horizon = final_forecast_horizon,
                              outer_cv_round = 0, s4xreg = FALSE)
toc()

print(names(myres))


