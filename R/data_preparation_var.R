source("./R/utils_av.R")


country_name <- "Brasil"

h_max = 6
#################################### Load Data #######################################

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
                           Peru = c("expec_demand", "", "", "", "", "", "", "", "", "", "", ""),
                           Uruguay = c("cred", "", "", "", "", "", "", "", "", "", "", ""))

variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)

data_qm_xts <- get_gdp_shaped_data(data_path = data_path, 
                                       list_variables_to_drop = variables_to_drop,
                                       only_complete_cases = TRUE,
                                       apply_log = FALSE)

data_qm_mts <- map(data_qm_xts, to_ts_q)

data_qm_xts_yoy <- map(data_qm_xts, make_yoy_xts)
data_qm_mts_yoy <- map(data_qm_xts_yoy, to_ts_q)

data_qm_xts_yoy_diff <- map(data_qm_xts_yoy, diff.xts, na.pad = FALSE)
data_qm_mts_yoy_diff <- map(data_qm_xts_yoy_diff, to_ts_q)

level_data_ts <- data_qm_mts[[country_name]]
yoy_data_ts <- data_qm_mts_yoy[[country_name]]
diff_yoy_data_ts <- data_qm_mts_yoy_diff[[country_name]]

level_rgdp_ts <- level_data_ts[ , "rgdp"]
