source('./R/utils_av.R')

country_name <- "Peru"

final_forecast_horizon <- c(2020, 12)
h_max = 8 # last rgdp data is 2017 Q4
number_of_cv = 8
train_span = 16
arima_res <- get_arima_results(country_name = country_name, h_max = 8,
                               number_of_cv = 8, train_span = 16,
                               final_ext_horizon = final_forecast_horizon)

# arima_res <- get_arima_results(country_name = country_name, read_results = TRUE)

# rmse_yoy_sarimax <- arima_res$compare_rmse_yoy
# rmse_level_sarimax <- arima_res$compare_rmse
# v_lags_order_season <- arima_res$var_lag_order_season 
extended_x_data_ts <- arima_res$mdata_ext_ts
rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima

vlos <- arima_res[["var_lag_order_season"]]
#### VAR and sarimax together -------------- 


models_and_accu <- readRDS("./data/Brasil_by_step_12345.rds")
cv_objects <- readRDS("./data/Brasil_by_step_12345_cv_objects.rds")
VAR_data <- readRDS("./data/VAR_data_Brasil.rds")

h_max <- 6


models_tbl <- make_models_tbl(
  arima_res = arima_res, var_models_and_rmse = models_and_accu, VAR_data = VAR_data,
  h_max = h_max)


models_tbl_ssel <- make_models_tbl(arima_res, var_models_and_rmse = models_and_accu, VAR_data = VAR_data,
h_max = h_max, ave_rmse_sel = TRUE)


ffall <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                           h = h_max, extended_x_data_ts = extended_x_data_ts,
                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                           max_rank_h = 30)
summ_all <- ffall %>% 
    group_by(horizon) %>%
    summarise(sum_one_h = reduce(one_model_w_fc, sum))


ffall_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                            h = h_max, extended_x_data_ts = extended_x_data_ts,
                            rgdp_ts_in_arima = rgdp_ts_in_arima,
                            max_rank_h = 30)
summ_all_ssel <- ffall_ssel %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



ffall_20 <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                            h = h_max, extended_x_data_ts = extended_x_data_ts,
                            rgdp_ts_in_arima = rgdp_ts_in_arima,
                            max_rank_h = 20)
summ_all_20 <- ffall %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))




ffall_VAR <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                           h = h_max, extended_x_data_ts = extended_x_data_ts,
                           rgdp_ts_in_arima = rgdp_ts_in_arima,
                           model_type = "VAR", max_rank_h = 30)
summ_all_VAR <- ffall_VAR %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



ffall_arima <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl,
                                h = h_max, extended_x_data_ts = extended_x_data_ts,
                                rgdp_ts_in_arima = rgdp_ts_in_arima,
                                model_type = "Arima")

summ_all_arima <- ffall_arima %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))


aricred <- ffall_arima %>% filter(variables == "cred") %>% select(variables, lags, fit)
aricred0 <- aricred$fit[[1]] 
aricred0$x
aricred0$xreg

cred_for_fc <-  window(extended_x_data_ts[, "cred"], start = c(2018, 1))
cred_in_ari <- window(extended_x_data_ts[, "cred"], start = c(2000, 1), end = c(2017,4))
cred_long <- window(extended_x_data_ts[, "cred"], start = c(2000, 1), end = c(2019,4))
cred_in_ari_adj <- window(extended_x_data_ts[, "cred"], start = c(2001, 4), end = c(2017,4))
cred_in_ari_miss <- ts(c(NA,NA,NA,NA,NA,NA,NA,cred_in_ari_adj), frequency = 4, start = c(2000, 1)) 
cred_in_ari_miss_long <- ts(c(cred_in_ari_miss, cred_for_fc), frequency = 4, start = c(2000, 1))

aricred0_miss <- Arima(y = rgdp_ts_in_arima, order = c(0,1,0), seasonal = c(0,1,2), xreg = cred_in_ari_miss)
aricred0_nan <- Arima(y = rgdp_ts_in_arima, order = c(0,1,0), seasonal = c(0,1,2), xreg = cred_in_ari)

fccred0_miss <- forecast(aricred0_miss, xreg = cred_for_fc, h = 8)
ext_rgdp_miss <- ts(c(rgdp_ts_in_arima, fccred0_miss$mean), frequency = 4, 
               start = stats::start(rgdp_ts_in_arima))
logdiff_aricred0_miss <- diff(ext_rgdp_miss, lag = 4)
logdiff_aricred0_miss

fccred0 <- forecast(aricred0, xreg = cred_for_fc, h = 8)
ext_rgdp <- ts(c(rgdp_ts_in_arima, fccred0$mean), frequency = 4, 
               start = stats::start(rgdp_ts_in_arima))
logdiff_aricred0 <- diff(ext_rgdp, lag = 4)
logdiff_aricred0
# from stata 2.858734131	2.899551392	3.041744232	3.258323669	2.708339691	2.666759491	2.62556076	2.58436203

mygdpari <- Arima(y = rgdp_ts_in_arima, order = c(0,1,0), seasonal = c(0,1,1))
mygdpari_ml <- Arima(y = rgdp_ts_in_arima, order = c(0,1,0), seasonal = c(0,1,1), method = "ML")

mygdpari_stats <- arima(x = rgdp_ts_in_arima, order = c(0,1,0), seasonal = list(order = c(0,1,1), period = 4))
mygdpari_stats_nomean <- arima(x = rgdp_ts_in_arima, order = c(0,1,0), seasonal = list(order = c(0,1,1), period = 4), include.mean = FALSE)

mygdpari_constant <- Arima(y = rgdp_ts_in_arima, order = c(0,1,0), seasonal = c(0,1,1), include.constant = TRUE)
mygdpari_mean <- Arima(y = rgdp_ts_in_arima, order = c(0,1,0), seasonal = c(0,1,1), include.mean = TRUE)

mygdpari
mygdpari_constant
mygdpari_mean
mygdpari_stats
mygdpari_stats_nomean



fc_rgdp_simple <- forecast(mygdpari, h = 8)
fc_rgdp_simple$mean
ext_gdp_simple <- ts(c(rgdp_ts_in_arima, fc_rgdp_simple$mean), frequency = 4, start = c(2000, 1))
logdiff_ext_gdp_simple <- diff(ext_gdp_simple, lag = 4)
logdiff_ext_gdp_simple
# 2.858734131	3.093719482	3.314876556	3.748607635	3.42092514	3.417682648	3.414344788	3.411102295

# stata:  11.782642 vs 11.78291 in R

gdp_and_cred <- ts.union(rgdp_ts_in_arima, cred_in_ari_miss, cred_in_ari_miss_long)
gdp_and_cred_tbl <- tk_tbl(gdp_and_cred)

# library(openxlsx)
# write.xlsx(gdp_and_cred_tbl, "./data/exported_rgdp_cred.xlsx")
# library(haven)
# write_dta(gdp_and_cred_tbl, "./data/exported_rgdp_cred.dta")


ffall_arima_ssel <- indiv_weigthed_fcs(tbl_of_models_and_rmse = models_tbl_ssel,
                                  h = h_max, extended_x_data_ts = extended_x_data_ts,
                                  rgdp_ts_in_arima = rgdp_ts_in_arima,
                                  model_type = "Arima")

summ_all_arima_ssel <- ffall_arima_ssel %>% 
  group_by(horizon) %>%
  summarise(sum_one_h = reduce(one_model_w_fc, sum))



m_analysis_path <- paste0("data/", country_name,"_m_analysis_rgdp.xlsx")

m_analysis_rgdp <- read_excel(m_analysis_path)

m_analysis_variables_lags <- m_analysis_rgdp %>% 
  select(cond_exo) %>% 
  mutate_if(is.double, function(x) 0.01 * x) %>% 
  mutate(pre_variable = str_remove(cond_exo, "^(.*?)\\.l"),
         variable = str_extract(pre_variable, "([^\\s]+)"),
         lag = ifelse(str_detect(pre_variable, "L"), 
                      ifelse(str_detect(pre_variable, "L2"), 2, 1) , 0)
  ) %>% 
  select(-pre_variable) %>% 
  mutate(variable = ifelse(variable == "_NONE", "rgdp", variable))

m_analysis_rmses <-  m_analysis_rgdp %>% 
  select("cond_exo", starts_with("rmse")) %>% 
  mutate_if(is.double, function(x) 0.01 * x) %>% 
  left_join(m_analysis_variables_lags, by = "cond_exo")


m_analysis_cond_fcs <-  m_analysis_rgdp %>% 
  select("cond_exo", starts_with("gr_cond")) %>% 
  mutate_if(is.double, function(x) 0.01 * x) %>% 
  left_join(m_analysis_variables_lags, by = "cond_exo")


bsarimax_ind_fcs <- arima_res[["all_raw_fcs"]] %>% 
  mutate(diff_fc_mean = map(raw_rgdp_fc, 
                            ~ logyoy(logfc_ts = ., log_data_ts = rgdp_ts_in_arima))
  ) %>% 
  rename(variable = id_fc) 

my_log_diff_fc <- reduce(bsarimax_ind_fcs[["diff_fc_mean"]], rbind)
dimnames(my_log_diff_fc) <- NULL
my_log_diff_fc <- as_tibble(my_log_diff_fc)
names(my_log_diff_fc) <- paste0("h_", 1:ncol(my_log_diff_fc))

my_log_diff_fc <- cbind(bsarimax_ind_fcs[,c("variable", "lag")], 
                        my_log_diff_fc) %>% 
  arrange(variable, lag)

my_log_diff_fc_long <- my_log_diff_fc %>% 
  gather(key = h, value = my_fc, -c(variable, lag))

m_analysis_cond_fcs_long <- m_analysis_cond_fcs %>% 
  select(-c(cond_exo, gr_cond18, gr_cond19)) %>% 
  gather(key = h, value = s_fc, -c(variable, lag)) %>% 
  mutate(h = recode(h, 
                    gr_cond181 = "h_1", gr_cond182 = "h_2", gr_cond183 = "h_3",
                    gr_cond184 = "h_4", gr_cond191 = "h_5", gr_cond192 = "h_6",
                    gr_cond193 = "h_7", gr_cond194 = "h_8"))

both_fcs <- left_join(my_log_diff_fc_long, m_analysis_cond_fcs_long, 
                      by = c("variable", "lag", "h")) 


my_construction <- extended_x_data_ts[,"construction"]
my_construction <- window(extended_x_data_ts[,"construction"],
                          start = c(2000, 1), end = c(2019, 4))
my_construction_est <- window(extended_x_data_ts[,"construction"],
                          start = c(2000, 1), end = c(2017, 4))
my_construction_fc <- window(extended_x_data_ts[,"construction"],
                              start = c(2018, 1), end = c(2019, 4))

vlos <- arima_res[["var_lag_order_season"]]
vlos_rgdp <- filter(vlos, variable == "rgdp")

arimax_construction <- Arima(y = rgdp_ts_in_arima, xreg = my_construction_est,
                             order = unlist(vlos_rgdp$arima_order), 
                             seasonal = unlist(vlos_rgdp$arima_seasonal))

fc_construction <- forecast(arimax_construction, h = 8,
                            xreg = my_construction_fc)


