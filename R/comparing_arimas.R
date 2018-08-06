source('./R/utils_av_ricardo.R')

seas_yr_growth <- function(data_ts, freq = 4 , is_log = TRUE, 
                           do_just_log = FALSE, year_1 = 2016, year_2 = 2017) {
  
  seas_gr <- tsibble::as_tsibble(data_ts)
  
  if (is_log) {
    seas_gr  <- seas_gr %>% 
      mutate(difflog_gr =  100 * difference(value, lag = freq))
    
    year_gr <- seas_gr %>% index_by(year_index = year(index)) %>%
      summarise(y_logdiff = mean(difflog_gr, na.rm = TRUE)) 
    
    if (freq == 12) {
      quarter_gr <- seas_gr %>% index_by(quarter_index = yearquarter(index)) %>%
        summarise(q_logdiff = mean(difflog_gr, na.rm = TRUE)) 
    }
    
    if (! do_just_log) {
      seas_gr  <-seas_gr  %>% 
        mutate(gr = 100 * difference(exp(value), lag = freq) / 
                 dplyr::lag(exp(value), n = freq))
      
      year_gr <- seas_gr %>% index_by(year_index = year(index)) %>%
        summarise(y_logdiff = mean(difflog_gr, na.rm = TRUE),
                  y_ave_gr = mean(gr, na.rm = TRUE),
                  y_rgdp = sum(exp(value), na.rm = TRUE))  %>%
        mutate(y_tot_gr = 100*difference(y_rgdp)/lag(y_rgdp)) 
      
      if (freq == 12) {
        quarter_gr <- seas_gr %>% index_by(quarter_index = yearquarter(index)) %>%
          summarise(q_logdiff = mean(difflog_gr, na.rm = TRUE),
                    q_ave_gr = mean(gr, na.rm = TRUE),
                    q_rgdp = sum(exp(value), na.rm = TRUE))  %>%
          mutate(q_tot_gr = 100*difference(q_rgdp, lag = 4)/lag(q_rgdp, n = 4))
        
      }
    }
    
  } else {
    seas_gr  <-seas_gr  %>% 
      mutate(gr = 100 * difference(value, lag = freq) / 
               dplyr::lag(value, n = freq))
    
    year_gr <- seas_gr %>% index_by(year_index = year(index)) %>% 
      summarise(y_ave_gr = mean(gr, na.rm = TRUE),
                y_rgdp = sum(value)) %>%
      mutate(y_tot_gr = 100*difference(y_rgdp)/lag(y_rgdp))
    
    if (freq == 12) {
      quarter_gr <- seas_gr %>% index_by(quarter_index = yearquarter(index)) %>% 
        summarise(q_ave_gr = mean(gr, na.rm = TRUE),
                  q_rgdp = sum(value)) %>%
        mutate(q_tot_gr = 100*difference(q_rgdp, lag = 4)/lag(q_rgdp, n = 4))
    }
    
  }
  
  
  if (freq == 4) {
    q_y1_logic <- year(seas_gr$index) >= year_1 
    q_y2_logic <- year(seas_gr$index) <= year_2
    q_y1_y2_logic <- q_y1_logic & q_y2_logic
    seas_gr_y1_y2 <- seas_gr %>% filter(q_y1_y2_logic)
    
    y_y1_logic <- year_gr$year_index >= year_1
    y_y2_logic <- year_gr$year_index <= year_2
    y_y1_y2_logic <- y_y1_logic & y_y2_logic
    year_gr_y1_y2 <- year_gr %>% filter(y_y1_y2_logic)
    
    return(list(seasonal_growth = seas_gr,
                year_growth = year_gr,
                seasonal_growth_y1_y2 = seas_gr_y1_y2,
                year_growth_y1_y2 = year_gr_y1_y2))
  }
  
  if (freq == 12) {
    m_y1_logic <- year(seas_gr$index) >= year_1 
    m_y2_logic <- year(seas_gr$index) <= year_2
    m_y1_y2_logic <- m_y1_logic & m_y2_logic
    seas_gr_y1_y2 <- seas_gr %>% filter(m_y1_y2_logic)

    
    q_y1_logic <- year(quarter_gr$quarter_index) >= year_1
    q_y2_logic <- year(quarter_gr$quarter_index) <= year_2
    q_y1_y2_logic <- q_y1_logic & q_y2_logic
    quarter_gr_y1_y2 <- quarter_gr %>% filter(q_y1_y2_logic)

    
    y_y1_logic <- year_gr$year_index >= year_1
    y_y2_logic <- year_gr$year_index <= year_2
    y_y1_y2_logic <- y_y1_logic & y_y2_logic
    year_gr_y1_y2 <- year_gr %>% filter(y_y1_y2_logic)
    
    return(list(seasonal_growth = seas_gr, 
                quarter_growth = quarter_gr,
                year_growth = year_gr,
                seasonal_growth_y1_y2 = seas_gr_y1_y2, 
                quarter_growth_y1_y2 = quarter_gr_y1_y2,
                year_growth_y1_y2 = year_gr_y1_y2
                ))
  }
  
  
  
}

country_name <- "Argentina"
arima_rds_path = "data/sarimax_objects_"

arima_res_suffix <- "_dm_s"
rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
dm_s <- readRDS(file = rds_file_name)

arima_res_suffix <- "_dm_s_fsv"
rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
dm_s_fsv <- readRDS(file = rds_file_name)

arima_res_suffix <- "_dm_r"
rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
dm_r <- readRDS(file = rds_file_name)

arima_res_suffix <- "_dm_r_fsv"
rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
dm_r_fsv <- readRDS(file = rds_file_name)

arima_res_suffix <- "_auto"
rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
auto <- readRDS(file = rds_file_name)

arima_res_suffix <- "_auto_fsv"
rds_file_name = paste0(arima_rds_path, country_name, arima_res_suffix, ".rds")
auto_fsv <- readRDS(file = rds_file_name)


data_and_fc_unc_dm_s <- dm_s$rgdp_data_and_uncond_fc
data_and_fc_unc_dm_r <- dm_r$rgdp_data_and_uncond_fc
data_and_fc_unc_auto <- auto$rgdp_data_and_uncond_fc

autoplot(data_and_fc_unc_dm_s) + 
  autolayer(data_and_fc_unc_dm_r) +
  autolayer(data_and_fc_unc_auto) 

yoy_data_and_fc_unc_dm_s <- dm_s$yoy_rgdp_data_and_uncond_fc
yoy_data_and_fc_unc_dm_r <- dm_r$yoy_rgdp_data_and_uncond_fc
yoy_data_and_fc_unc_auto <- auto$yoy_rgdp_data_and_uncond_fc

autoplot(yoy_data_and_fc_unc_dm_s) + 
  autolayer(yoy_data_and_fc_unc_dm_r) +
  autolayer(yoy_data_and_fc_unc_auto) 

gr_qrt_data_and_fc_unc_dm_s_2018_2019 <- seas_yr_growth(data_and_fc_unc_dm_s,
                                                        year_1 = 2018, year_2 = 2019)[["seasonal_growth_y1_y2"]]
gr_qrt_data_and_fc_unc_dm_r_2018_2019 <- seas_yr_growth(data_and_fc_unc_dm_r,
                                                        year_1 = 2018, year_2 = 2019)[["seasonal_growth_y1_y2"]]
gr_qrt_data_and_fc_unc_auto_2018_2019 <- seas_yr_growth(data_and_fc_unc_auto,
                                                        year_1 = 2018, year_2 = 2019)[["seasonal_growth_y1_y2"]]


gr_yr_data_and_fc_unc_dm_r_2018_2019 <- seas_yr_growth(data_and_fc_unc_dm_r,
                                                       year_1 = 2018, year_2 = 2019)[["seasonal_growth_y1_y2"]]
gr_yr_data_and_fc_unc_dm_s_2018_2019 <- seas_yr_growth(data_and_fc_unc_dm_s,
                                                       year_1 = 2018, year_2 = 2019)[["seasonal_growth_y1_y2"]]
gr_yr_data_and_fc_unc_auto_2018_2019 <- seas_yr_growth(data_and_fc_unc_auto,
                                                       year_1 = 2018, year_2 = 2019)[["seasonal_growth_y1_y2"]]


gr_qrt_201819_3_arimas <- dplyr::left_join(gr_qrt_data_and_fc_unc_dm_s, 
                                           gr_qrt_data_and_fc_unc_dm_r,
                                           by = "index", 
                                           suffix = c("_dm_s", "_dm_r")) %>% 
  left_join(gr_qrt_data_and_fc_unc_auto, by = "index") %>% 
  select(index, gr_dm_s, gr_dm_r, gr) %>% 
  filter(year(index) >= 2018, year(index) <= 2019)



gr_yr_201819_3_arimas <- dplyr::left_join(gr_yr_data_and_fc_unc_dm_s, gr_yr_data_and_fc_unc_dm_r,
                                   by = "year_index", suffix = c("_dm_s", "_dm_r")) %>% 
  left_join(gr_yr_data_and_fc_unc_auto, by = "year_index") %>% 
  select(year_index, y_ave_gr_dm_s, y_ave_gr_dm_r, y_ave_gr) %>% 
  filter(year_index >= 2018, year_index <= 2019)

print(gr_qrt_201819_3_arimas)

print(gr_yr_201819_3_arimas)  



