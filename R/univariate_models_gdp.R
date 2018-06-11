
##---- load_libs_sources ----
source('./R/utils_av.R')
library(fpp2)

##---- get_data ----

country_data_level_ts <- get_raw_data_ts()
all_rgdp_level_ts <- country_data_level_ts %>% 
  map(~ .[, "rgdp"]) %>% reduce(ts.union)
colnames(all_rgdp_level_ts) <- names(country_data_level_ts)

chl_rgdp_level_ts <- all_rgdp_level_ts[, "Chile"]

##---- obs_and_dates ----

dates_and_obs_vbl <- function(data_list, variable = "rgdp") {
  
  datesobs_col <- function(rgdp_ts) {
    start_date <- as.yearqtr(min(time(na.omit(rgdp_ts))))
    end_date <- as.yearqtr(max(time(na.omit(rgdp_ts))))
    n_obs <- length(na.omit(rgdp_ts))
    d_vec <- c(as.character(start_date), as.character(end_date), as.character(n_obs))
    return(d_vec)
  }
  
  dates_obs_rgdp <- country_data_level_ts %>% 
    map(~ .[, variable]) %>% 
    map(datesobs_col) %>% 
    reduce(cbind) %>% t() %>% 
    as_data_frame() 
  
  dates_obs_rgdp <- as_tibble(cbind(names(country_data_level_ts), dates_obs_rgdp))
  names(dates_obs_rgdp) <-   c("country", "start", "end", "obs")
  return(dates_obs_rgdp)
}

dates_rgdp_all <- dates_and_obs_vbl(country_data_level_ts)

##---- rgdp_mts_as_tbl ----
chl_rgdp_log_ts <- log(chl_rgdp_level_ts)

##---- rgdp_data_plots ----




