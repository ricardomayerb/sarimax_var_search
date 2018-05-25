source("./R/utils_av.R")

# Ecuador causes an error
level_data_ts <- get_data(country_name = "Ecuador",
                          data_transform = "level", apply_log = FALSE)
level_rgdp_ts <- level_data_ts[ , "rgdp"]
this_series <- level_rgdp_ts

tests_names <- c("kpss", "pp", "adf")
tests_season_names <- c("seas", "ocsb", "hegy", "ch")
tests_alpha <- c(0.01, 0.05, 0.1)
tests_type <- c("level", "trend")

do_stati_after_seas <- TRUE

seas_and_sta <- as_tibble(
  expand.grid(tests_names, tests_alpha, tests_type,
              stringsAsFactors = FALSE)) %>% 
  rename(test = Var1, alpha = Var2, deter_part = Var3) %>% 
  mutate(default_seas = map_dbl(alpha,
                                ~ nsdiffs(x = this_series, alpha = .)),
         sta_result = pmap_dbl(list(test, alpha, deter_part),
                               ~ ndiffs(x = this_series, alpha = ..2,
                                        test = ..1, type = ..3))
  )

# print(seas_and_sta[1,])
# ndiffs(diff(this_series, lag = 4, differences = 0), alpha = 0.01, test = "kpss", type = "level")

         
seas_sta_and_staaseas <- seas_and_sta %>% 
  mutate(sta_result_after_seas = pmap_dbl(
    list(test, alpha, deter_part, default_seas),
    ~ ndiffs(x = my_diff(this_series, differences = ..4, lag = 4), 
             alpha = ..2, test = ..1, type = ..3, max.d = 2))
  )

with_recommendation <-  seas_sta_and_staaseas %>% 
  mutate(recommendation = pmap_chr(
    list(default_seas, sta_result, sta_result_after_seas),
    ~ make_recommendation(seas = ..1, sta = ..2, sta_after_seas = ..3)
  )
    
  )
