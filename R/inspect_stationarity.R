source("./R/utils_av.R")

level_data_ts <- get_data(country_name = "Brasil",
                             data_transform = "level", apply_log = FALSE)

level_rgdp_ts <- level_data_ts[ , "rgdp"]

this_series <- level_rgdp_ts


tests_of_stationarity_a <- suppressWarnings(comb_ndiffs(this_series = level_rgdp_ts, return_4_seas = TRUE))
stati_a <- tests_of_stationarity_a$stationarity
seas_a <- tests_of_stationarity_a$seas

yoy_data_ts <- get_data(country_name = "Brasil",
                          data_transform = "yoy", apply_log = FALSE)



print(stati_a)


### --- let's test it with more varied results

fake_row_1 <- tibble(test = "fake1", alpha = 0.05, deter_part = "trend",
                     default_seas = 0, sta_result = 0, sta_result_after_seas = 0)

fake_row_2 <- tibble(test = "fake2", alpha = 0.05, deter_part = "trend",
                     default_seas = 0, sta_result = 1, sta_result_after_seas = 1)

fake_row_3 <- tibble(test = "fake3", alpha = 0.05, deter_part = "trend",
                     default_seas = 1, sta_result = 2, sta_result_after_seas = 1)

moo <- rbind(fake_row_1, fake_row_2, fake_row_3, select(stati_a, -recommendation)) 
  
foo <- moo %>% 
  mutate(recommedation = pmap_chr(list(default_seas, sta_result, sta_result_after_seas),
                         ~ make_recommendation(seas = ..1, sta = ..2, 
                                    sta_after_seas = ..3))
         )


print(foo)
