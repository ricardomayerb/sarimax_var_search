source("./R/utils_av.R")

# Ecuador causes an error
level_data_ts <- get_data(country_name = "Uruguay",
                             data_transform = "level", apply_log = FALSE)
level_rgdp_ts <- level_data_ts[ , "rgdp"]
this_series <- level_rgdp_ts

tests_of_stationarity_a <- suppressWarnings(comb_ndiffs(level_rgdp_ts))


# print(stati_a)

# Ecuador causes an error
# country_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia",
#                    "Ecuador", "Mexico", "Paraguay", "Peru", "Uruguay")
country_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia",
                   "Mexico", "Paraguay", "Peru", "Uruguay")

all_countries_data <- get_data(country_name = "all",
                               data_transform = "level")

stationarity_list <- list_along(country_names)
tictoc::tic()
for (i in seq_along(country_names)) {
  country_name <- country_names[i]
  level_data_ts <- all_countries_data [[country_name]]
  
  level_rgdp_ts <- level_data_ts[ , "rgdp"]
  tests_of_stationarity <- suppressWarnings(comb_ndiffs(level_rgdp_ts))
  tests_of_stationarity$country <- country_name
  
  stationarity_list[[i]] <- tests_of_stationarity
  
}
tictoc::toc()

all_countries_rgdp_stati <- reduce(stationarity_list, rbind)




# 
# ### --- let's test it with more varied results
# 
# fake_row_1 <- tibble(test = "fake1", alpha = 0.05, deter_part = "trend",
#                      default_seas = 0, sta_result = 0, sta_result_after_seas = 0)
# 
# fake_row_2 <- tibble(test = "fake2", alpha = 0.05, deter_part = "trend",
#                      default_seas = 0, sta_result = 1, sta_result_after_seas = 1)
# 
# fake_row_3 <- tibble(test = "fake3", alpha = 0.05, deter_part = "trend",
#                      default_seas = 1, sta_result = 2, sta_result_after_seas = 1)
# 
# moo <- rbind(fake_row_1, fake_row_2, fake_row_3, select(stati_a, -recommendation)) 
#   
# foo <- moo %>% 
#   mutate(recommedation = pmap_chr(list(default_seas, sta_result, sta_result_after_seas),
#                          ~ make_recommendation(seas = ..1, sta = ..2, 
#                                     sta_after_seas = ..3))
#          )
# 
# 
# print(foo)
