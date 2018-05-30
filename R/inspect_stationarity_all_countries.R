source("./R/utils_av.R")

country_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia",
                   "Ecuador", "Mexico", "Paraguay", "Peru", "Uruguay")

all_countries_data <- get_raw_data_ts(country = NULL)
stationarity_list <- list_along(country_names)
sta_reco_list <- list_along(country_names)

tictoc::tic()
for (i in seq_along(country_names)) {
  country_name <- country_names[i]
  this_level_data_ts <- all_countries_data[[country_name]]
  names_of_variables <- colnames(this_level_data_ts)

  for (j in seq_along(names_of_variables)) {
    this_variable <- names_of_variables[j]
    this_variable_ts <- this_level_data_ts[ , this_variable]
    tests_of_stationarity <- suppressWarnings(comb_ndiffs(this_variable_ts))
    tests_of_stationarity$country <- country_name
    tests_of_stationarity$variable <- this_variable
    
    reco <- get_reco_from_sta(tests_of_stationarity, this_variable)
    
    stationarity_list[[i]][[j]] <- tests_of_stationarity
    sta_reco_list[[i]][[j]] <- reco
    
  }
  
}
tictoc::toc()


reco_by_country_list <- map(sta_reco_list,  ~ reduce(., rbind))
names(reco_by_country_list) <- country_names
reco_all_countries_tbl  <- map_df(sta_reco_list,  ~ reduce(., rbind))


this_country <- "Chile"
country_rec <- reco_by_country_list[[this_country]]
country_data <- all_countries_data[[this_country]]

table_of_recommendations <- country_rec
data_tbl_ts <- country_data
country_transformed_data <- follow_rec(data_tbl_ts, table_of_recommendations)

country_transformed_data_naom <- na.omit(country_transformed_data)


# 
# # altertive way. Inneficiente bc even for a single country it always compute series for all countries
# data_path <- "./data/excel/"
# fn_level_data_ts <- get_data(data_path = data_path, country_name = "Chile",
#                              data_transform = "level", apply_log = FALSE)
# 
# # way to go if we wont need data from other countries
# chile_by_other_way <- get_raw_data_ts(country = "Chile")






# 

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
