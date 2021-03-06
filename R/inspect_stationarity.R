source("./R/utils_av.R")


one_country_name <- "Chile"
one_variable_name <- "rgdp"
one_data_transform <- "level"

# doo_chile_ts <- get_raw_data_ts(country = "Chile")
# doo_chile_ts[,8]
# 
# doo_all_ts <-  get_raw_data_ts(country = NULL)
# 
# boo <- doo_all_ts[["Brasil"]]
# boo[, 8]

# level_data_ts <- get_data(country_name = country_name,
#                              data_transform = data_transform, apply_log = FALSE)

one_level_data_ts <- get_raw_data_ts(country = one_country_name)


one_level_rgdp_ts <- one_level_data_ts[ , one_variable_name]
# this_series <- level_rgdp_ts

tests_of_stationarity_e <- suppressWarnings(comb_ndiffs(one_level_rgdp_ts)) %>% 
  arrange(test, deter_part, alpha)

tests_of_stationarity_es <- suppressWarnings(
  comb_ndiffs(one_level_rgdp_ts, do_other_seas = TRUE, return_4_seas = TRUE)) 
seas_ecu <- tests_of_stationarity_es$seas

country_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia",
                   "Ecuador", "Mexico", "Paraguay", "Peru", "Uruguay")

# all_countries_data <- get_data(country_name = "all",
#                                data_transform = "level")

all_countries_data <- get_raw_data_ts(country = NULL)

stationarity_list <- list_along(country_names)
sta_reco_list <- list_along(country_names)


tictoc::tic()
for (i in seq_along(country_names)) {
  country_name <- country_names[i]
  # print(country_name)
  this_level_data_ts <- all_countries_data[[country_name]]

  names_of_variables <- colnames(this_level_data_ts)
  # print(names_of_variables)
  
  for (j in seq_along(names_of_variables)) {
    # print("antes")
    this_variable <- names_of_variables[j]
    # print(this_variable) 
    this_variable_ts <- this_level_data_ts[ , this_variable]
    # print("despues")
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


chile_rec <- reco_by_country_list[["Chile"]]
chile_data <- all_countries_data[["Chile"]]

table_of_recommendations <- chile_rec
data_tbl_ts <- chile_data
chile_transformed <- follow_rec(data_tbl_ts, table_of_recommendations)


# all_countries_reco <- reduce(sta_reco_list, rbind)
# 
# all_countries_rgdp_stati <- reduce(stationarity_list, rbind) %>% 
#   group_by(country, test) %>% 
#   arrange(country, test, deter_part, alpha)
























# stdata <- tests_of_stationarity_e 
# stdata$country <- "Chile"

# unanim <- stdata %>% 
#   mutate(unanimity = min(recommendation) == max(recommendation),
#          unanimity = ifelse(unanimity, recommendation, NA)) %>% 
#   select(country, unanimity) %>% 
#   unique()
# 
# unanim_deter_level <- stdata %>%
#   filter(deter_part == "level" ) %>% 
#   mutate(unan_level = min(recommendation) == max(recommendation),
#          unan_level = ifelse(unan_level, recommendation, NA)) %>% 
#   select(country, unan_level) %>% 
#   unique()
# 
# unanim_05_deter_level <- stdata %>%
#   filter(deter_part == "level", alpha == 0.05 ) %>% 
#   mutate(unan_05_level = min(recommendation) == max(recommendation),
#          unan_05_level = ifelse(unan_05_level, recommendation, NA)) %>% 
#   select(country, unan_05_level) %>% 
#   unique()
# 
# unanim_kpss <- stdata %>% 
#   filter(test == "kpss") %>% 
#   mutate(unan_kpss = min(recommendation) == max(recommendation),
#          unan_kpss = ifelse(unan_kpss, recommendation, NA)) %>% 
#   select(country, unan_kpss) %>% 
#   unique()
# 
# unanim_kpss_level <- stdata %>% 
#   filter(test == "kpss", deter_part == "level") %>% 
#   mutate(unan_kpss_lev = min(recommendation) == max(recommendation),
#          unan_kpss_lev = ifelse(unan_kpss_lev, recommendation, NA)) %>% 
#   select(country, unan_kpss_lev) %>% 
#   unique()
# 
# kpss_reco <- stdata %>% 
#   filter(test == "kpss", deter_part == "level", alpha == 0.05) %>%
#   select(country, recommendation) %>% 
#   rename(kpss_05_level = recommendation)
# 
# country_recos <- left_join(unanim, unanim_deter_level, by = "country") %>% 
#   left_join(unanim_05_deter_level, by = "country") %>% 
#   left_join(unanim_kpss, by = "country") %>% 
#   left_join(unanim_kpss_level, by = "country") %>% 
#   left_join(kpss_reco, by = "country")
# 
# yoy_reco <- stdata %>% 
#   filter(recommendation == "yoy")
# 
# diff_yoy_reco <- stdata %>% 
#   filter(recommendation == "diff_yoy")
# 
# get_reco(country_name = country_name, variable_name = variable_name,
#          data_transform = data_transform)
# 
# get_reco_from_sta(stdata, "rgdp")
# 
# 
# names(stdata)
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
