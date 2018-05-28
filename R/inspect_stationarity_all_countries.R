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

rgdp_na_index <- is.na(country_transformed_data[ , "rgdp"])
country_transformed_data_balanced <- country_transformed_data[!rgdp_na_index, ]


data_path <- "./data/excel/"
fn_level_data_ts <- get_data(data_path = data_path, country_name = "Chile",
                             data_transform = "level", apply_log = FALSE)




co1 <- sort(colnames(country_transformed_data_balanced))
co2 <- sort(colnames(fn_level_data_ts))

chile_by_other_way <- get_raw_data_ts(country = "Chile")
co3 <- sort(colnames(chile_by_other_way))

country_transformed_data_naom <- na.omit(country_transformed_data)

co3 %in% co1











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
