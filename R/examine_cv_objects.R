source('./R/utils_av.R')
country_name <- "Uruguay"
path_models_and_accu <- paste("./data/", country_name, "_by_step_12345.rds", sep = "")
path_cv_objects <- paste("./data/", country_name, "_by_step_12345_cv_objects.rds", sep = "")
path_VAR_data <- paste("./data/VAR_data_", country_name, ".rds", sep = "")

models_and_accu <- readRDS(path_models_and_accu)
cv_objects <- readRDS(path_cv_objects)
VAR_data <- readRDS(path_VAR_data)

names(cv_objects)


cvtestdata <- cv_objects[["cv_test_data"]]
cvfcs <- cv_objects[["cv_fcs"]]

cvtestdata_model1 <- cv_objects[["cv_test_data"]][[1]]
cvfcs_model1 <- cv_objects[["cv_fcs"]][[1]]

length(cvtestdata)



path_models_and_accu6 <- paste("./data/", country_name, "_by_step_h6_12345.rds", sep = "")
path_cv_objects6 <- paste("./data/", country_name, "_by_step_12345_h6_cv_objects.rds", sep = "")

models_and_accu6 <- readRDS(path_models_and_accu6)
cv_objects6 <- readRDS(path_cv_objects6)




cvtestdata6 <- cv_objects6[["cv_test_data"]]
cvfcs6 <- cv_objects6[["cv_fcs"]]

cvtestdata6_model1 <- cv_objects6[["cv_test_data"]][[1]]
cvfcs6_model1 <- cv_objects6[["cv_fcs"]][[1]]




