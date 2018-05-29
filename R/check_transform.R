library(xts)
library(lubridate)
library(timetk)
library(readxl)
library(tidyverse)

source("./R/utils_av.R")


chl <- read_excel("data/excel/Chile.xlsx",
sheet = "rgdp", col_types = c("date",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric"))

startdate <- min(chl$date)

chl_xts <- tk_xts(chl, silent = TRUE)
chl_ts <- tk_ts(chl, silent = TRUE, frequency = 4, 
                start = c(year(startdate), quarter(startdate)))

rgdp_ts <- chl_ts[ , "rgdp"]
yoy_ts <-  chl_ts[ , "rgdp_yoy"]
diff_yoy_ts <-  chl_ts[ , "rgdp_diff_yoy"]
diff_diff_yoy_ts <-  chl_ts[ , "rgdp_diff_diff_yoy"]
diff_ts <-  chl_ts[ , "rgdp_diff"]
diff_diff_ts <-  chl_ts[ , "rgdp_diff_diff"]


my_yoy <- make_yoy_ts(rgdp_ts)
my_diff_yoy <- base::diff(make_yoy_ts(rgdp_ts))
my_diff_diff_yoy <- base::diff(make_yoy_ts(rgdp_ts), differences = 2)
my_diff1 <- base::diff(rgdp_ts)
my_diff_diff_yoy <- base::diff(make_yoy_ts(rgdp_ts), differences = 2)
my_diff_diff <- base::diff(rgdp_ts, differences = 2)
