source('./R/itall.R')
library(forecast)


imacec <- readRDS(file = "./data/imacec_monthly.rds")

fit <- sarima(data = imacec, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

fc <- sarima.for(xdata = imacec, nahead = 23, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
fc_mean <- fc[["pred"]]

imacec_and_fc <- ts(data = c(imacec, fc_mean), frequency = 12, start = start(imacec))
imacec_and_fc
difflog_ima <- diff(imacec_and_fc, lag = 12)
difflog_ima

fit_fp <- Arima(y = imacec, order = c(0, 1, 1), seasonal = c(0, 1, 1))
fc_fp <- forecast(fit_fp, h = 23)
fc_mean_fp <- fc_fp$mean
imacec_and_fc_fp <- ts(data = c(imacec, fc_mean_fp), frequency = 12, start = start(imacec))
difflog_ima_fp <- diff(imacec_and_fc_fp, lag = 12)
difflog_ima_fp

both <- ts.union(difflog_ima, difflog_ima_fp, difflog_ima - difflog_ima_fp)
both

# > x_for_test_uncond_fit
# Series: x_for_test_monthly_ts 
# ARIMA(0,1,1)(0,1,1)[12] 
# 
# Coefficients:
#   ma1     sma1
# -0.4297  -0.6909
# s.e.   0.0606   0.0742
# 
# sigma^2 estimated as 0.0001581:  log likelihood=494.51
# AIC=-983.02   AICc=-982.87   BIC=-973.65


# > fit
# $`fit`
# 
# Call:
#   arima(x = data, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S), 
#         optim.control = list(trace = 1, REPORT = 1, reltol = tol))
# 
# Coefficients:
#   ma1     sma1
# -0.4288  -0.6914
# s.e.   0.0607   0.0741
# 
# sigma^2 estimated as 0.0001549:  log likelihood = 494.51,  aic = -983.02
# 
# $AIC
# [1] -7.750424
# 
# $AICc
# [1] -7.738625
# 
# $BIC
# [1] -8.715082