# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
eco_data <- read.csv("../data_final/eco.csv")

library('ggplot2')
library('forecast')
library('tseries')

# non stationary accept the null hyp
adf.test(eco_data$DXCM.Close, alternative = "stationary")

Acf(eco_data$DXCM.Close, main='') # MA
Pacf(eco_data$DXCM.Close, main='') # AR
a = diff(eco_data$DXCM.Close, 2)
Acf(a)
Pacf(a)
fit <- arima(eco_data$DXCM.Close,order=c(1,1,0))
fit
plot(forecast(fit_auto,h=50))
plot(fit_auto$residuals)
fit_auto <- auto.arima(eco_data$DXCM.Close, trace = TRUE)
fit_auto
plot(fit_auto$residuals)

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# The augmented Dickey-Fuller test on differenced data rejects the null hypotheses of non-stationarity.
# Plotting the differenced series, we see an oscillating pattern around 0 with no visible strong trend.
# This suggests that differencing of order 1 terms is sufficient and should be included in the model.

Acf(count_d1, main='ACF for Differenced Series') #7
Pacf(count_d1, main='PACF for Differenced Series')

fit_auto <- auto.arima(deseasonal_cnt, seasonal=FALSE)
fit_auto
tsdisplay(residuals(fit_auto), lag.max=45, main='(2,1,2) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(2,1,4))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
