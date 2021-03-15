int_rate_data <- read.csv("../data_final/UKinterestrates.dat", header = FALSE)
library(forecast)
library(tseries)
int_rate_data.clean_cnt = tsclean(int_rate_data[,1])
plot.ts(int_rate_data.clean_cnt)

plot.ts(int_rate_data[,1])

tsData = ts(int_rate_data[,1], frequency = 30)
plot(tsData)
int_rate_data.ts = decompose(tsData)
plot(int_rate_data.ts)
adf.test(int_rate_data[,1], alternative = "stationary")
# p-value is 0.07 so we reject the null hypotesis, the data is stationary

mean(int_rate_data[,1])
acf(int_rate_data[,1], main='ACF')
pacf(int_rate_data[,1], main='PACF') # 2

# AR order, the degree of differencing, and the MA order.
int_rate_data.arima = arima(int_rate_data[,1],order=c(2,0,0))
int_rate_data.arima
tsdiag(int_rate_data.arima)
tsdisplay(int_rate_data.arima$residuals, main='(2,0,0) Model Residuals')
acf(int_rate_data.arima$residuals)

Box.test(int_rate_data.arima$residuals)
qqnorm(int_rate_data.arima$residuals)
qqline(int_rate_data.arima$residuals)

Box.test(int_rate_data.arima$residuals)
plot(int_rate_data.arima$residuals)
predict(int_rate_data.arima,n.ahead = 5)
plot(forecast(int_rate_data.arima, h=20))

fit_auto<- auto.arima(int_rate_data, trace=TRUE)
fit_auto
plot(fit_auto$residuals)
Box.test(fit_auto$residuals)
qqnorm(fit_auto$residuals)
qqline(fit_auto$residuals)
plot(forecast(fit_auto, h=20))

fit3 <- arima(int_rate_data,order=c(2,0,1))
fit3
tsdiag(fit3)
Box.test(fit3$residuals)
plot(fit3$residuals)
plot(forecast(fit3, h=20))

fit <- arima(int_rate_data,order=c(7,0,0))
fit
plot(fit$residuals)

fit1 <- arima(int_rate_data,order=c(8,1,1))
fit1
plot(fit1$residuals)

plot.ts(int_rate_data)
lines(fitted(int_rate_data.arima),col="green")
lines(fitted(fit3),col="red")
lines(fitted(fit1),col="blue")

qqnorm(fit3$residuals)
qqline(fit3$residuals)


ndiffs(int_rate_data[,1])
int_rate_data_diff = diff(int_rate_data[,1])
plot.ts(int_rate_data_diff)
acf(int_rate_data_diff)
pacf(int_rate_data_diff)
int_rate_data_diff.arima = arima(int_rate_data_diff,order=c(1,0,1))
int_rate_data_diff.arima
auto_fit = auto.arima(int_rate_data_diff, trace = TRUE)

