coin_data <- read.csv("imp_coin.csv")[-c(1,4,5,6)]

library(forecast)
library(tseries)

error_plot <- function(model) {
  tsdiag(model)
  tsdisplay(model$residuals)
  Box.test(model$residuals)
  qqnorm(model$residuals)
  qqline(model$residuals)
}

stationary_test_with_diff <- function(data_set) {
  # p-value is 0.07 so we reject the null hypotesis, the data is stationary
  test_1 = adf.test(data_set, alternative = "stationary")
  if (test_1$p.value < 0.05) {
    return(data_set)
  } else {
    diff_value = ndiffs(data_set)
    print(diff_value)
    diff_data = diff(data_set,diff_value)
    test_2 = adf.test(diff_data, alternative = "stationary")
    if (test_2$p.value < 0.05) {
      return(diff_data)
    }
  }
}

plot.ts(coin_data[2])
# 2017-01-01 to 2019-01-01
data_2017_2019 = tail(coin_data[2], 730)
plot.ts(data_2017_2019)
tsData = ts(data_2017_2019,frequency = 30)
int_rate_data.ts = decompose(tsData)
plot(int_rate_data.ts)

diff_data = stationary_test_with_diff(data_2017_2019[,1])
mean(diff_data)
acf(diff_data, main='ACF')
pacf(diff_data, main='PACF')

fit_auto<- auto.arima(diff_data, trace=TRUE)
fit_auto
error_plot(fit_auto)
# AR order, the degree of differencing, and the MA order.


predict(int_rate_data.arima,n.ahead = 5)
plot(forecast(int_rate_data.arima, h=20))

# log data
log_data_2010_2019 = log(coin_data[2])
plot.ts(log_data_2010_2019)
arima(log_data_2010_2019[,1],order=c(4,1,2))

diff_data = stationary_test_with_diff(log_data_2010_2019[,1])
mean(diff_data)
acf(diff_data, main='ACF')
pacf(diff_data, main='PACF') # 2

fit_auto<- auto.arima(diff_data, trace=TRUE)
error_plot(fit_auto)
error_plot(arima(log_data_2010_2019[,1],order=c(4,1,2)))
