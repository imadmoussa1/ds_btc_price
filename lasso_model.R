library(caret)
library(glmnet)
coin_data <- read.csv("imp_coin.csv")[-c(1,2,4,5,6,8)]
coin_data_test <-read.csv("imp_coin_test.csv")[-c(1,2,4,5,6,8)]
# Split the data into training and test set

lasso_model <- function(x, y) {
  lambdas_to_try <- 10^seq(-3, 10, length.out = 100)
  set.seed(1) 
  # cv.lasso <- cv.glmnet(x, y, alpha = 1, family="gaussian")
  cv.lasso <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, nfolds = 10)
  # plot(cv.lasso)
  cv.lasso$lambda.min
  cv.lasso$lambda.1se
  coef(cv.lasso, cv.lasso$lambda.min)
  coef(cv.lasso, cv.lasso$lambda.1se)
  # Fit the final model on the training data
  set.seed(1)
  model <- glmnet(x, y, alpha = 1, family = "gaussian", lambda = cv.lasso$lambda.min)
  
  print(coef(model))
  return(model)
}

predict_model <- function(model, data_to_predict, real_data) {
  a  = predict(model, newx = data_to_predict)
  plot.ts(a)
  lines(real_data$btc_price,col="green")
  print(mean(a), mean(real_data$btc_price))
}

training.samples <- createDataPartition(coin_data$btc_price, p = 0.7, list = FALSE)
train.data  <- coin_data[training.samples, ]
test.data <- coin_data[-training.samples, ]
# day by day model on 70% data trained
x <- model.matrix(btc_price~., train.data)[,-1]
y <- as.double(train.data$btc_price)
x.test <- model.matrix(btc_price ~., test.data)[,-1]

xa <- model.matrix(btc_price~., coin_data)[,-1]
ya <- as.double(coin_data$btc_price)
test_last_20 <- model.matrix(btc_price ~., coin_data_test)[,-1]

# predict on the 30% of the data
predict_model(lasso_model(x, y) ,x.test, test.data)
# predict on the last 20 day of the data day by day
predict_model(lasso_model(x, y), test_last_20, coin_data_test[1])

# predict on the 30% of the data delay one day
predict_model(lasso_model(head(x,-1), tail(y,-1)),head(x.test, -1), tail(test.data, -1))
# predict on the last 20 day of the data delay one day
predict_model(lasso_model(head(x,-1), tail(y,-1)),head(test_last_20, -1), tail(coin_data_test[1], -1))

# day by day model on all data trained test on the last 20 day
predict_model(lasso_model(xa, ya), test_last_20, coin_data_test[1])

# delay one day model on all of data trained test on the last 20 day
xa <- head(model.matrix(btc_price~., coin_data)[,-1], -1)
ya <- tail(as.double(coin_data$btc_price), -1)
predict_model(lasso_model(xa, ya),head(test_last_20,-1), tail(coin_data_test[1],-1))

