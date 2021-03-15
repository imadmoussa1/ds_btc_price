blockchain_data <- read.csv("blockchain_data.csv")[-c(1, 2, 11)]
library(caret)
library(glmnet)

# Split the data into training and test set
set.seed(1)
training.samples <- createDataPartition(blochain_data$market_price, p = 0.7, list = FALSE)
train.data  <- blochain_data[training.samples, ]
test.data <- blochain_data[-training.samples, ]

# Dumy code categorical predictor variables
x <- head(model.matrix(market_price~., train.data)[,-1],-1)
# Convert the outcome (class) to a numerical variable
y <- tail(train.data$market_price,-1)

lambdas_to_try <- 10^seq(-5, 5, length.out = 100)
set.seed(1)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family="gaussian", lambda = lambdas_to_try, nfolds = 10)
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)
# Fit the final model on the training data
plot( glmnet(x, y, alpha = 1) ,xvar="lambda",label=TRUE)
plot( glmnet(x, y, alpha = 1),xvar="dev",label=TRUE)
model <- glmnet(x, y, alpha = 1, family = "gaussian", lambda = cv.lasso$lambda.min)

# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(market_price ~., test.data)[,-1]
probabilities <- predict(model, newx = head(x.test,-1))
predicted.classes <- probabilities
# Model accuracy
observed.classes <- test.data$market_price
plot.ts(predicted.classes)
lines(observed.classes,col="green")
mean(predicted.classes)
mean(observed.classes)


blockchain_data_test <- read.csv("blockchain_data_test.csv")[-c(1, 2, 11)]

# xa <- head(model.matrix(market_price~., blockchain_data)[,-1], -1)
# ya <- tail(as.double(blockchain_data$market_price), -1)
# set.seed(1) 
# cva.lasso <- cv.glmnet(xa, ya, alpha = 1, family="gaussian", lambda = lambdas_to_try, nfolds = 10)
# plot(cva.lasso)
# cva.lasso$lambda.min
# cva.lasso$lambda.1se
# coef(cva.lasso, cva.lasso$lambda.min)
# coef(cva.lasso, cva.lasso$lambda.1se)
# 
# modela <- glmnet(xa, ya, alpha = 1, family = "gaussian", lambda = cva.lasso$lambda.min)
# coef(modela)

x.testa <- model.matrix(market_price ~., blockchain_data_test)[,-1]
a  = predict(model, newx = head(x.testa,-1))
plot.ts(a, ylim=c(3000, 5000))
lines(tail(blockchain_data_test$market_price,-1), col="green")
