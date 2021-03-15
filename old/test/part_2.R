# LASSO - Part 2
# data house houseValue

#homeValue: median price of single-family house in the neighborhood (measured in dollar)

#crimeRate_pc: crime rate per capital, measured by number of crimes per 1000 residents in neighborhood
#nonRetailBusiness: the proportion of non-retail business acres per neighborhood
#withWater: the neighborhood within 5 miles of a water body (lake, river, etc); 1 if true and 0 otherwise 
#ageHouse: proportion of house built before 1950
#distanceToCity: distances to the nearest city (measured in miles)
#pupilTeacherRatio: average pupil-teacher ratio in all the schools in the neighborhood 
#pctLowIncome: percentage of low income household in the neighborhood
#pollutionIndex: pollution index, scaled between 0 and 100, with 0 being the best and 100 being the worst (i.e. uninhabitable) 
#nBedRooms: average number of bed rooms in the single family houses in the neighborhood

house_value_data <- read.csv("../data_final/houseValueData.csv")
library("glmnet")
library("caret")

x = as.matrix(house_value_data[-c(9)]) 
y = house_value_data$homeValue

set.seed(1)
cv.lasso <- cv.glmnet(x, y, alpha = 1)
plot(cv.lasso)
cv.lasso$lambda.min

cv.lasso$lambda.1se

coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)

set.seed(1)
lasso.model <- glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.min)
lasso.model
summary(lasso.model)
coef(lasso.model)
#poisson
set.seed(1)
cv_p.lasso <- cv.glmnet(x, y, alpha = 1,family="poisson")
plot(cv_p.lasso)
fit_poisson = glmnet(x,y,family="poisson", alpha = 1, lambda = cv_p.lasso$lambda.min)
fit_poisson
summary(fit_poisson)
coef(fit_poisson, fit_poisson$lambda.min)

# Perform 10-fold cross-validation to select lambda
lambdas_to_try <- 10^seq(3, 10, length.out = 100)
# Setting alpha = 1 implements lasso regression
set.seed(1)
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, nfolds = 10)
# Plot cross-validation results
plot(lasso_cv)
# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min
lambda_cv

set.seed(1)
res <- glmnet(x, y, alpha = 1, lambda = lambdas_to_try)
res
coef(lasso_cv, lambda_cv)
coef(res)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .5)

house_value_data_raw <- read.csv("../data_final/houseValueData.csv")
set.seed(1)
training.samples <- createDataPartition(house_value_data_raw$homeValue, p = 0.8, list = FALSE)
train.data  <- house_value_data_raw[training.samples, ]
test.data <- house_value_data_raw[-training.samples, ]

x_t <- model.matrix(homeValue~., train.data)[,-1]
y_t <- train.data$homeValue

set.seed(1)
cv.lasso <- cv.glmnet(x_t, y_t, alpha = 1)
model_n <- glmnet(x_t, y_t, alpha = 1, lambda = cv.lasso$lambda.min)
x.test <- model.matrix(homeValue ~., test.data)[,-1]
probabilities <- predict(model_n, newx = x.test)
mean(probabilities)
mean(test.data$homeValue)

cv.lasso <- cv.glmnet(x_t, y_t, alpha = 1)
model_n <- glmnet(x_t, y_t, alpha = 1, lambda = cv.lasso$lambda.1se)
x.test <- model.matrix(homeValue ~., test.data)[,-1]
probabilities <- predict(model_n, newx = x.test)
mean(probabilities)
mean(test.data$homeValue)

cv.lasso <- cv.glmnet(x_t, y_t, alpha = 1, family="poisson")
model_n <- glmnet(x_t, y_t, alpha = 1, lambda = cv.lasso$lambda.min, family="poisson")
x.test <- model.matrix(homeValue ~., test.data)[,-1]
probabilities <- predict(model_n, newx = x.test)
mean(probabilities)
mean(test.data$homeValue)

cv.lasso <- cv.glmnet(x_t, y_t, alpha = 1, lambda = lambdas_to_try, nfolds = 10)
model_n <- glmnet(x_t, y_t, alpha = 1, lambda = lambdas_to_try)
# Make predictions on the test data
x.test <- model.matrix(homeValue ~., test.data)[,-1]
probabilities <- predict(model_n, newx = x.test)
mean(probabilities)
mean(test.data$homeValue)
