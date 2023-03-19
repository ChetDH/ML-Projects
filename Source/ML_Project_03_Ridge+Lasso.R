
library(tidyverse)
library(dplyr)

getwd()
setwd("C:/Users/yangs/Documents/Classwork/3 Machine Learning 20/data")

#Read bike share dataset
bikes <-read.csv("Capital Bike Sharing data by hour.csv", 
                 stringsAsFactors =TRUE)

#Remove the variables that we won't use in our model
bikes <- bikes %>%
  select(-dteday, -instant, -casual, -registered, -yr)


bikes = bikes[ , -which(names(bikes) %in% c("dteday","instant","casual","registered","yr"))]
colnames(bikes)

#Recreate the dataset we used from our linear regression model

###############################################################

#### 1. Data partition ####
trainrows <- sample(rownames(bikes), dim(bikes)[1]*0.7)
train <- bikes[trainrows, ]

# we will set the difference of the training into the validation set i.e. 30%
testrows <- setdiff(rownames(bikes), trainrows)
test <- bikes[testrows, ]

colnames(train)

#### 2. Missing values ####
#2a. If you have less than 20% missing (replace values with mean)
sapply(train,function(x) sum(is.na(x)))

#No missing values 


#2b. TEST DATA: MISSING VALUES
#No missing values in test or training data

#Placeholder for linear regression model from project 1

###############################################################
install.packages("glmnet")
library(glmnet)   # implementing regularized regression approaches
library(ggplot2)  # plotting
library(dplyr)    # basic data manipulation procedures

#### 3. RIDGE REGRESSION ####

#To implement Ridge regression we will focus on the glmnet package
#glmnet does not use the formula method (y ~ x) so prior to modeling we need to create our feature and target set. 
#Furthermore, we use the model.matrix function on our feature set, which will automatically dummy encode qualitative variables

# transform into log variables datasets
colnames(bikes)
bikes_train_x <- model.matrix(cnt ~ ., train)[, -1]
bikes_train_y <- log(train$cnt)

bikes_test_x <- model.matrix(cnt ~ ., test)[, -1]
bikes_test_y <- log(test$cnt)

# What is the dimension of of your feature matrix?
dim(bikes_train_x)
## [1] 12165 12
## We have 12,165 observations and 12 predictors

# Apply Ridge regression to bikes train data
bikes_ridge <- glmnet(
  x = bikes_train_x,
  y = bikes_train_y,
  alpha = 0
)

plot(bikes_ridge, xvar = "lambda")
#As log of lambda approarches inifity our coefficients decrease


#You can see the exact lambda values applied
# lambdas applied to penalty parameter
bikes_ridge$lambda %>% head()

#all 100 lambda values 
bikes_ridge$lambda

# coefficients for variables of interest
coef(bikes_ridge)[c("season", "mnth", "hr", "holiday", "weekday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"), 100]
coef(bikes_ridge)[c("season", "mnth", "hr", "holiday", "weekday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"), 1]

#at this point, we do not understand how much improvement we are experiencing in our model.
#to identify the optimal ?? value we need to perform cross-validation (CV) in order to find out at what lambda value we can get the minimum means squared error (MSE)

# Apply CV Ridge regression to bikes data; this is a 10-fold cross validation
bikes_ridge <- cv.glmnet(
  x = bikes_train_x,
  y = bikes_train_y,
  alpha = 0
)

# plot results
plot(bikes_ridge)

min(bikes_ridge$cvm)       # minimum MSE

bikes_ridge$lambda.min     # lambda for this min MSE

bikes_ridge$cvm[bikes_ridge$lambda == bikes_ridge$lambda.1se]  # 1 st.error of min MSE

bikes_ridge$lambda.1se  # lambda for this MSE
#The advantage of identifying the ?? with an MSE within one standard error becomes more obvious 
#with the lasso and elastic net models. However, for now we can assess this visually.

#Here we plot the coefficients across the ?? values and 
#the dashed red line represents the largest ?? that falls within one standard error of the minimum MSE
bikes_ridge_min <- glmnet(
  x = bikes_train_x,
  y = bikes_train_y,
  alpha = 0
)

plot(bikes_ridge_min, xvar = "lambda")
abline(v = log(bikes_ridge$lambda.1se), col = "red", lty = "dashed")

#Variable importance
#s is the value of the penalty parameter lambda at which predictions are made
library(broom)
coef(bikes_ridge, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

#The results of our ridge regression model indicate that feels like temperature (atemp), actual temperature (temp), and windspeed are our most influential variables. 

# predict ridge
pred <- predict(bikes_ridge, s = bikes_ridge$lambda.min, bikes_test_x)
mean((bikes_test_y - pred)^2)

#### 4. LASSO REGRESSION ####
## Apply lasso regression to bike data
bike_lasso <- glmnet(
  x = bikes_train_x,
  y = bikes_train_y,
  alpha = 1
)

plot(bike_lasso, xvar = "lambda")

#Similar to the Ridge regression, we need to perform CV to determine what the right value is for ??
# Apply CV Ridge regression to bikes data (specify alpha=1)
bikes_lasso <- cv.glmnet(
  x = bikes_train_x,
  y = bikes_train_y,
  alpha = 1
)
# plot results
plot(bikes_lasso)

#As before, we can extract our minimum and one standard error MSE and ?? values
min(bikes_lasso$cvm)       # minimum MSE

bikes_lasso$lambda.min     # lambda for this min MSE

bikes_lasso$cvm[bikes_lasso$lambda == bikes_lasso$lambda.1se]  # 1 st.error of min MSE

bikes_lasso$lambda.1se  # lambda for this MSE


bikes_lasso_min <- glmnet(
  x = bikes_train_x,
  y = bikes_train_y,
  alpha = 1
)

plot(bikes_lasso_min, xvar = "lambda")
abline(v = log(bikes_lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(bikes_lasso$lambda.1se), col = "red", lty = "dashed")

#Variable importance
#s is the value of the penalty parameter lambda at which predictions are required
coef(bikes_lasso, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

#often when we remove features we sacrifice accuracy. 
#Consequently, to gain the refined clarity and simplicity that lasso provides, we sometimes reduce the level of accuracy. 
#Typically we do not see large differences in the minimum errors between the two. 

# minimum Ridge MSE
min(bikes_ridge$cvm)

# minimum Lasso MSE
min(bikes_lasso$cvm)



# predict lasso
pred <- predict(bike_lasso, s = bike_lasso$lambda.min, bikes_test_x)
mean((bikes_test_y - pred)^2)



