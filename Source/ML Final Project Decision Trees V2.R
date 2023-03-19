library(gdata)
library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(graphics)
library(caret)
library(moments)
library(car)
library(PerformanceAnalytics)
library(openxlsx)
library(corrplot)
library(car)
library(ROCR)
library(ROSE)
#Read in dataset
#setwd("C:/Users/yangs/Documents/Classwork/3 Machine Learning 20/")
setwd("/Users/chethammer/Documents/School/R Studio/Data")
#emps = read.csv("data/Employee_Data_Project.csv", sep = ",", stringsAsFactors = TRUE, na.strings = "NA")
emps = read.xlsx("/Users/chethammer/Documents/School/R Studio/Data/Employee_Data_Project.xlsx", na.strings = "NA")

dim(emps)
head(emps)

table(emps$Attrition)
prop.table(table(emps$Attrition))

summary(emps)

#### 1. Data splitting into training and test (70:30) ####
#Create split, any column is fine
library(caret)
set.seed(123)  # for reproducibility
index <- createDataPartition(emps$Attrition, p = 0.7, 
                             list = FALSE)
train <- emps[index, ]
test  <- emps[-index, ]
table(train$Attrition)
table(test$Attrition)
prop.table(table(train$Attrition))
prop.table(table(train$Attrition))

#### 2. Data categorizing ####
str(emps)

library(dplyr)
data_train <- train %>% 
  #select(c(JobLevel, Education, EnvironmentSatisfaction, JobSatisfaction)) %>% 
  mutate(JobLevel = as.factor(JobLevel),
         Education = as.factor(Education),
         EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction),
         JobSatisfaction = as.factor(JobSatisfaction),
         NumCompaniesWorked = as.numeric(NumCompaniesWorked),
         Gender = as.factor(Gender),
         BusinessTravel = as.factor(BusinessTravel),
         MaritalStatus = as.factor(MaritalStatus),
         Attrition = as.factor(if_else(Attrition == "Yes", 1, 0)))

data_test <- test %>% 
  #select(c(JobLevel, Education, EnvironmentSatisfaction, JobSatisfaction)) %>% 
  mutate(JobLevel = as.factor(JobLevel),
         Education = as.factor(Education),
         EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction),
         JobSatisfaction = as.factor(JobSatisfaction),
         NumCompaniesWorked = as.numeric(NumCompaniesWorked),
         Gender = as.factor(Gender),
         BusinessTravel = as.factor(BusinessTravel),
         MaritalStatus = as.factor(MaritalStatus),
         Attrition = as.factor(if_else(Attrition == "Yes", 1, 0)))

#### 3. Cleaning Data ####
str(data_train)
str(data_test)
sapply(data_train, function(x){sum(is.na(x))})
sapply(data_test, function(x){sum(is.na(x))})

#Remove observations where environment and job satisfaction are null- categorical modes may influence significantly
data_train = data_train %>% filter(is.na(JobSatisfaction)==FALSE, is.na(EnvironmentSatisfaction) == FALSE)
data_test = data_test %>% filter(is.na(JobSatisfaction)==FALSE, is.na(EnvironmentSatisfaction) == FALSE)


#Need to replace values in num companies worked and total working years- replace with medians
train_clean <- data_train %>%
  mutate(NumCompaniesWorked = if_else(is.na(NumCompaniesWorked), 
                                      median(NumCompaniesWorked, na.rm = TRUE), 
                                      NumCompaniesWorked),
         TotalWorkingYears = if_else(is.na(TotalWorkingYears),
                                     median(TotalWorkingYears,na.rm = TRUE), 
                                     TotalWorkingYears))

test_clean <- data_test %>%
  mutate(NumCompaniesWorked = if_else(is.na(NumCompaniesWorked), 
                                      median(NumCompaniesWorked, na.rm = TRUE), 
                                      NumCompaniesWorked),
         TotalWorkingYears = if_else(is.na(TotalWorkingYears),
                                     median(TotalWorkingYears,na.rm = TRUE), 
                                     TotalWorkingYears))

str(train_clean)
str(test_clean)
colSums(is.na(train_clean))
colSums(is.na(test_clean))

# Exploratory Data Analysis
# I want to explore some variables of interest in this project (JobSatisfaction, YearsAtCompany, Education, Age, TotalWorkingYears, and Gender) and compare them 
# with the attrition variable to understand which variables may be key predictors. This analysis will be done on the training data

# first explore how many yes and no values for attrition
table(data_train$Attrition) # 490 yes and 2,542 no

# basic statistics
summary(data_train)

# plot attrition by JobSatisfaction
ggplot(data=data_train, aes(JobSatisfaction))+
  geom_bar(aes(fill=Attrition), position="dodge")


data_train %>%
  select(Attrition, JobSatisfaction) %>%
  group_by(Attrition, JobSatisfaction) %>% 
  summarise(cnt = n()) %>%
  group_by(JobSatisfaction) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  arrange(JobSatisfaction,Attrition)


# plot attrition by Education
ggplot(data=data_train, aes(Education))+
  geom_bar(aes(fill=Attrition), position="dodge") +
  ggtitle("Histogram of Education by Attrition Value") +
  ylab("Count")


data_train %>%
  select(Attrition, Education) %>%
  group_by(Attrition, Education) %>% 
  summarise(cnt = n()) %>%
  group_by(Education) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  arrange(Education,Attrition)


# plot attrition by Gender
ggplot(data=data_train, aes(Gender))+
  geom_bar(aes(fill=Attrition), position="dodge")


data_train %>%
  select(Attrition, Gender) %>%
  group_by(Attrition, Gender) %>% 
  summarise(cnt = n()) %>%
  group_by(Gender) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  arrange(Gender,Attrition)


# plot attrition by Age
ggplot(data_train, aes(x=Age, y=Attrition)) +
  geom_jitter()

ggplot(data_train,aes(x=Age)) +
  geom_histogram(bins=20)+
  facet_grid(~Attrition)


data_train %>%
  select(Attrition, Age) %>%
  group_by(Attrition) %>% 
  summarise(avg = mean(Age))



# plot attrition by YearsAtCompany
ggplot(data_train, aes(x=YearsAtCompany, y=Attrition)) +
  geom_jitter()


ggplot(data_train,aes(x=YearsAtCompany)) +
  geom_histogram(bins=20)+
  facet_grid(~Attrition)


data_train %>%
  select(Attrition, YearsAtCompany) %>%
  group_by(Attrition) %>% 
  summarise(avg = mean(YearsAtCompany))


# plot attrition by TotalWorkingYears
ggplot(data_train, aes(x=TotalWorkingYears, y=Attrition)) +
  geom_jitter()


ggplot(data_train,aes(x=TotalWorkingYears)) +
  geom_histogram(bins=20)+
  facet_grid(~Attrition)



data_train %>%
  select(Attrition, TotalWorkingYears) %>%
  group_by(Attrition) %>% 
  summarise(avg = median(TotalWorkingYears))


# Downsampling
data_balanced_under <- ovun.sample(Attrition ~ ., data = data_train, N=1000, method = "under")$data
table(data_balanced_under$Attrition)



# Logistic Regression Modeling
model1 <- glm(Attrition ~ ., family = "binomial", data = data_balanced_under)  
summary(model1)


#Check multicollinearity
vif(model1)
# All VIF are less than 5 so we need not reject any variable



# Which of the above variables significantly impact employee attrition?
# Using a 95% significance level, the top 3 variables which significantly impact employee attrition at the company are: MaritalStatus (MaritalStatusSingle)
#, YearsWithCurrManager, and BusinessTravel (BusinessTravelTravel_Frequently )


# Perform model selection
stepmodel = step(model1, direction="both")
stepmodel2 = step(model1, direction = "backward")
stepmodel3 = step(model1, direction = "forward")

formula(stepmodel)
formula(stepmodel2)
formula(stepmodel3)

summary(stepmodel)
summary(stepmodel2)
summary(stepmodel3)
#Using stepwise selection, the model using forward selection as it has practically the same AIC with a lower model deviance.

formula(stepmodel3)
summary(stepmodel3)

# Which of the above variables significantly impact employee attrition?
# Using a 95% significance level, the top 3 variables which significantly impact employee attrition at the company are: MaritalStatus (MaritalStatusSingle)
#, YearsWithCurrManager, and BusinessTravel (BusinessTravelTravel_Frequently)


##Next we look at Odds ratio for the different variables
#Odds ratio is the exponential of the coefficients (which are the log of odds, what is the change in odds per unit change of x)
#Outcome ratio < 1: Denominator Group is more likely to have this outcome
#Odds Ratio > 1: Numerator Group is more likely to have the outcome
#ratio of the odds after a unit change in the predictor to the original odds
#What is impact of x on odds of y?

exp(coef(stepmodel3))
#Odds Ratio as Percentage
((exp(coef(stepmodel3)[-1]))-1)*100

#odds of y decreases by exponential of coefficient to 1 for each unit increase in X
#Education2, JobLevel2, MaritalStatusSingle, YearsAtCompany, BusinessTravelTravel_Frequently, JobLevel4, NumCompaniesWorked, BusinessTravelTravel_Rarely, DistanceFromHome,
# GenderMale, and MaritalStatusMarried all have odds ratios greater than 1, meaning that the odds of attrition are more likely for these variables for each unit increase 
# in the variable.
#Those who work at more companies may be more likely to leave as they have proven to be less committed
#Those who have worked more years at the company, may feel the need to take on another role/be challenged another way
# Those who travel frequently may be more burnt out
# Those who are single may leave as they have less responsibilities tying them to one job


# interpreting the coefficients by transforming them back to probabilities
summary(stepmodel3)
exp(coef(stepmodel3))
#Odds Ratio as Percentage
((exp(coef(stepmodel3)[-1]))-1)*100
# to help us better understand the regression results it is important to interpret the coefficients. for example, we can see that the estimate for NumCompaniesWorked is 0.1382.
# if we convert this to the odds ratio, we can learn that as a person's number of companies worked for increases by 1, their odds of attrition is multiplied by 1.15. Or, an increase
# in one company worked for is associated with an increase of 15% in the odds of attrition.


# The predictor YearsWithCurrManager has a coefficient of -0.184. If we convert this to the odds ratio, we can learn that as the years with current manager increases, the odds
# of attrition decreases by 17%.

# The predictor MaritalStatusSingle has a coefficient of 1.236. If we convert this to the odds ratio, we can learn that the odds of attrition is multiplied by 3.44 for single workers.

# the predictor BusinessTravelTravel_Frequently has a coefficient of 1.319. If we convert this to the odds ratio, we can learn that the odds of attrition is multiplied by 3.74 for 
# frequently traveling workers.


# Provide my insights using model accuracy measures, a ROC curve, and other measures using a probability cut off of 50%.
# First do predictions on the test set
dat_pred <- predict(object = stepmodel3, newdata = data_test, type = "response")
head(dat_pred)
summary(dat_pred)

#Lets set the threshold to 0.50 for predicting into 1
data_test$predicted<-ifelse(dat_pred>=0.5, 1, 0)
head(data_test$predicted)

# Model Performance
#Accuracy
table(data_test$Attrition, data_test$predicted)

confusionMatrix(data = as.factor(data_test$predicted),
                reference =  as.factor(data_test$Attrition),
                positive = "1")

#As it can be seen , the sensitivity or the power of the model to detect yes cases is above average (67%)
# the specificity or power of model to detect no cases is also above average (69%)

#ROC
roc_pred <- prediction(predictions = dat_pred  , labels = data_test$Attrition)
roc_perf <- performance(roc_pred , "tpr" , "fpr")
plot(roc_perf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))


roc.curve(data_test$Attrition, dat_pred)

# AUC 
sprintf("AUC: %s", round(as.numeric(performance(roc_pred, "auc")@y.values),4))

## Area under the curve (AUC): 0.751

#### 4. PERFORM DATA EXPLORATION ####


####5. DECISION TREES#####
library(rpart)       #Recursive partitioning for classification
library(rattle)      #A free graphical interface for ML 
library(rpart.plot)  #Tree Plots

tree.survival = rpart(Attrition~., data=train_clean)
printcp(tree.survival)

plotcp(tree.survival)

#Plot the tree
par(mfrow=c(1,2)) # print 2 charts horizontally
plot(tree.survival)
text(tree.survival)

fancyRpartPlot(tree.survival)

#Prune the tree to prevent overfitting
cp <- min(tree.survival$cptable[,1])
pruned.tree.survival <- prune(tree.survival, cp=cp)

#Now we can use our pruned decision tree to make a prediction on the test data set
tree.survival.predict <- predict(pruned.tree.survival, test_clean, type="class")

#Create the confusion matrix to define the reliability of our model
#The confusion Matrix enables to analyze the the success rate per prediction ( survival or non-survival ). 
#On the horizontal axis you have the actual classes and on the vertical axis you have the predicted class.
library(caret)
confusionMatrix(tree.survival.predict, test_clean$Attrition, positive="1")

#ROC Curve
library(ROCR)

predict_rpart <- predict(pruned.tree.survival, test_clean, type="prob")[,2]

roc_rpart <- prediction(predict_rpart, test_clean$Attrition)
rpart_perform <- performance(roc_rpart,measure = "tpr",
                             x.measure = "fpr")
par(mfrow=c(1,1))
plot(rpart_perform)

#AUC
auc_rpart <- performance(roc_rpart,measure="auc")
auc_rpart <- auc_rpart@y.values[[1]]
auc_rpart

#### 6. BAGGING ####

library(caret)       # bagging
library(lattice)     # sub-library for caret
library(ggplot2)     # plotting
# train bagged model
# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(
  Attrition ~ .,
  data = train_clean,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv

# plot most important variables
plot(varImp(bagged_cv), 10) 

#Confusion Matrix
confusionMatrix(bagged_cv)

#Performance on the Test Data

pred_bag <- predict(bagged_cv, test_clean)
confusionMatrix(pred_bag, test_clean$Attrition, positive = "1")

#ROC Curve
library(ROCR)
library(caret)

#convert them to numeric from factor
pred_bag=as.numeric(pred_bag)
test_clean$Attrition = as.numeric(test_clean$Attrition)

roc_rpart <- prediction(pred_bag, test_clean$Attrition)
rpart_perform <- performance(roc_rpart,measure = "tpr",
                             x.measure = "fpr")

plot(rpart_perform)

#AUC
auc_rpart <- performance(roc_rpart,measure="auc")
auc_rpart <- auc_rpart@y.values[[1]]
auc_rpart

#### 7. RANDOM FOREST#####

library(randomForest)

rf.tree.survival <- randomForest(Attrition~., data=train_clean, ntree=500)
rf.tree.survival

#Plotting the model will illustrate the error rate as we average across more trees
plot(rf.tree.survival)

#Model tuning
#We will build a model with the number of trees that give us the minimum OOB error 
ntrees <- which.min(rf.tree.survival$err.rate[,1])

rf.tree.survival <- randomForest(Attrition~., data=train_clean, ntree=ntrees)
rf.tree.survival

#Prediction and confusion matrix
str(train_clean)
str(test_clean)

test_clean = test_clean %>% mutate(Attrition = if_else(Attrition == 2, 1, 0))
test_clean$Attrition = as.factor(test_clean$Attrition)
levels(test_clean) = levels(train_clean)

rf.trees.predict <- predict(rf.tree.survival, newdata=test_clean, type="class")
str(rf.trees.predict)
confusionMatrix(rf.trees.predict, test_clean$Attrition, positive="1")

#MODEL PERFORMANCE
require(ROCR)
tree.survival.predict2 <- predict(rf.tree.survival, test_clean, type="prob")
predROC <- prediction(tree.survival.predict2[,2], test_clean$Attrition)
perfROC <- performance(predROC, "tpr", "fpr")
plot(perfROC)
abline(a=0, b=1)

#Calculate the area under the curve
perfROC <- performance(predROC, "auc")
perfROC@y.values[[1]]

#Variable Importance
varImp(rf.tree.survival)
varImpPlot(rf.tree.survival)
