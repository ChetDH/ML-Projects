getwd()
setwd("/Users/chethammer/Documents/School/R Studio/Data")

library(readxl)
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
#Read employee data dataset
dat = read.xlsx("/Users/chethammer/Documents/School/R Studio/Data/Employee_Data_Project.xlsx", na.strings = "NA")


#Data Partitioning
set.seed(22)  # for reproducibility
trainrows <- sample(rownames(dat), dim(dat)[1]*0.7) # sample for training data
train_dat <- dat[trainrows, ] # get training data
table(train_dat$Attrition)
prop.table(table(train_dat$Attrition))

# we will set the difference of the training into the validation set i.e. 30%
testrows <- setdiff(rownames(dat), trainrows) 
test_dat <- dat[testrows, ] # get testing data
table(test_dat$Attrition)
prop.table(table(test_dat$Attrition))


unique(dat$StandardHours)

# Data Selection: we don't need employee id for the model and standard hours is always 8
train_dat = train_dat %>%
  select(-EmployeeID, -StandardHours)

test_dat = test_dat %>%
  select(-EmployeeID, -StandardHours)


# Data Cleaning
#TRAINING DATA: Check for Missing values
sum(is.na(train_dat))
# 55, so missing data I need to remove
train_dat = na.omit(train_dat)
table(train_dat$Attrition)
prop.table(table(train_dat$Attrition))

#TEST DATA: Check for missing values
sum(is.na(test_dat))
# 18, so contains missing data as well
test_dat = na.omit(test_dat)
table(test_dat$Attrition)
prop.table(table(test_dat$Attrition))



# convert character columns of interest to factor or numeric
#TRAINING DATA: Alter Attrition first
train_dat$Attrition <- ifelse(train_dat$Attrition=="Yes", 1, 0)


# correlation plot
corr_data = train_dat %>%
  select(Attrition, YearsAtCompany, Age, TotalWorkingYears, DistanceFromHome, NumCompaniesWorked, TrainingTimesLastYear, YearsWithCurrManager)

corr_data.corr = cor(corr_data)

print(corr_data.corr)
corrplot(corr_data.corr, title="Correlation Plot of Numeric Features",mar=c(0,0,1,0))
# the correlation matrix tells us that (yearswithcurrmanager and yearsatcompany) and (totalworkingyears and age) have the strongest positive correlation with attrition



# convert to factor
train_dat$JobSatisfaction <- as.factor(train_dat$JobSatisfaction)
train_dat$Gender <- as.factor(train_dat$Gender)
train_dat$Education <- as.factor(train_dat$Education)
train_dat$Attrition <- as.factor(train_dat$Attrition)
train_dat$JobLevel <- as.factor(train_dat$JobLevel)
train_dat$EnvironmentSatisfaction <- as.factor(train_dat$EnvironmentSatisfaction)
train_dat$BusinessTravel <- as.factor(train_dat$BusinessTravel)
train_dat$MaritalStatus <- as.factor(train_dat$MaritalStatus)



# convert to numeric
train_dat$TotalWorkingYears <- as.numeric(train_dat$TotalWorkingYears)


#TESTING DATA: Alter JobSatisfaction, TotalWorkingYears, and Gender
test_dat$Attrition <- ifelse(test_dat$Attrition=="Yes", 1, 0)

# convert to factor
test_dat$JobSatisfaction <- as.factor(test_dat$JobSatisfaction)
test_dat$Gender <- as.factor(test_dat$Gender)
test_dat$Education <- as.factor(test_dat$Education)
test_dat$Attrition <- as.factor(test_dat$Attrition)
test_dat$JobLevel <- as.factor(test_dat$JobLevel)
test_dat$EnvironmentSatisfaction <- as.factor(test_dat$EnvironmentSatisfaction)
test_dat$BusinessTravel <- as.factor(test_dat$BusinessTravel)
test_dat$MaritalStatus <- as.factor(test_dat$MaritalStatus)


# convert to numeric
test_dat$TotalWorkingYears <- as.numeric(test_dat$TotalWorkingYears)


# Exploratory Data Analysis
# I want to explore some variables of interest in this project (JobSatisfaction, YearsAtCompany, Education, Age, TotalWorkingYears, and Gender) and compare them 
# with the attrition variable to understand which variables may be key predictors. This analysis will be done on the training data

# first explore how many yes and no values for attrition
table(train_dat$Attrition) # 490 yes and 2,542 no

# basic statistics
summary(train_dat)

# plot attrition by JobSatisfaction
ggplot(data=train_dat, aes(JobSatisfaction))+
  geom_bar(aes(fill=Attrition), position="dodge")


train_dat %>%
  select(Attrition, JobSatisfaction) %>%
  group_by(Attrition, JobSatisfaction) %>% 
  summarise(cnt = n()) %>%
  group_by(JobSatisfaction) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  arrange(JobSatisfaction,Attrition)


# plot attrition by Education
ggplot(data=train_dat, aes(Education))+
  geom_bar(aes(fill=Attrition), position="dodge") +
  ggtitle("Histogram of Education by Attrition Value") +
  ylab("Count")


train_dat %>%
  select(Attrition, Education) %>%
  group_by(Attrition, Education) %>% 
  summarise(cnt = n()) %>%
  group_by(Education) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  arrange(Education,Attrition)


# plot attrition by Gender
ggplot(data=train_dat, aes(Gender))+
  geom_bar(aes(fill=Attrition), position="dodge")


train_dat %>%
  select(Attrition, Gender) %>%
  group_by(Attrition, Gender) %>% 
  summarise(cnt = n()) %>%
  group_by(Gender) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  arrange(Gender,Attrition)


# plot attrition by Age
ggplot(train_dat, aes(x=Age, y=Attrition)) +
  geom_jitter()

ggplot(train_dat,aes(x=Age)) +
  geom_histogram(bins=20)+
  facet_grid(~Attrition)


train_dat %>%
  select(Attrition, Age) %>%
  group_by(Attrition) %>% 
  summarise(avg = mean(Age))



# plot attrition by YearsAtCompany
ggplot(train_dat, aes(x=YearsAtCompany, y=Attrition)) +
  geom_jitter()


ggplot(train_dat,aes(x=YearsAtCompany)) +
  geom_histogram(bins=20)+
  facet_grid(~Attrition)


train_dat %>%
  select(Attrition, YearsAtCompany) %>%
  group_by(Attrition) %>% 
  summarise(avg = mean(YearsAtCompany))


# plot attrition by TotalWorkingYears
ggplot(train_dat, aes(x=TotalWorkingYears, y=Attrition)) +
  geom_jitter()


ggplot(train_dat,aes(x=TotalWorkingYears)) +
  geom_histogram(bins=20)+
  facet_grid(~Attrition)



train_dat %>%
  select(Attrition, TotalWorkingYears) %>%
  group_by(Attrition) %>% 
  summarise(avg = median(TotalWorkingYears))


# Downsampling
data_balanced_under <- ovun.sample(Attrition ~ ., data = train_dat, N=1000, method = "under")$data
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
dat_pred <- predict(object = stepmodel3, newdata = test_dat, type = "response")
head(dat_pred)
summary(dat_pred)

#Lets set the threshold to 0.50 for predicting into 1
test_dat$predicted<-ifelse(dat_pred>=0.5, 1, 0)
head(test_dat$predicted)

# Model Performance
#Accuracy
table(test_dat$Attrition, test_dat$predicted)

confusionMatrix(data = as.factor(test_dat$predicted),
                reference =  as.factor(test_dat$Attrition),
                positive = "1")

#As it can be seen , the sensitivity or the power of the model to detect yes cases is above average (67%)
# the specificity or power of model to detect no cases is also above average (69%)

#ROC
roc_pred <- prediction(predictions = dat_pred  , labels = test_dat$Attrition)
roc_perf <- performance(roc_pred , "tpr" , "fpr")
plot(roc_perf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))


roc.curve(test_dat$Attrition, dat_pred)

# AUC 
sprintf("AUC: %s", round(as.numeric(performance(roc_pred, "auc")@y.values),4))

## Area under the curve (AUC): 0.751
