# Lab 4
# Author: Himanshu Nirmal
# Part 1

library(tidyverse)
library(data.table)
library(dplyr)
library(MASS)
library(leaps)
library(glmnet)
library(caTools)


# 1. Split the data set into a training set and a test set.

College=read.csv("College.csv",header=T,na.strings = "?")
summary(College)
str(College)

College=College[,-1] # removing the first variable as it is just college names & wouldn't be useful in our analysis
str(College) # checking the data frame now after removal

train = sample(777,500)

set.seed(123)
sample=sample.split(College,SplitRatio = 0.70)
train1=subset(College,sample ==TRUE)
test1=subset(College,sample ==FALSE)

traindata1 = College[train,]
testdata1 = College[-train,]
response=College[,2]
trainoutp1 = response[train]
testoutp1 = response[-train]

# 2. Fit a linear model using least squares on the training set, and report the test error obtained.

lm1 = lm(Apps~.,data = College,subset = train)
summary(lm1)

#length(trainoutp1)
#length(testoutp1)

#caculating the test error
MSE = mean((testoutp1-predict(lm1,testdata1))^2)
lm1error = sqrt(MSE)
lm1error

# not a very good model as the error rate is (+ - 903) which is not accurate

#3. Fit a ridge regression model on the training set, with ?? chosen by cross-validation. Report the test error obtained.

# Rather than accepting a formula and data frame, ridge regression requires a vector input and matrix of predictors.
# with alpha = 0

train2=model.matrix(Apps~.,traindata1)
test2=model.matrix(Apps~.,testdata1)

set.seed (100)
cvridge=cv.glmnet(train2,trainoutp1,alpha =0) # using cv to find and chose tuning parameter

cvlamdaridge = cvridge$lambda.min # finding the best lambda
ridge1 =glmnet(test2,testoutp1,alpha=0,lambda=cvlamdaridge)
ridge1pred = predict(ridge1,s= cvlamdaridge,newx = test2)
MSE=mean((testoutp1-ridge1pred)^2)
print(MSE)
ridgefinalerr=sqrt(MSE)
ridgefinalerr

# This error is less compared to our previous linear model error

#4. Fit a lasso model on the training set, with ?? chosen by cross-validation. Report the 
# test error obtained, along with the number of non-zero coefficient estimates.

# The lasso model fitting is same as the ridge model, only difference being alpha = 1

set.seed(200)
cvlasso=cv.glmnet(train2,trainoutp1,alpha =1)
cvlamdalasso = cvlasso$lambda.min # finding out the best lamda by Cross Validation
lasso1 =glmnet(test2,testoutp1,alpha=1,lambda=cvlamdalasso)
lasso1pred = predict(lasso1,d= cvlamdalasso, newx = test2)
MSEl=mean((testoutp1-lasso1pred)^2)
print(MSEl) 
lassofinalerr=sqrt(MSEl)
lassofinalerr

# the lasso model shows the least error out of all

zerocoeflasso=predict(lasso1,type="coefficients",s=cvlamdalasso)[1:length(lasso1$beta),]
zerocoeflasso[zerocoeflasso==0] #the number of zero coefficient estimates for lasso regression

# Above are the predictors that have a 0 coefficient

# 5. Comment on the results obtained. How accurately can we predict the number of college applications received? 
# Is there much difference among the test errors resulting from these five approaches?

lm1error
ridgefinalerr
lassofinalerr

# from the difference between the test errors for the all approaches, lasso has the least error rate while linear model 
# has the most!


## Part 2
# We will now try to predict per capita crime rate in the Boston data set, which is part of ISLR package.
 
# 1. Try out some of the regression methods explored in this week, such as best subset selection, the lasso, 
# and ridge regression. Present and discuss results for the approaches that you consider.

library(MASS)
data('Boston')
dim(Boston)
str(Boston)
set.seed(546)

train=sample(506,0.7*506)
# splitting the data into train and test
traindata10 = Boston[train,]
testdata10 = Boston[-train,]
response=Boston[,1]
trainoutp10 = response[train]
testoutp10 = response[-train]

#fitting a linear model and checking test error
lm1=lm(crim~.,data = Boston,subset = train)
summary(lm1)

MSE = mean((testoutp10- predict(lm1,testdata10))^2)
lm1error = sqrt(MSE)
lm1error

# linear model shows an error of 4.37

# lasso model
set.seed(121)
train2=model.matrix(crim~.,traindata10)
test2=model.matrix(crim~.,testdata10)
cvlasso=cv.glmnet(train2,trainoutp10,alpha =1)
cvlamdalasso = cvlasso$lambda.min # finding out the best lamda by Cross Validation
lasso1 =glmnet(test2,testoutp10,alpha=1,lambda=cvlamdalasso)
lasso1pred = predict(lasso1,d= cvlamdalasso, newx = test2)
#Calculating Accuracy 
MSEl=mean((testoutp10-lasso1pred)^2)
#Printing MSE
print(MSEl) 
lassofinalerr=sqrt(MSEl)
lassofinalerr

# lasso model shows an error of 3.84


#ridge model

set.seed (100)
cvridge=cv.glmnet(train2,trainoutp10,alpha =0) # using cv to find and chose tuning parameter
cvlamdaridge = cvridge$lambda.min # finding the best lambda
ridge1 =glmnet(test2,testoutp10,alpha=0,lambda=cvlamdaridge)
ridge1pred = predict(ridge1,s= cvlamdaridge,newx = test2)
#Calculating Accuracy
MSE=mean((testoutp10-ridge1pred)^2)
#Printing MSE
print(MSE)
ridgefinalerr=sqrt(MSE)
ridgefinalerr

# ridge model has an error of 3.86

lm1error
lassofinalerr
ridgefinalerr


# we can conclude that the lasso model has the least error followed by the ridge and then the linear model

# 2. Propose a model (or set of models) that seem to perform well on this data set, and
#justify your answer. Make sure that you are evaluating model performance using
#validation set error, cross-validation, or some other reasonable alternative, as opposed to using training error.

lm1=lm(crim~.,data = Boston,subset = train)
summary(lm1)
?Boston

# we can see that only dis, rad and medv truly contribute to the final outcome and now we shall fit a model using only
# the relevant predictors

lm2=lm(crim~dis+rad+medv,data = Boston, subset = train)
summary(lm2)


# 3. Does your chosen model involve all of the features in the dataset? Why or why not?

# summary of l2 when compared to l1 clearly shows that only when relevant features are used then the test error rate decreases
# as you can see the r squared value has decreased when compared to l1
# Hence, our choosen model does not have all the features for better accuracy.




