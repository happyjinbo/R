library(tidyr)
library(dplyr)
library(tree)

Apple <- as.data.frame(drop_na(read.csv("~/Desktop/Stock return/AAPL_modified_10yr.csv", stringsAsFactors=FALSE)))
head(Apple)
summary(Apple)

# Model specification--to see the pattern of data
Apple_reg <- Apple[,c(9:17)]
head(Apple_reg)

lm.fit=lm(Return_t~.,data=Apple_reg)
summary(lm.fit)
plot(lm.fit)

# Build Apple_classification dataset
Return_t <- Apple$Return_t
Return_t_pos=ifelse (Return_t <= 0," No"," Yes ")
Apple_classification =data.frame(Apple[,c(9,11:17)], Return_t_pos)
head(Apple_classification)

#Logistic Regression to see relations between Return_t_pos and other predictors
glm.fits=glm(Return_t_pos~Vol_mi+Return_t_1+Return_t_2+Cum_return3+Cum_return2+Cum_re
turn1+Cum_inc+Cum_dec,data=Apple_classification, family=binomial)
summary(glm.fits)

#Use backward selection to check
step(glm.fits)

#Use forward selection to check if everything is the same as backward
nothing=glm(Return_t_pos~1, data=Apple_classification, family=binomial)
summary(nothing)

forwards = step(nothing, scope=list(lower=formula(nothing),upper=formula(glm.fits)),
direction="forward")

#Run a logistic regression model on the predictors from F-Selection
glm.selected=glm(Return_t_pos~Return_t_1 + Return_t_2 + Cum_return3 + Cum_return2 +
Cum_inc, data=Apple_classification, family=binomial)
summary(glm.selected)

#set up the training set and test set
set.seed(154)
train=sample(nrow(Apple_classification), nrow(Apple_classification)*0.75)
trainset_=Apple_classification[train,]
testset_=Apple_classification[-train,]

#Apply the F-Selection model predictors on the training set.
glm.train2=glm(Return_t_pos~Return_t_1 + Return_t_2 + Cum_return3 + Cum_return2 +
Cum_inc, data=trainset_, family=binomial)
summary(glm.train2)

#Apply the F-selection logistic model on the test set.
glm.probs_F=predict(glm.train2, testset_, type="response")
glm.pred=rep(0, nrow(testset_))
glm.pred[glm.probs_F>0.5]=1
table(glm.pred, testset_$Return_t_pos)
1-mean(as.integer(glm.pred)==as.integer(testset_$Return_t_pos))

#Apply the logistic regression with predictors from B-Selection on the training set
glm.train1=glm(Return_t_pos~Return_t_1 + Cum_return3 + Cum_return2+Cum_inc, data=trai
nset_, family=binomial)
summary(glm.train1)

#Apply the B-Selection logistic model on the test set.
glm.probs=predict(glm.train1, testset_, type="response")
glm.pred=rep(0, nrow(testset_))
glm.pred[glm.probs>0.5]=1
table(glm.pred, testset_$Return_t_pos)
1-mean(as.integer(glm.pred)==as.integer(testset_$Return_t_pos))

##a) Ridge Regression
library(glmnet)
library(ggplot2)
x = as.matrix(Apple_reg[,-2])
y = Apple_reg[,2]
set.seed(154)
train = sample(1:nrow(x), nrow(x)*0.75)
test = (-train)
y.test = y[test]

##Ridge MSE when lambda=1
grid = 10^seq(10, -2, length = 10000)
ridge.mod1=glmnet(x[train,], y[train], alpha=0, lambda=grid)
ridge.pred=predict(ridge.mod1, s=1, newx=x[test,])
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = 1)[1:9, ]
paste("MSE for Ridge when lambda=1: ", mean((ridge.pred-y.test)^2) )

##find best lambda
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)bestlam = cv.out$lambda.min
paste("bestlam for Ridge: ", bestlam)

##MSE of best lambda
ridge.pred = predict(ridge.mod1, s = bestlam, newx = x[test, ])
paste("MSE for best lambda: ", mean((ridge.pred - y.test)^2))
# estimated coefficients for best lambda
predict(out, type = "coefficients", s = bestlam)[1:9, ]

##Too see whether ridge regression can do the variable selection
plot(ridge.mod1, label =T )
paste("To see the above result, it shows ridge regression does not perform variable s
election! ")

##b) LASSO with lambda=1
lasso.mod2=glmnet(x[train,], y[train], alpha=1, lambda=grid)
lasso.pred=predict(lasso.mod2, s=1, newx=x[test,])
out = glmnet(x, y, alpha = 1, lambda = grid)
predict(out, type = "coefficients", s = 1)[1:9, ]
paste("MSE for LASSO: ", mean((lasso.pred-y.test)^2))

##find best lambda
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
paste("bestlam for LASSO: ", bestlam)

##MSE of best lambda
lasso.pred = predict(lasso.mod2, s = bestlam, newx = x[test, ])
paste("MSE for best lambda: ", mean((lasso.pred - y.test)^2))
# coefficient for best lamda
predict(out, type = "coefficients", s = bestlam)[1:9, ]
##Too see whether ridge regression can do the variable selection
plot(lasso.mod2, label =T )











