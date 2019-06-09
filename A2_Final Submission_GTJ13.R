library(MASS)
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
require(ISLR)
require(caret)
require(tidyverse)
library(glmnet)

# Load the data
data3 <- get(load("C:\\Users\\DELL\\Desktop\\Stat448\\A2\\Residen.Rdata"))

head(data3)
str(data3)
names(data3)
sum(is.na(data3$V104)) # Checking whether selected variable got any missing values 



#Split the data set into a training set and a test set  Part 2 Q-1



set.seed(1)
row.number <- sample(1:nrow(data3), 0.8*nrow(data3))
train = data3[row.number,]
test = data3[-row.number,]
dim(train)
dim(test)



############## Linear model #################################

library(leaps)
####- Qb another method, exlcusing V105 variable 

fit.lm <- lm(`V104` ~ .-`V105`, data = train)
pred.lm <- predict(fit.lm, test)
mean((pred.lm - test$`V104`)^2)


# [1] 158797.1

M<-mean((pred.lm - test$`V104`)^2) 

RMSE <- (M)^(1/2)
RMSE
# [1] 398.4935


summary(fit.lm)
plot(fit.lm)

coef(fit.lm)

################# QC - Fit the previous model using stepwise selection on the training


# #################################Model FWD

model_fwd <- stepAIC(fit.lm, direction="forward")

pred.model_fwd <- predict(model_fwd, test)
mean((pred.model_fwd - test$`V104`)^2)

# [1] 158797.1

M<-mean((pred.model_fwd - test$`V104`)^2)

RMSE <- (M)^(1/2)
RMSE
# [1] 398.4935

summary(model_fwd)
plot(model_fwd)




# Residual standard error: 137.9 on 223 degrees of freedom
# Multiple R-squared:  0.9895,	Adjusted R-squared:  0.9861 
# F-statistic: 287.6 on 73 and 223 DF,  p-value: < 2.2e-16

##########################Model BackWard


model_back <- stepAIC(fit.lm, direction="backward")


pred.model_back <- predict(model_back, test)
mean((pred.model_back - test$`V104`)^2)
# [1] 54430.5


M<-mean((pred.model_back - test$`V104`)^2)
RMSE <- (M)^(1/2)
RMSE
# [1] 233.3035


summary(model_back)
plot(model_back)

# Residual standard error: 129.3 on 267 degrees of freedom
# Multiple R-squared:  0.9889,	Adjusted R-squared:  0.9877 
# F-statistic: 823.9 on 29 and 267 DF,  p-value: < 2.2e-16

#Qd) Fit the previous model using ridge regression on the training set,
#with ?? chosen by cross validation. Report the test RMSE obtained.

library(glmnet)
set.seed(123)




########### Ridge Regression#################


train.matrix <- model.matrix(`V104` ~ .-`V105`, data = train)
test.matrix <- model.matrix(`V104` ~ .-`V105`, data = test)
lambda <- 10^seq(4, -2, length = 100)
ridge_mod = glmnet(train.matrix, train$`V104`, alpha = 0, lambda = lambda)

dim(coef(ridge_mod))
plot(ridge_mod)    # Draw plot of coefficients

bestlambda_ridge <- ridge_mod$lambda.min
bestlambda_ridge


fit.ridge_mod <- glmnet(train.matrix, train$`V104`, alpha = 0, lambda = bestlambda_ridge)
pred.ridge_mod <- predict(fit.ridge_mod, s=bestlambda_ridge, test.matrix)

mean((pred.ridge_mod -test$`V104`)^2)

# [1] 886409.2

M<-mean((pred.ridge_mod -test$`V104`)^2)

RMSE <- (M)^(1/2)
RMSE

# [1] 941.4931


# Using previous model from Q B



train.matrix <- model.matrix(fit.lm, data = train)
test.matrix <- model.matrix(fit.lm, data = test)
lambda1 <- 10^seq(4, -2, length = 100)

cv.ridge <- cv.glmnet(train.matrix, train$`V104`, alpha = 0, lambda = lambda1)

plot(cv.ridge)

bestlambda_ridge1 <- cv.ridge$lambda.min
bestlambda_ridge1

# [1] 18.73817


fit.ridgereg1 <- glmnet(train.matrix, train$`V104`, alpha = 0, lambda = bestlambda_ridge1)
pred.ridge1 <- predict(fit.ridgereg1, s=bestlambda_ridge1, test.matrix)

mean((pred.ridge1 - test$`V104`)^2)

# [1] 47359.01

M<-mean((pred.ridge1 - test$`V104`)^2)

RMSE <- (M)^(1/2)
RMSE

# [1] 217.6213


#Using previous model from Q C

train.matrix1 <- model.matrix(model_back, data = train)

test.matrix1 <- model.matrix(model_back, data = test)

lambda2 <- 10^seq(4, -2, length = 100)

cv.ridge2 <- cv.glmnet(train.matrix1, train$`V104`, alpha = 0, lambda = lambda2)

plot(cv.ridge2)


bestlambda_ridge2 <- cv.ridge2$lambda.min
bestlambda_ridge2
# 0.05336699

fit.ridgereg2 <- glmnet(train.matrix1, train$`V104`, alpha = 0, lambda = bestlambda_ridge2)

pred.ridge2 <- predict(fit.ridgereg2, s=bestlambda_ridge2, test.matrix1)


mean((pred.ridge2 - test$`V104`)^2)
# [1] 52854.33

M<-mean((pred.ridge2 - test$`V104`)^2)
RMSE <- (M)^(1/2)
RMSE

# [1] 229.9007



##################Lasso#################################

#Q(e) Fit a the previous model using lasso on the training set, with ??
#chosen by cross validation. Report the test RMSE obtained along
#with the number of non-zero coefficient estimates.

train.matrix <- model.matrix(fit.lm, data = train)
test.matrix <- model.matrix(fit.lm, data = test)
lambda1 <- 10^seq(4, -2, length = 100)




set.seed(123)
cv_lasso <- cv.glmnet(train.matrix, train$`V104`, alpha = 1, lambda = lambda1)
summary(cv_lasso)



plot(cv_lasso)

bestlambda_lasso <- cv_lasso$lambda.min
bestlambda_lasso
# [1] 4.641589


fit_lasso <- glmnet(train.matrix, train$`V104`, alpha = 1, lambda = bestlambda_lasso)
#plot(fit_lasso)
predict_lasso <- predict(fit_lasso, s = bestlambda_lasso, newx = test.matrix)
predict_lasso


mean((predict_lasso - test$`V104`)^2)
# [1] 58428.52

M<-mean((predict_lasso - test$`V104`)^2)
RMSE <- (M)^(1/2) # Root mean squared error 
RMSE

# [1] 241.7199

summary(fit_lasso)
summary(predict_lasso)


pred_lass1 <- predict(fit_lasso, s = bestlambda_lasso, type = "coefficients")
pred_lass1
pred_lass1[pred_lass1 !=0]##Display only non-zero coefficients

# > pred_lass1[pred_lass1 !=0]
# <sparse>[ <logic> ] : .M.sub.i.logical() maybe inefficient
# [1] -5.384659e+02  9.311214e+00 -4.314091e+00  4.698011e-04  1.174849e-02  2.489788e+01  1.170725e+00  1.633981e-02  1.279974e-01
# [10]  2.313508e-04  5.637720e-03  3.038709e-03 -2.246345e-02  2.584474e-03  6.533049e-03 -3.426057e+00 -2.957208e+00  4.517363e-03
# [19]  6.808243e-03  3.191063e+01  3.486574e-03  2.043782e-03


plot(pred_lass1)







############################### Different methods######

#`V104` ~ .-`V105`, data = train


train.matrix <- model.matrix(`V104` ~ .-`V105`, data = train)
test.matrix <- model.matrix(`V104` ~ .-`V105`, data = test)
lam1 <- 10^seq(4, -2, length = 100)


set.seed(123)
cv0.lasso <- cv.glmnet(train.matrix, train$`V104`, alpha = 1, lambda = lam1)
summary(cv0.lasso)



bestlambda.lasso <- cv0.lasso$lambda.min
bestlambda.lasso



fit0.lasso <- glmnet(train.matrix, train$`V104`, alpha = 1, lambda = bestlambda.lasso)
#plot(fit_lasso)
predict0.lasso <- predict(fit0.lasso, s = bestlambda.lasso, newx = test.matrix)
predict0.lasso


mean((predict0.lasso - test$`V104`)^2)
# [1] 58428.52

M<-mean((predict0.lasso - test$`V104`)^2)
RMSE <- sqrt(M) # Root mean squared error 
RMSE

# [1] 241.7199

summary(fit_lasso)
summary(predict_lasso)


pred_lass1 <- predict(fit_lasso, s = bestlambda_lasso, type = "coefficients")
pred_lass1
pred_lass1[pred_lass1 !=0]##Display only non-zero coefficients










#############  Final Part - Explanation 



test.avg <- mean(test$`V104`)

lm.r2 <- 1 - mean((pred.lm - test$`V104`)^2) / mean((test.avg - test$`V104`)^2)

lm.step <- 1 - mean((pred.model_back - test$`V104`)^2) / mean((test.avg - test$`V104`)^2)

ridge.r2 <- 1 - mean((pred.ridge1 - test$`V104`)^2) / mean((test.avg - test$`V104`)^2)

lasso.r2 <- 1 - mean((predict_lasso - test$`V104`)^2) / mean((test.avg - test$`V104`)^2)

lm.r2
# [1] 0.9093981

#Step method
lm.step
# [1] 0.9689446

#ridged regression
ridge.r2
# [1] 0.9729792

#lasso 
lasso.r2
# [1] 0.9666635








