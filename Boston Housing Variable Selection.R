library(MASS)
library(dplyr)
library(corrr)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DT)
library(MASS)
library(leaps)
library(glmnet)
library(PerformanceAnalytics)
library(GGally)
library(tidyr)


data("Boston")
#Sampling of 70% training data
set.seed(134)
index <- sample(nrow(Boston),nrow(Boston)*0.70)
boston_train <- Boston[index,]
boston_test <- Boston[-index,]

##################
#1. Best linear regression model using AIC, BIC and Lasso

#1.1 Stepwise Regression Using AIC
nullmodel=lm(medv~1, data=boston_train)
fullmodel=lm(medv~., data=boston_train) 

model_step_s <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both')

sum.model.step <- summary(model_step_s)
model.step.mse <- (sum.model.step$sigma)^2
model.step.rsq <- sum.model.step$r.squared
model.step.arsq <- sum.model.step$adj.r.squared
test.pred.model.step <- predict(model_step_s, newdata=boston_test[, 1:13]) 
model.step.mspe <- mean((boston_test$medv-test.pred.model.step)^2)
model.step.aic <- AIC(model_step_s)
model.step.bic <- BIC(model_step_s)

stats.model.step <- matrix(c("Model.Stepwise","7", model.step.mse, model.step.rsq, model.step.arsq, model.step.mspe,
                             model.step.aic,model.step.bic))
rownames(stats.model.step) <- c("Model type", "Number of Predictor Variables","MSE", "R-Squared", "Adjusted R-Squared", "Test MSPE","AIC","BIC")
colnames(stats.model.step) <- c("Parameters")
stepwise.table <- as.table(stats.model.step)

#1.2 LASSO
lasso_fit = glmnet(x = as.matrix(boston_train[, -c(which(colnames(boston_train)=='medv'))]),
                   y = boston_train$medv, alpha = 1)

#use 5-fold cross validation to pick lambda
cv_lasso_fit = cv.glmnet(x = as.matrix(boston_train[, -c(which(colnames(boston_train)=='medv'))]), 
                         y = boston_train$medv, alpha = 1, nfolds = 5)
plot(cv_lasso_fit)
cv_lasso_fit$lambda.min
cv_lasso_fit$lambda.1se

#lambda = lambda.1se
coef(lasso_fit,s=cv_lasso_fit$lambda.1se)

#Prediction on training dataset
pred.lasso.train <- predict(lasso_fit, newx = as.matrix(boston_train[, -c(which(colnames(boston_train)=='medv'))]),
                            s=cv_lasso_fit$lambda.1se)

#Prediction on Test
pred.lasso.test <- predict(lasso_fit, newx =as.matrix(boston_test[, -c(which(colnames(boston_test)=='medv'))]),
                           s=cv_lasso_fit$lambda.1se)

#MSE

lasso.mse <- sum((as.matrix(boston_train[, c(which(colnames(boston_train)=='medv'))]) - pred.lasso.train)^2)/(354-7)

#MSPE

lasso.mspe <- mean((as.matrix(boston_test[, c(which(colnames(boston_test)=='medv'))]) - pred.lasso.test)^2)

#R_squared
sst <- sum((as.matrix(boston_train[, c(which(colnames(boston_train)=='medv'))]) - mean(as.matrix(boston_train[, c(which(colnames(boston_train)=='medv'))])))^2)
sse_1se <- sum((as.matrix(boston_train[, c(which(colnames(boston_train)=='medv'))])-pred.lasso.train)^2)
rsq_1se <- 1 - sse_1se / sst

#adj_R_squared
adj_rsq_1se <- 1 - (dim(as.matrix(boston_train[, -c(which(colnames(boston_train)=='medv'))]))[1]-1)*(1-rsq_1se)/(dim(as.matrix(boston_train[, -c(which(colnames(boston_train)=='medv'))]))[1]-7-1)

stats.model.lasso.1se <- matrix(c("model.lasso.1se","7", lasso.mse, rsq_1se, adj_rsq_1se, lasso.mspe))

rownames(stats.model.lasso.1se) <- c("Model type","Number of Predictor Variables", "MSE", "R-Squared", "Adjusted R-Squared", "Test MSPE")
colnames(stats.model.lasso.1se) <- c("Parameters")
model.lasso.table <- as.table(stats.model.lasso.1se)


Comparison.models <- cbind(stepwise.table[1:6,],model.lasso.table)

#Based on the three tables, we can clearly see that Stepwise (with AIC) is producing better results
#Hence we go with model_step_s as our final model

###Residual Diagnosis
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(2,2))
plot(model_step_s)

#We do a quick residual analysis of the selected subset model and 
#observe the following:

#The variance is not completely constant and hence the assumption of constant 
#variance is not totally satisfied
#From the q-q plot we see that it is not completely normal and a little 
#skewed to the right

##Part (iv) - Cross-validation
model.cv <- glm(medv~lstat+rm+ptratio+black+dis+nox+zn, data = Boston)
cv.boston <- cv.glm(data = Boston, glmfit = model.cv, K = 3)
cv.boston$delta[2]
