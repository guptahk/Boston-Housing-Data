library(MASS)
data(Boston)

#sampling the data in to training and testing
sample_index <- sample(nrow(Boston),nrow(Boston)*0.70)
Boston_train <- Boston[sample_index,]
Boston_test <- Boston[-sample_index,]


#linear regression model
#medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat

lrmodel <- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data=Boston_train)
model_summary <- summary(lrmodel)
#mse
(model_summary$sigma)^2
#mspe - out of sample
pi <- predict(object = lrmodel, newdata = Boston_test)
mean((pi - Boston_test$medv)^2)

#regression tree
library(rpart)
library(rpart.plot)

boston.rpart <- rpart(formula = medv ~ ., data = Boston_train)
#in sample prediction
boston.train.pred.tree = predict(boston.rpart)
MSE.tree <- mean((boston.train.pred.tree - Boston_train$medv)^2)
#out of sample
boston.test.pred.tree = predict(boston.rpart,Boston_test)
MSPE.tree <- mean((boston.test.pred.tree - Boston_test$medv)^2)
#plotting the tree
prp(boston.rpart,digits = 4, extra = 1)

#bagging
library(ipred)
boston.bag<- bagging(medv~., data = Boston_train, nbagg=100)
boston.bag
#prediction
boston.bag.pred<- predict(boston.bag, newdata = Boston_test)
mean((Boston_test$medv-boston.bag.pred)^2)
#out of bag
boston.bag.oob<- bagging(medv~., data = Boston_train, coob=T, nbagg=100)
boston.bag.oob


#random forest
library(randomForest)
boston.rf<- randomForest(medv~., data = Boston_train, importance=TRUE)
boston.rf
#oob error plot with each tree
plot(boston.rf$mse, type='l', col=2, lwd=2, xlab = "ntree", ylab = "OOB Error")
#pred on test sample
boston.rf.pred<- predict(boston.rf, Boston_test)
mean((Boston_test$medv-boston.rf.pred)^2)
#variable importance
boston.rf$importance

#boosting
library(gbm)
boston.boost<- gbm(medv~., data = Boston_train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 8)
summary(boston.boost)



#pred on test sample
boston.boost.pred.test<- predict(boston.boost, Boston_test, n.trees = 10000)
mean((Boston_test$medv-boston.boost.pred.test)^2)


#We can investigate how the testing error changes with different number of trees.
ntree<- seq(100, 10000, 100)
predmat<- predict(boston.boost, newdata = Boston_test, n.trees = ntree)
err<- apply((predmat-Boston_test$medv)^2, 2, mean)
plot(ntree, err, type = 'l', col=2, lwd=2, xlab = "n.trees", ylab = "Test MSE")
abline(h=min(test.err), lty=2)


