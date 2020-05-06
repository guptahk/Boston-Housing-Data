set.seed(134)
data(Boston)
index <- sample(nrow(Boston),nrow(Boston)*0.70)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]


####GAM####
library(mgcv)

Boston.gam <- gam(medv ~ s(crim) + s(zn) + s(indus) + s(nox) + s(rm) + s(age) + s(dis) + 
                    s(tax) + s(ptratio) + s(black) + s(lstat) + chas + rad, data = boston.train)
summary(Boston.gam)

#model 2 - removing s() from functions which are linear
Boston.gam1 <- gam(medv ~ s(crim) + s(zn) + s(indus) + s(nox) + s(rm) + age + s(dis) + 
                     s(tax) + s(ptratio) + s(black) + s(lstat) + chas + rad, data = boston.train)
summary(Boston.gam1)

plot(Boston.gam1, shade=TRUE,seWithMean=TRUE,scale=0, pages = 1)

#Model AIC, BIC, mean residual deviance
AIC(Boston.gam1)
BIC(Boston.gam1)
Boston.gam1$deviance

#In-sample prediction
(Boston.gam1.mse <- mean((predict(Boston.gam1) - boston.train$medv) ^ 2))

#Out-of-sample prediction - MSPE
(Boston.gam1.mspe <- mean((predict(Boston.gam1, newdata = boston.test) - boston.test$medv) ^ 2))

####NEURAL NETWORK####

library(MASS)
maxs <- apply(Boston, 2, max) 
mins <- apply(Boston, 2, min)

scaled <- as.data.frame(scale(Boston, center = mins, scale = maxs - mins))
index <- sample(1:nrow(Boston),round(0.70*nrow(Boston)))

train_boston <- scaled[index,]
test_boston <- scaled[-index,]

library(neuralnet)
n <- names(train_boston)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_boston,hidden=c(5,3),linear.output=T)
plot(nn)


pr.nn.tr <- compute(nn,train_boston[,1:13])
pr.nn_tr <- pr.nn.tr$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
train.r <- (train_boston$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)

#In sample test
MSE.nn.tr <- sum((train.r - pr.nn_tr)^2)/nrow(train_boston)
MSE.nn.tr


pr.nn <- compute(nn,test_boston[,1:13])
str(pr.nn)


pr.nn_ <- pr.nn$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
test.r <- (test_boston$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)

error.df <- data.frame(test.r, pr.nn_)
head(error.df)
library(ggplot2)
ggplot(error.df, aes(x = test.r, y = pr.nn_)) + geom_point() + stat_smooth()

# MSE of testing set
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_boston)
MSE.nn