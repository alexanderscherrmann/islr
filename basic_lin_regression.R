library(MASS)
library(ISLR)
library(tidyverse)
names(Boston)
?Boston
# just plot
plot(medv~lstat,Boston)
lin_fit <- lm(medv~lstat,data=Boston)
# statistics to the fit
summary(lin_fit)
# helpline
abline(lin_fit,col="red")

## confidence interval 95%
confint(lin_fit)

predict(lin_fit,data.frame(lstat=c(5,10,15)),interval="confidence")


### multiple regressions
fit2 <- lm(medv~lstat+age,data=Boston)
### all variables
fit3 <- Boston %>% lm(medv~.,data=.)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

## update previous fit, removeing age, indus
fit4 <- update(fit3, ~. -age -indus, data=Boston)
fit5 <- lm(medv~lstat*age,data=Boston)

## quadratic term added via identity function I(), which lets everything inside being executed without further 
## evaluation in the call context

fit6 <- lm(medv~lstat +I(lstat^2),data=Boston)

par(mfrow=c(1,1))
plot(Boston$medv~Boston$lstat)
points(Boston$lstat,fitted(fit6),col="red",pch=20)

## polynomials can be fitted with
fit7 <- lm(medv~poly(lstat,4))
plot(1:20,1:20,pch=1:20,cex=4)


### function definition and calling
plot_func <- function(x,y,...){
  fit <- lm(y~x)
  plot(y~x,...)
  abline(fit,col="red")
}

plot_func(x=1:20,y=1:20,pch=1:20,cex=4)
