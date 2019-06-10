### Review before the midterm exam###


library(ISLR)
library(car)

data(package = "ISLR")
help("Hitters")

# We wish to predict salary on the basis of other predictors from Hitters data set.

fix(Hitters)
### We note that the Salary variable is missing for some of the players.

dim(Hitters)
sum(is.na(Hitters$Salary)) 
### is.na() identifing the missing observations.

Hitters=na.omit(Hitters)
### na.omit() removes all the rows that have missing value.
dim(Hitters)

sum(is.na(Hitters))

attach(Hitters)
names(Hitters)



### Multiple Linear Regression ###

lm1=lm(Salary~., data = Hitters)
summary(lm1)


lm2=lm(Salary~AtBat+Hits+Walks+CRuns+CWalks+Division+PutOuts+Assists, data = Hitters)
summary(lm2)


lm3=lm(Salary~AtBat+Hits+Walks+CRuns+CWalks+Division+PutOuts, data = Hitters)
summary(lm3)


AIC(lm1, lm3)
BIC(lm1, lm3)
### adjusted R^2.


hist(Salary)
hist(log(Salary))

hist(AtBat)

hist(Hits)

hist(Walks)

hist(CRuns)
hist(log(CRuns))

hist(CWalks)
hist(log(CWalks))


Hitters$logSalary=log(Hitters$Salary)
Hitters$logCRuns=log(Hitters$CRuns)
Hitters$logCWalks=log(Hitters$CWalks)

lm4=lm(logSalary ~ AtBat+Hits+Walks+logCRuns+logCWalks+Division+PutOuts, data = Hitters)
summary(lm4)

AIC(lm3, lm4)
BIC(lm3, lm4)
### adjusted R^2.



plot(lm4, which=c(1))
### Residuals vs Fitted values. This plot shows if residuals have non-linear patterns.
### There could be a non-linear relationship between predictor variables
### and an outcome variable and the pattern could show up in this plot
### if the model doesn't capture the non-linear relationship.
### If you find equally spread residuals around a horizontal line without distinct 
### patterns, that is a good indication you don't have non-linear relationships.


plot(lm4, which=c(2))
### Normal Q-Q. This plot shows if residuals are normally distributed.
### Do residuals follow a straight line well or do they deviate severely?
### It's good if residuals are lined well on the straight dashed line.


plot(lm4, which=c(3))
### sqrt(|Standardized Residuals|) vs Fitted values.
### This plot shows if residuals are spread equally along the ranges of predictors.
### This is how you can check the assumption of constant variance of error terms.
### It's good if you see a horizontal line with equally (randomly) spread points.


plot(lm4, which=c(5))
### Residuals vs Leverage. This plot helps us to find influential cases if any.
### We watch out for outlying values at the upper right corner or at the lower right corner.
### Those spots are the places where cases can be influential against a regression line.
### Look for cases outside of a dashed line, Cook's distance. 
### When cases are outside of the Cook's distance (meaning they have high Cook's distance
### scores), the cases are influential to the regression results.
### The regression results will be altered if we exclude those cases.


plot(predict(lm4), rstudent(lm4))
### Studentized Residuals vs Fitted values
abline(h = -3, col = 'red')
abline(h = 3, col = 'red')               
which(abs(rstudent(lm4))>3)           
### for identifying outliers.


hatvalues(lm4)
# Leverage statistics.

plot(hatvalues(lm4))                   
abline(h=2*8/nrow(Hitters), col = 'red')
which(hatvalues(lm3)>2*8/nrow(Hitters))
### Use 2(p+1)/n as a threshold to determine high leverage points.

### or

plot(hatvalues(lm4)) 
abline(h=3*8/nrow(Hitters), col = 'red')
which(hatvalues(lm4)>3*8/nrow(Hitters))
### Use 3(p+1)/n as a threshold to determine high leverage points.

vif(lm4)


Hitters=Hitters[-c(173,230,241),]

lm5=lm(logSalary ~ AtBat+Hits+Walks+logCRuns+logCWalks+Division+PutOuts, data = Hitters)
summary(lm5)

lm6=lm(logSalary ~ AtBat+Hits+Walks+logCRuns+Division+PutOuts, data = Hitters)
summary(lm6)

### F-stat.=132.5>>1 and corresponding p-value<2.2*10^{-16} implies that there is a relationship
### between Salary and the other variables.

### R^2=0.7586, it means that the 75.86% variability of the logSalary can be explained by these variables.


mean(Hitters$logSalary)
0.4382/5.922586
### RSE/mean(Salary)=0.074 or actual logSalary deviate from the true regression line by 0.44 units or by 7.4% on average. 

confint(lm6)
### Notice that the confidence interval for Division includes zero,
### indicating that this variable is not statistically significant.



### Ridge Regression ###

rm(list=ls())

#install.packages("glmnet")
library(glmnet)

Hitters=na.omit(Hitters)

x=model.matrix(Salary~.,Hitters)[,-1]
### Create a matrix corresponding to the 19 predictors and automatically  
### transform any qualitative variables into dummy variables
y=Hitters$Salary

### Compare to see the difference for qualitative variables.
x[1:2,]
Hitters[1:2,]

grid=10^seq(10,-2,length=100)
### Creating a grid of values ranging from \lambda=10^10 to 10^(-2)

ridge_mod=glmnet(x,y,alpha=0,lambda=grid)
### \alpha=0 for ridge regression, \alpha=1 for the Lasso

dim(coef(ridge_mod))
### is a 20x100 matrix, with 20 rows (one for each predictor plus an intercept)
### and 100 columns (one for each value of \lambda)


### We expect the coefficient estimates to be much smaller, in terms of l_2 norm, when a large value of
### \lambda is used, as compared to when a small value of \lambda is used.

ridge_mod$lambda[50]
### \lambda=11497.57
coef(ridge_mod)[,50]
sqrt(sum(coef(ridge_mod)[-1,50]^2))
### l_2 norm of the coefficients=6.36


ridge_mod$lambda[60]
### \lambda=705.48
coef(ridge_mod)[,60]
sqrt(sum(coef(ridge_mod)[-1,60]^2))
### l_2 norm of the coefficients=57.11


predict(ridge_mod,s=50,type="coefficients")[1:20,]
### predicting ridge regression coefficients for a new value of \lambda, e.g. \lambda=50


### We now split the samples into a training set and a test set in order to estimate the test error of
### ridge regression and the lasso. 

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y_test=y[test]


### We fit a ridge regression model on the training set, and evaluate its MSE on the test set,
###  using \lambda = 4.

ridge_mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge_pred=predict(ridge_mod,s=4,newx=x[test,])
mean((ridge_pred-y_test)^2)
### Test MSE=101036.8

### Note that if we had instead simply fit a model with just an intercept, we would have predicted
### each test observation using the mean of the training observations.

mean((mean(y[train])-y_test)^2)
### Test MSE=193253.1

### We could also get the same result by fitting a ridge regression model with a very large value
### of \lambda. Note that 1e10 means 10^10.

ridge_pred=predict(ridge_mod,s=1e10,newx=x[test,])
mean((ridge_pred-y_test)^2)
### Test MSE=193253.1

### Recall that ordinary least squares (OLS) is simply ridge regression with \lambda=0.

ridge_pred=predict(ridge_mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge_pred-y_test)^2)
### OLS regression Test MSE=114783.1

lm(y~x, subset=train)
predict(ridge_mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]


### In general, instead of arbitrarily choosing \lambda=4, it would be better to use cross-validation
### to choose the tuning parameter \lambda.

set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],alpha=0, nfolds = 10)
plot(cv_out)
bestlam=cv_out$lambda.min
bestlam
### lambda=211.74

ridge_pred=predict(ridge_mod,s=bestlam,newx=x[test,])
mean((ridge_pred-y_test)^2)
### Test MSE=96015.51

### Finally, we refit our ridge regression model on the full data set, using the value of \lambda
### chosen by cross-validation, and examine the coefficient estimates.

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

### As expected, none of the coefficients are zero-ridge regression does not perform variable selection!



### The Lasso ###


### Whether the lasso can yield either a more accurate or a more interpretable model than ridge regression?

lasso_mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso_mod)
### Depending on the choice of \lambda, some of the coefficients will be exactly equal to zero

set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv_out)
bestlam=cv_out$lambda.min
bestlam
### lambda=16.78
lasso_pred=predict(lasso_mod,s=bestlam,newx=x[test,])
mean((lasso_pred-y_test)^2)
### Test MSE=100743.4

### This (Test MSE=100743.4) is substantially lower than the test set MSE of the null model (Test MSE=193253.1)
### and of ordinary least squares (Test MSE=114783.1) and very similar to the test MSE of ridge regression with
### \lambda chosen by cross-validation (Test MSE=96015.51).

### However, the lasso has a substantial advantage over ridge regression in that the resulting coefficient
### estimates are sparse. Here we see that 12 of the 19 coefficient estimates are exactly zero.
### So the lasso model with \lambda chosen by cross-validation contains only 7 variables.

out=glmnet(x,y,alpha=1,lambda=grid)
lasso_coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso_coef

lasso_coef[lasso_coef!=0]




