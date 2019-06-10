### Regularization (Ridge Regression and the Lasso) ###


library(ISLR)

data(package = "ISLR")
help("Hitters")
names(Hitters)
# We wish to predict salary on the basis of other predictors from Hitters data set.

fix(Hitters)                                                # We note that the Salary variable
                                                            # is missing for some of the players
dim(Hitters)
sum(is.na(Hitters$Salary))                                  # is.na() identifing the missing observations

Hitters=na.omit(Hitters)                                    # na.omit() removes all the rows that have missing value 
dim(Hitters)

sum(is.na(Hitters))



###Best Subset Selection###
library (leaps)
regfit.full=regsubsets (Salary~.,Hitters, nvmax=19 )
reg.summary=summary(regfit.full)
reg.summary
reg.summary$rsq
reg.summary$rss
reg.summary$adjr2


par(mfrow =c(2,2))


plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
       ylab=" Adjusted RSq",type="l")
plot(reg.summary$rsq ,xlab =" Number of Variables ",
     ylab="RSq",type="l")




### Ridge Regression ###

#install.packages("glmnet")
library(glmnet)
library(pls)

x=model.matrix(Salary~.,Hitters)[,-1]                       # Create a matrix corresponding to the 19 predictors
x                                                            # and automatically transform any qualitative 
                                                            # variables into dummy variables
y=Hitters$Salary
y
# Compare to see the difference for qualitative variables.
x[1:2,]
Hitters[1:2,]

grid=10^seq(10,-2,length=100)                                     # Creating a grid of values ranging from \lambda=10^10
grid
                                                                  # to 10^(-2)
ridge_mod=glmnet(x,y,alpha=0,lambda=grid, standardize = FALSE)    # \alpha=0 for ridge regression, \alpha=1 for the Lasso

coef(ridge_mod)

dim(coef(ridge_mod))                                              # is a 20x100 matrix, with 20 rows (one for each
#plot(ridge_mod)                                                                  # predictor plus an intercept) and 100 columns
                                                                  # (one for each value of \lambda)


# We expect the coefficient estimates to be much smaller, in terms of l_2 norm, when a large value of
# \lambda is used, as compared to when a small value of \lambda is used.

ridge_mod$lambda[50]                                        # \lambda=11497.57
coef(ridge_mod)[,50]
sqrt(sum(coef(ridge_mod)[-1,50]^2))                         # l_2 norm of the coefficients=6.36


ridge_mod$lambda[60]                                        # \lambda=705.48
coef(ridge_mod)[,60]
sqrt(sum(coef(ridge_mod)[-1,60]^2))                         # l_2 norm of the coefficients=57.11


predict(ridge_mod,s=50,type="coefficients")[1:20,]          # predicting ridge regression coefficients for a new
                                                            # value of \lambda, e.g. \lambda=50


# We now split the samples into a training set and a test set in order to estimate the test error of
# ridge regression and the lasso. 

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)

train 

test=(-train)

test

y_test=y[test]
y_train=y[train]

x[train,]

y_train
y_test



pcr.fit=pcr(Salary~., data=Hitters ,subset=train ,scale =TRUE ,validation ="CV")  #Principal component regression
validationplot(pcr.fit ,val.type="MSEP")


# We fit a ridge regression model on the training set, and evaluate its MSE on the test set, using
# \lambda = 4.

ridge_mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge_pred=predict(ridge_mod,s=4,newx=x[test,])

ridge_pred

mean((ridge_pred-y_test)^2)                                 # Test MSE=101036.8

# Note that if we had instead simply fit a model with just an intercept, we would have predicted
# each test observation using the mean of the training observations.

mean((mean(y[train])-y_test)^2) # Test MSE=193253.1


# We could also get the same result by fitting a ridge regression model with a very large value
# of \lambda. Note that 1e10 means 10^10.

ridge_pred=predict(ridge_mod,s=1e10,newx=x[test,])
mean((ridge_pred-y_test)^2)                                 # Test MSE=193253.1

# Recall that ordinary least squares (OLS) is simply ridge regression with \lambda=0.

ridge_pred=predict(ridge_mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge_pred-y_test)^2)                                 # OLS regression Test MSE=114783.1

lm_fit=lm(y~x, subset=train)
summary(lm_fit)
predict(ridge_mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]


# In general, instead of arbitrarily choosing \lambda=4, it would be better to use cross-validation
# to choose the tuning parameter \lambda.

set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],alpha=0, nfolds = 10)
plot(cv_out)
bestlam=cv_out$lambda.min
bestlam                                                     # lambda=211.74

ridge_pred=predict(ridge_mod,s=bestlam,newx=x[test,])
mean((ridge_pred-y_test)^2)                                 # Test MSE=96015.51

# Finally, we refit our ridge regression model on the full data set, using the value of \lambda
# chosen by cross-validation, and examine the coefficient estimates.

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# As expected, none of the coefficients are zero-ridge regression does not perform variable selection!



### The Lasso ###


# Whether the lasso can yield either a more accurate or a more interpretable model than ridge regression?

lasso_mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso_mod)                                             # Depending on the choice of \lambda, some of the
                                                            # coefficients will be exactly equal to zero

set.seed(1)
cv_out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv_out)
bestlam=cv_out$lambda.min
bestlam                                                     # lambda=16.78
lasso_pred=predict(lasso_mod,s=bestlam,newx=x[test,])
mean((lasso_pred-y_test)^2)                                 # Test MSE=100743.4

# This (Test MSE=100743.4) is substantially lower than the test set MSE of the null model (Test MSE=193253.1)
# and of ordinary least squares (Test MSE=114783.1) and very similar to the test MSE of ridge regression with
# \lambda chosen by cross-validation (Test MSE=96015.51).

# However, the lasso has a substantial advantage over ridge regression in that the resulting coefficient
# estimates are sparse. Here we see that 12 of the 19 coefficient estimates are exactly zero.
# So the lasso model with \lambda chosen by cross-validation contains only 7 variables.

out=glmnet(x,y,alpha=1,lambda=grid)
lasso_coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso_coef

lasso_coef[lasso_coef!=0]





