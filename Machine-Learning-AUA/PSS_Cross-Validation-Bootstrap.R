### Cross-Validation and the Bootstrap ###

### The Validation Set Approach ###

library(ISLR)
help("Auto")

set.seed(1)
train=sample(392,196)                                       # split the set of obsevations into two halves

lm_fit=lm(mpg~horsepower,data=Auto,subset=train)            # Simple linear regression using only the observations
                                                            # corresponding to the "train" subset

attach(Auto)
mean((mpg-predict(lm_fit,Auto))[-train]^2)                  # Calculate Test MSE=26.14

lm_fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)   # The same process for quadratic 
mean((mpg-predict(lm_fit2,Auto))[-train]^2)                 # regression, Test MSE=19.82 

lm_fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)   # The same process for cubic 
mean((mpg-predict(lm_fit3,Auto))[-train]^2)                 # regression, Test MSE=19.78

set.seed(2)                                                 # Choose a different
train=sample(392,196)                                       # training set

lm_fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm_fit,Auto))[-train]^2)                  # Test MSE=23.30

lm_fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm_fit2,Auto))[-train]^2)                 # Test MSE=18.90

lm_fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm_fit3,Auto))[-train]^2)                 # Test MSE=19.26



### Leave-One-Out Cross-Validation ###

library(boot)

glm_fit=glm(mpg~horsepower,data=Auto)
coefficients(glm_fit)

cv_err=cv.glm(Auto,glm_fit)                                 # Leave-One-Out Cross-Validation (LOOCV)
cv_err$delta                                                # LOOCV estimate for the Test MSE=24.23
                                                            # bias-corrected version=24.23

cv_error=rep(0,5)                                           # Repeat the procedure for regression 
for (i in 1:5){                                             # with higher order polynomials
  glm_fit5=glm(mpg~poly(horsepower,i),data=Auto)
  cv_error[i]=cv.glm(Auto,glm_fit5)$delta[1]
}
cv_error



### k-Fold Cross-Validation ###

glm_fit=glm(mpg~horsepower,data=Auto)
cv_err_10=cv.glm(Auto,glm_fit,K=10)                         # K-fold Cross-Validation
cv_err_10$delta                                             # K-fold CV estimate (average of MSE-s)=24.31
                                                            # bias-corrected version=24.29

set.seed(17)
cv_error_10=rep(0,5)
for (i in 1:5){
  glm_fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv_error_10[i]=cv.glm(Auto,glm_fit,K=10)$delta[1]
}
cv_error_10



### The Bootstrap ###

help(Portfolio) 
str(Portfolio)
summary(Portfolio)
head(Portfolio)


alpha_fn=function(data,index){                              # Create a function that returns \alpha
  X=data$X[index]                                           # that minimizes the Var(\alpha*X+(1-\alpha)*Y)
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha_fn(Portfolio,1:100)                                   # Estimate \alpha using all 100 observations  

set.seed(1)
alpha_fn(Portfolio,sample(100,100,replace=T))               # Constructing new bootstrap data and
                                                            # recalculate alpha

boot(Portfolio,alpha_fn,R=1000)                             # bootstrap estimate \alpha=0.5758, SE(\alpha)=0.0886



### Estimating the Accuracy of a Linear Regression Model ###

boot_fn=function(data,index){
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}
boot_fn(Auto,1:392)                                         # \beta_0=39.936, \beta_1=-0.158

set.seed(1)
boot_fn(Auto,sample(392,392,replace=T))                     # \beta_0=38.739, \beta_1=-0.148
boot_fn(Auto,sample(392,392,replace=T))                     # \beta_0=40.038, \beta_1=-0.160

boot(Auto,boot_fn,1000)                                     # bootstrap estimate SE(\beta_0)=0.887,
                                                            # SE(\beta_1)=-0.0077

summary(lm(mpg~horsepower,data=Auto))$coef                  # SE(\beta_0)=0.717, SE(\beta_1)=-0.0064

# The bootstrap approach giving a more accurate estimates of the SE(\beta_0), SE(\beta_1)
# because of the less assumptions (e.g., an estimation of the variance sigma^2 by RSE)


boot_fn2=function(data,index){
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
}

set.seed(1)
boot(Auto,boot_fn2,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

# Since the quadratic model provides good fit to the data there is better cororrespondence between
# the bootstrap estimates and the standard estimates of SE(\beta_0), SE(\beta_1) and SE(\beta_2)



