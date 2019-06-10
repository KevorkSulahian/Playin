
#install.packages("ISLR")
#install.packages("ggplot2")
#install.packages("gridExtra")
install.packages("MASS")

library(MASS)
data(package = "MASS")
install.packages("car")
library("car")

library(ISLR)
library(ggplot2)
library(gridExtra)
data(package = "ISLR")


###################################################################
###################################################################
###################################################################
###################!!!!Linear Regression!!!########################
###################################################################
###################################################################
###################################################################


# install.packages('forecast')
library(forecast)
library(MASS) 
library(ISLR) 

edit(Boston) # data from MASS library

#?Boston for help
#medv - median house value
#rm - average number of rooms per house
#age - average age of houses
#lstat - percent of hoseholds with low socioeconomic status

dev.new()
# The problem is predict "medv" using the remaining 13 predictors
names(Boston)
pairs(Boston)


#simple linear regression
lm_fit = lm(medv ~ lstat, data = Boston)
lm_fit

summary(lm_fit)
names(lm_fit)
mean(medv)                        # RSE/Mean=6.216/22.53281=0.2758644% error
coef(lm_fit)                      # or lm_fit$coefficients
confint(lm_fit, level = 0.95)     # 95% confidence intervals for model parameters
lm_fit$fitted.values              # Fitted (predicted) by the model values

plot(Boston$medv, Boston$lstat)
par(new = T)
plot(lm_fit$fitted.values, col = 'red')
par(new = F)


plot(lm_fit$residuals, xlab = 'X', ylab = 'Residuals')




plot(Boston$medv,Boston$lstat, ylim = c(0,40))
abline(lm_fit, col = 'red')

# confidence intervals for model parameters
confint(lm_fit)


# confidence intervals and prediction intervals for the prediction 
# of medv for a given value of lstat
predict(lm_fit, data.frame(lstat = c(150)), interval='confidence')
predict(lm_fit, data.frame(lstat = c(150)), interval='prediction')

# Diagnostic plots
par(mfrow = c(2,2))
plot(lm_fit)


# Multiple linear regression

lm_fit =lm(medv ~ lstat+age, data=Boston)
summary(lm_fit)


# All features included
lm_fit = lm(medv ~ ., data=Boston)
summary(lm_fit)


# All features included, except 'age' and 'indus' columns
lm_fit = lm(medv ~ .-age-indus, data=Boston)
summary(lm_fit)


lm_fit = lm(medv ~ .-age-indus-tax, data=Boston)
summary(lm_fit)

vif(lm_fit) # variance inflation factor

par(mfrow = c(2,2))
plot(lm_fit)

# interaction terms
lm.fit = lm(medv ~ lstat*age, data = Boston)
summary(lm_fit)

# interaction terms (other way of writing the above fitting)
lm.fit = lm(medv ~ lstat + age + lstat:age, data = Boston)
summary(lm_fit)

# non-linear terms
lm_fit2 = lm(medv ~ lstat +I(lstat^2), data = Boston)
summary(lm_fit2)

par(mfrow=c(2,2))
plot(lm_fit2)


# including higher powers

lm_fit5=lm(medv ~ poly(lstat,5))
summary(lm_fit5)


# This suggests that including additional polynomial terms, up to fifth order,
# leads to an improvement in the model fit!



# qualitative variables

fix(Carseats)
names(Carseats)


lm_fit =lm(Sales~ .+ Income:Advertising + Price:Age, data=Carseats)
summary (lm_fit)

dev.new()
par(mfrow=c(2,2))
plot(lm_fit)

################################################################################################################
##################################################Diagnostics###################################################
################################################################################################################

dev.new()
plot(lm_fit, which=c(1)) # Residuals vs Fitted values. This plot shows if residuals have non-linear patterns.
                         # There could be a non-linear relationship between predictor variables
                         # and an outcome variable and the pattern could show up in this plot
                         # if the model doesn't capture the non-linear relationship.
                         # If you find equally spread residuals around a horizontal line without distinct 
                         # patterns, that is a good indication you don't have non-linear relationships.


dev.new()
plot(lm_fit, which=c(2)) # Normal Q-Q. This plot shows if residuals are normally distributed.
                         # Do residuals follow a straight line well or do they deviate severely?
                         # It's good if residuals are lined well on the straight dashed line.


dev.new()
plot(lm_fit, which=c(3)) # sqrt(|Standardized Residuals|) vs Fitted values.
                         # This plot shows if residuals are spread equally along the ranges of predictors.
                         # This is how you can check the assumption of constant variance of error terms.
                         # It's good if you see a horizontal line with equally (randomly) spread points.


dev.new()
plot(lm_fit, which=c(4)) # Cook's Distance


dev.new()
plot(lm_fit, which=c(5)) # Residuals vs Leverage. This plot helps us to find influential cases if any.
                         # We watch out for outlying values at the upper right corner or at the lower right corner.
                         # Those spots are the places where cases can be influential against a regression line.
                         # Look for cases outside of a dashed line, Cook's distance. 
                         # When cases are outside of the Cook's distance (meaning they have high Cook's distance
                         # scores), the cases are influential to the regression results.
                         # The regression results will be altered if we exclude those cases.


dev.new()
plot(lm_fit, which=c(6)) # Cook's distance vs Leverage


dev.new()
plot(predict(lm_fit), rstudent(lm_fit))  # Studentized Residuals vs Fitted values
abline(h = -3, col = 'red')
abline(h = 3, col = 'red')               
which(abs(rstudent(lm_fit))>3)           # for identifying outliers.


hatvalues(lm_fit)                        # Leverage statistics. 
plot(hatvalues(lm_fit))                   
abline(h=2*2/506, col = 'red')           # Often use 2(p+1)/n or 3(p+1)/n threshold
which(hatvalues(lm_fit)>2*2/506)

abline(h=3*2/506, col = 'red')           # to determine high leverage points.
which(hatvalues(lm_fit)>3*2/506)


lm_fit =lm(medv ~ lstat+age, data=Boston)   

vif(lm_fit)                             # Variance inflation factors range from 1 upwards. The numerical value for
                                        #VIF tells you (in decimal form) what percentage the variance 
                                        #(i.e. the standard error squared) is inflated for each coefficient. 
                                        #For example, a VIF of 1.9 tells you that the variance of a particular 
                                        #coefficient is 90% bigger than what you would expect if there was no multicollinearity - 
                                        #- if there was no correlation with other predictors.
                                        
                                        #A rule of thumb for interpreting the variance inflation factor:
  
                                               # 1 = not correlated.
                                               # Between 1 and 5 = moderately correlated.
                                               # Greater than 5 = highly correlated.




