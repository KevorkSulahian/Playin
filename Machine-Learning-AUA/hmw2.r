# problem 1

set.seed(1)

x = rnorm(100, 0, 1)

e = rnorm(100,0,0.25)

y = -1 + x*0.5 + e

plot(x,y)

# It is visible that we have a linear regression 

model <- lm(y~x)

summary(model)

# comment

# F is higher than one
# p value is low
# we can reject the null

# for every increase in x our intercept increases by 0.48

plot(x,y)
abline(model, col ="red") 
legend(-1,-2, c("Regression line"), lwd=1, col="red",bty ="n")

  model2 <- lm(y~x+x^2)
summary(model2)

# there are no big difference 
# between the 2 models


# problem 2

library(ISLR)
data("Auto")
attach(Auto)

model3 <- lm(mpg~horsepower, data = Auto)
summary(model3)

# yes there's a relationship strong one

# for every increase in horsepower -0.1 decrease in mpg

predict(model3, data.frame(horsepower=c(98)), interval="confidence")
predict(model3, data.frame(horsepower=c(98)), interval="prediction")

# F is larger than 1 
# p is close to 0
# we can reject null
confint(model3)

plot(horsepower,mpg)
abline(model3)

# based on the plot and previous summary we can surly say that there is
# a negative relationship between the horsepower and mpg
# i.e. more hp less mpg


model3_resid <- resid(model3)

plot(horsepower, model3_resid)
abline(0,0)
# we can visually see the error between the actual predictor and what we predicted
par(mfrow=c(2,2))
plot(lm.fit)
# there is some evidence of non-linearity.

# problem 3

data("Carseats")
attach(Carseats)

model4 <- lm(Sales~Price+Urban+US)

summary(model4)

# the model tells us that there isn't an important relationship between the Sales and Urban(when it's Yes)
# based on the high pr value and most importantly the "*" :D


model5 <- lm(Sales~Price+US)
summary(model5)

# based on the RSE and R,R^2 we can say that they're similar with slight better performance with model5

confint(model5)

par(mfrow=c(2,2))
plot(model5)
# we can see some high leverage in the 4th plot 

# problem 4

pairs(Auto)

cor(Auto[c(-1,-9)], mpg) 

model6 <- lm(mpg~ . -name, data = Auto)
summary(model6)

# there are strong relationship between mpg and weight year and origin
# there is a good relathionship between mpg and displacement
# there is no/weak relationship between mpg and cylinders hp and accerleration

# for each increase in year mpg will be changed by 0.7
par(mfrow=c(1,1))
plot(model6)

model7 <- lm(mpg~ sqrt(horsepower)+ log(cylinders) + displacement + weight + acceleration + year + origin, data = Auto)
summary(model7)

# this was one of the best combination i got, whichusing sqrt on hp made it significant and slighly upped our r2^symbols to fit linear regression models with interaction effects



model8 <- lm(mpg~  (horsepower * cylinders) + displacement + weight + acceleration + year + origin, data = Auto)
summary(model8)

# this is simply one of the highes R^2 i've got with what I've tinkered with