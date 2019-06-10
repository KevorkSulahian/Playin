#install.packages('caret')
library(ISLR)
library(ggplot2)
library(MASS)
library(gridExtra)
library(forecast)
library(car)
library(glmnet)
library(caret)


data(package  = "ISLR")
head(Credit)
summary(Credit)
str(Credit)
names(Credit)
dim(Credit)

# "ID"        "Income"    "Limit"      "Rating"    "Cards"     "Age"       "Education"  "Gender"   
# "Student"   "Married"   "Ethnicity"  "Balance"  


################################################
#### dealing with missing values
################################################

index = complete.cases(Credit)
sum(index)
dim(na.omit(Credit))

################################################
#### Pairs
################################################

#install.packages('GGally')
library(GGally)
pairs(Credit)
ggpairs(Credit)

################################################
###### Data Visualization
################################################

g1 = ggplot(data = Credit, aes(x = Rating, y = Balance, color = Student))+
  geom_point()+
  ggtitle("Balance  vs Rating") + xlab('Rating') + ylab('Balance')+
  geom_smooth(method = 'lm', se = F)


g2 = ggplot(data = Credit, aes(x = Age, y = Balance))+
  geom_point(color = 'red')+
  ggtitle("Balance  vs  Age") + xlab('Age') + ylab('Balance')+
  geom_smooth(method = 'loess', se = F)+
  xlim(c(0,80))

g3 = ggplot(data = Credit, aes(x = Student, y = Balance, color = Student))+
  geom_boxplot()+
  ggtitle("Balance  vs  Student") + xlab('Student') + ylab('Balance')+
  geom_smooth(method = 'loess', se = F)+
  labs(color = "Student")


g4 = ggplot(data = Credit, aes(x = Married, y = Balance, color = Married))+
  geom_boxplot()+
  ggtitle("Balance  vs  Married") + xlab('Married') + ylab('Balance') +
  geom_smooth(method = 'loess', se = F)+
  labs(color = "Married")

grid.arrange(g1,g2,g3,g4, nrow = 1)


################################################
#### Simple Regression
################################################

mod_1 = lm(Balance ~ Student, data = Credit)
summary(mod_1)

mean(Credit$Balance[Credit$Student == 'Yes']) 
mean(Credit$Balance[Credit$Student == 'No'])

mod_2 = lm(Balance ~ Married, data = Credit)
summary(mod_2)

mean(Credit$Balance[Credit$Married == 'Yes']) 
mean(Credit$Balance[Credit$Married == 'No'])

mod_3 = lm(Balance ~ Rating, data = Credit)
summary(mod_3)

plot(Credit$Rating, Credit$Balance, col = 'black')
abline(mod_3, col = 'red')

qplot(Credit$Rating,Credit$Balance) + 
  stat_smooth(method = "lm", se = F, col = 'red')+
  xlab('Rating') + ylab('Balance') + ggtitle('Balance vs Rating')


###############################################
#### Multiple Regression
###############################################

mod_all = lm(Balance ~ ., data = Credit)
summary(mod_all)

mod_all_important = lm(Balance ~ .-ID - Education - Ethnicity - Married - Gender, data = Credit)
summary(mod_all_important)

plot(mod_all_important$fitted.values, ylim = c(0,2000), type = 'l')
par(new = T)
plot(Credit$Balance, ylim = c(0,2000), type = 'l', col = 'red' )
par(new= F)

plot(mod_all_important$residuals)
plot(mod_all_important$fitted.values, mod_all_important$residuals)

acf(mod_all_important$residuals)
pacf(mod_all_important$residuals)

##################################################

mod_all = lm(Balance ~ Income + Limit + Cards + Student, data = Credit)
summary(mod_all)

par(mfrow = c(2,2))
plot(mod_all)
par(mfrow = c(1,1))

vif(mod_all)

plot(rstandard(mod_all) - rstudent(mod_all))


confint(mod_all)
predict(mod_all, Credit[1:3,], interval = 'confidence')
predict(mod_all, Credit[1:3,], interval = 'prediction')

###############################################
#### Regularization by glmnet package
###############################################

x = model.matrix(Balance ~ . , data = Credit)
dim(x)
x = x[,c(-1,-2)]
dim(x)
y = Credit$Balance 


####### Ridge Regression

grid = 2^seq(10,-2, length = 300)
grid
mod_ridge = glmnet(x,y, alpha = 0, lambda = grid)
plot(mod_ridge, "lambda")
dim(coef(mod_ridge))

mod_ridge$lambda[50]
coef(mod_ridge)[,50]
coef(mod_ridge)[-1,50]
sum(coef(mod_ridge)[-1,50]^2)


mod_ridge$lambda[150]
coef(mod_ridge)[,150]
sum(coef(mod_ridge)[-1,150]^2)


mod_ridge$lambda[1]
coef(mod_ridge)[,1]
sum(coef(mod_ridge)[-1,1]^2)

for (j in 1:300) {
  cat('j ->', j, 'MSE ->',mean(predict(mod_ridge, s = mod_ridge$lambda[j], newx = x) - y)^2,"\n")
}

###############
mod_ridge$a0
mod_ridge$beta
###############


####### Ridge Regression - Validation set approach

set.seed(1)
n = dim(Credit)[1]
train = sample(1:400,40)

trainX = x[train,]
trainY = y[train]

testX = x[-train,]
testY = y[-train]

grid = 2^seq(10,-2, length = 300)

error_list = c()
for (j in grid){
  mod_ridge_vsa = glmnet(trainX, trainY, alpha = 0, lambda = j, thresh = 1e-12)
  pred_ridge = predict(mod_ridge_vsa, s = j, newx = testX)
  error = sqrt(sum((pred_ridge - testY)^2))
  cat("j->", j, "error->", error, "\n")
  error_list = c(error_list, error)
}
plot(error_list)
which.min(error_list)
grid[which.min(error_list)]

mod_ridge_1 = glmnet(trainX,trainY,alpha = 0, lambda = grid, thresh = 1e-15)
pred_ridge_1 = predict(mod_ridge_1, s = grid[which.min(error_list)], newx = testX)
sqrt(sum((pred_ridge_1 - testY)^2))

mod_ridge_2 = glmnet(trainX,trainY, alpha = 0, lambda = grid, thresh = 1e-15)
pred_ridge_2 = predict(mod_ridge_2, s = 0, newx = testX)
sqrt(sum((pred_ridge_2 - testY)^2))

####### Ridge Regression - Cross Validation

set.seed(1)
set.seed(1)
n = dim(Credit)[1]
train = sample(1:400,40)

trainX = x[train,]
trainY = y[train]

testX = x[-train,]
testY = y[-train]



grid = 2^seq(10,-2, length = 300)
mod_ridge_cv = cv.glmnet(trainX, trainY, alpha = 0, lambda = grid, thresh = 1e-12)
plot(mod_ridge_cv)
bestlam = mod_ridge_cv$lambda.min
bestlam

pred_ridge = predict(mod_ridge_cv, s = bestlam, newx = testX)
sqrt(sum((pred_ridge - testY)^2))


pred_ridge = predict(mod_ridge_cv, s = 0, newx = testX)
sqrt(sum((pred_ridge - testY)^2))



####### Lasso Regression - Cross Validation

set.seed(1)
n = dim(Credit)[1]
train = sample(1:400,40)

trainX = x[train,]
trainY = y[train]

testX = x[-train,]
testY = y[-train]



grid = 2^seq(10,-2, length = 300)
mod_lasso_cv = cv.glmnet(trainX, trainY, alpha = 1, lambda = grid)
plot(mod_lasso_cv)
bestlam = mod_lasso_cv$lambda.min
bestlam

pred_lasso = predict(mod_lasso_cv, s = bestlam, newx = testX)
sqrt(sum((pred_lasso - testY)^2))


pred_lasso = predict(mod_lasso_cv, s = 0, newx = testX)
sqrt(sum((pred_lasso - testY)^2))

##########################################################
#### final model and ploting
##########################################################


grid = 2^seq(10,-2, length = 300)
lasso_final = cv.glmnet(x, y, alpha = 1, lambda = grid)
lasso_final$lambda.min


plot(lasso_final)
plot(lasso_final$glmnet.fit,"lambda")
rownames(coef(lasso_final))
coef(lasso_final)



mod_lasso = glmnet(x, y, alpha = 1, lambda = grid)
plot(mod_lasso,"lambda")
coef(mod_lasso)


#install.packages('reshape2')
library(reshape2)
data = as.data.frame(t(as.matrix(coef(mod_lasso)[-1,])))
data = data.frame(data, lambda = mod_lasso$lambda)
test_data_long = melt(data, id = "lambda")  # convert to long format

ggplot(data = test_data_long, aes(x = lambda,  y = value, colour = variable)) +
  geom_line(lwd = 1) + coord_cartesian(xlim = c(0,150), ylim = c(-15,25))  


###############################################
#### Regularization by caret package
###############################################

set.seed(100)
train = createDataPartition(Credit$Balance, p = 0.8, list = FALSE)

trainData = Credit[train,-1]
testData = Credit[-train,-1]
dim(trainData)
dim(testData)


#mygrid = expand.grid(.lambda = 2^seq(-1,-20, length = 700)) 

mycontrol = trainControl(method = "cv", 
                         number = 10, 
                         verbose = T)
                         
                         
mod_ridge = train(Balance ~., 
                 data = trainData, 
                 method ="ridge", 
                 trControl = mycontrol,
                 preProc = c("center", "scale"),
                 tuneLength = 300)
                 #tuneGrid = mygrid)
                 #tuneLength = 1000)


plot(mod_ridge)
mod_ridge$bestTune
names(mod_ridge$finalModel)



importance = varImp(mod_ridge, scale  = TRUE)
importance
plot(importance, top = 10, main = "Relative Importance",
     xlab = "Importance", ylab = "Features")


pred = predict(mod_ridge, testData)
sqrt(mean((pred - testData$Balance)^2))





