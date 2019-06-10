rm(list = ls())
#install.packages("rpart.plot")
library(caret)
library(ggplot2)
library(ISLR)
library(pROC)
library(rpart.plot)

data = Carseats
data = na.omit(data)
data$High = rep("No", length(data$Sales))
data$High[data$Sales > 8] = "Yes" 
data$High = as.factor(data$High)
str(data)
table(data$High)
data = data[,-1]
head(data)
#######################################################################################


train_control = trainControl(method = "cv",
                             number = 5,
                             verbose = T)

model_tree = train(High ~ ., 
                   data = data, 
                   method = "adaboost",
                   trControl = train_control,
                   tuneLength = 10)

plot(model_tree)
model_tree$results

pred_tree = predict(model_tree,newdata = data, type = "raw")
cm = confusionMatrix( pred_tree, data = data$High)
cm

imp = varImp(model_tree)
ggplot(imp)

