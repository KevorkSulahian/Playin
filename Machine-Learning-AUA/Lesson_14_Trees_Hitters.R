rm(list = ls())
#install.packages("rpart.plot")
library(caret)
library(ggplot2)
library(ISLR)
library(pROC)
library(rpart.plot)


#data(package = "ISLR")
data = Hitters
data = na.omit(data)
str(data)



############ Short data #############################
short_data = data[,c("Salary","Hits","Years")]
str(short_data)


#### Short Tree
mygrid = expand.grid(cp = c(0.1))
train_control = trainControl(method = "cv",
                             number = 5)

model_tree = train(Salary ~ ., data = short_data, 
                   method = "rpart",
                   trControl = train_control,
                   metric = "RMSE",
                   tuneLength = 500,
                   tuneGrid = mygrid)


plot(model_tree$finalModel)
text(model_tree$finalModel, pretty = 0, cex = 0.8)

prp(model_tree$finalModel, digits = 4,roundint = F)

###### Long Tree

mygrid = expand.grid(cp = c(0))  
  
train_control = trainControl(method = "cv",
                             number = 5)

model_tree = train(Salary ~ ., data = short_data, 
                   method = "rpart",
                   trControl = train_control,
                   metric = "RMSE",
                   tuneLength = 500,
                   tuneGrid = mygrid)


plot(model_tree$finalModel)
text(model_tree$finalModel, pretty = 0, cex = 0.8)

prp(model_tree$finalModel, digits = 4,roundint = F)



###### Tree Pruning

train_control = trainControl(method = "cv",
                             number = 5)

model_tree = train(Salary ~ ., data = short_data, 
                   method = "rpart",
                   trControl = train_control,
                   metric = "RMSE",
                   tuneLength = 1000)

plot(model_tree)
model_tree
plot(model_tree$finalModel)
text(model_tree$finalModel, pretty = 0, cex = 0.8)

prp(model_tree$finalModel, digits = 4,roundint = F)

##### Full Data

#mygrid = expand.grid(cp = c(0))  

train_control = trainControl(method = "cv",
                             number = 5)

model_tree = train(Salary ~ ., data = data, 
                   method = "rpart",
                   trControl = train_control,
                   metric = "RMSE",
                   tuneLength = 1550)

plot(model_tree)
model_tree
plot(model_tree$finalModel)
text(model_tree$finalModel, pretty = 0, cex = 0.8)

prp(model_tree$finalModel, digits = 4,roundint = F)
