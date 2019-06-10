rm(list = ls())
install.packages("rpart.plot")
library(caret)
library(ggplot2)
library(ISLR)
library(pROC)
library(rpart.plot)

data = read.csv("C:\\Users\\aharutyunyan\\Data\\heart.csv")

data = na.omit(data)
str(data)
table(data$AHD)
head(data)
####################################################################################

mygrid = expand.grid(cp = c(0))
train_control = trainControl(method = "cv",
                             number = 5,
                             verbose = T)

model_tree = train(AHD ~ ., 
                   data = data, 
                   method = "rpart",
                   trControl = train_control,
                   tuneGrid = mygrid,
                   control = rpart.control(minsplit = 2))

plot(model_tree)
prp(model_tree$finalModel, extra = 3, type = 4, 
    box.palette = "auto",faclen = 0)
model_tree$results

pred_tree = predict(model_tree, newdata = data, type = "raw")
cm = confusionMatrix( pred_tree, data = data$AHD)
cm

imp = varImp(model_tree)
ggplot(imp)
##############################################


train_control = trainControl(method = "cv",
                             number = 5,
                             verbose = T)

model_tree = train(AHD ~ ., 
                   data = data, 
                   method = "rpart",
                   trControl = train_control,
                   tuneLength = 500)

plot(model_tree)
prp(model_tree$finalModel, extra = 3, type = 4, 
    box.palette = "auto",faclen = 0)
model_tree$results

pred_tree = predict(model_tree, newdata = data, type = "raw")
cm = confusionMatrix( pred_tree, data = data$AHD)
cm

imp = varImp(model_tree)
ggplot(imp)



###############################################################

train_control = trainControl(method = "cv",
                             number = 5,
                             verbose = T)

model_tree = train(AHD ~ ., 
                   data = data, 
                   method = "adaboost",
                   trControl = train_control,
                   tuneLength = 5)

plot(model_tree)

pred_tree = predict(model_tree, newdata = data, type = "raw")
cm = confusionMatrix( pred_tree, data = data$AHD)
cm

imp = varImp(model_tree)
ggplot(imp)
