### Decision Trees

library(ggplot2)
library(caret)
library(ISLR)
library(MASS)
library(ROCR)
library(rpart.plot)


### Fitting Classification Trees

 rm(list = ls())
attach(Carseats)
Carseats = na.omit(Carseats)
    High = ifelse(Sales<=8,"No","Yes")
Carseats = data.frame(Carseats,High)
Carseats = Carseats[,-1]

set.seed(1)
 train_index = createDataPartition(High, p=0.7, list = F)
  test_index = -train_index
train_data_1 = Carseats[train_index,]
 test_data_1 = Carseats[test_index,]

grid = expand.grid(cp = seq(0, 0.4, by = 0.01))

train_control_1 = trainControl(method = "cv",
                               number = 10,
                               classProbs = T)

model_tree_1 = train(High ~ .,
                     data = train_data_1,
                     method = "rpart",
                     tuneLength = 30,
                     trControl = train_control_1,
                     tuneGrid = grid)

model_tree_1
ggplot(model_tree_1)

rpart.plot(model_tree_1$finalModel)
prp(model_tree_1$finalModel, digits = 4, roundint = F)

imp1 = varImp(model_tree_1)
ggplot(imp1)

 pred_tree_1raw = predict(model_tree_1, newdata = test_data_1, type = "raw")
pred_tree_1prob = predict(model_tree_1, newdata = test_data_1, type = "prob")

cm1 = confusionMatrix(pred_tree_1raw, data = test_data_1$High)
cm1

pred_tree_1 = prediction(pred_tree_1prob[,2], test_data_1$High)
ROC1 = performance(pred_tree_1, "tpr", "fpr")
plot(ROC1)
abline(0, 1, lty = 2)

perf_1 = performance(pred_tree_1, measure = "auc")
  AUC1 = perf_1@y.values
  AUC1


### Fitting Regression Trees

rm(list = ls())
attach(Boston)
 Boston = na.omit(Boston)

set.seed(1)
 train_index = createDataPartition(medv, p=0.7, list = F)
  test_index = -train_index
train_data_2 = Boston[train_index,]
 test_data_2 = Boston[test_index,]

grid = expand.grid(cp = seq(0, 0.4, by = 0.01))

train_control_2 = trainControl(method = "cv",
                               number = 10)

model_tree_2 = train(medv ~ .,
                     data = train_data_2,
                     method = "rpart",
                     tuneLength = 50,
                     trControl = train_control_2,
                     tuneGrid=grid)

model_tree_2
ggplot(model_tree_2)

rpart.plot(model_tree_2$finalModel)
prp(model_tree_2$finalModel, digits = 4, roundint = F)

imp2 = varImp(model_tree_2)
ggplot(imp2)

pred_tree_2 = predict(model_tree_2, newdata = test_data_2, type = "raw")
Test_RMSE_2 = RMSE(pred_tree_2, test_data_2$medv)
Test_RMSE_2



### Bagging and Random Forests

rm(list = ls())
 Boston = na.omit(Boston)

set.seed(1)
 train_index = createDataPartition(medv, p=0.7, list = F)
  test_index = -train_index
train_data_3 = Boston[train_index,]
 test_data_3 = Boston[test_index,]

grid3 = expand.grid(mtry = seq(2, ncol(train_data_3)-1, by = 1))

train_control_3 = trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 3,
                               allowParallel = TRUE)

model_tree_3 = train(medv ~ .,
                     data = train_data_3,
                     method = "rf",
                     trControl = train_control_3,
                     tuneGrid = grid3,
                     num.threads = 3,
                     ntree = 500)

model_tree_3
ggplot(model_tree_3)

pred_tree_3 = predict(model_tree_3, newdata = test_data_3, type = "raw")
Test_RMSE_3 = RMSE(pred_tree_3, test_data_3$medv)
Test_RMSE_3
