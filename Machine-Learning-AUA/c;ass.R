library(ISLR)
library(e1071)
library(ggplot2)
library(caret)
??plotROC

train_index <- createDataPartition(Credit$Balance, p = 0.8, list = F)

train_data <- Credit[train_index,]
test_data <- Credit[-train_index,]

train_control <- trainControl(method = "cv",
                              number = 10,
                              verboseIter = T)
model_knn <- train(Balance ~., data = train_data,
                   method ="knn",
                   trControl = train_control,
                   tuneLength = 60,
                   metric= "RMSE")

ggplot(model_knn, metric = "RMSE")

model_knn$bestTune
