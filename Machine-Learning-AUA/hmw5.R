german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

german_credit$response = german_credit$response - 1
german_credit$response <- as.factor(german_credit$response)
german_credit$response <- make.names(german_credit$response)
library(caret)
library(MASS)
set.seed(1)
trainIndex <- createDataPartition(german_credit$response, p = .8, list = F)

train <- german_credit[trainIndex,]
test <- german_credit[-trainIndex,]

## 1)

mygrid = expand.grid(cp = c(0))
train_control = trainControl(method = "cv",
                             number = 10,
                             classProbs = T,
                             savePredictions = T)

model_tree = train(response ~., 
                   data = train, 
                   method = "rpart",
                   trControl = train_control,
                   tuneLength = 40)


pred_tree = predict(model_tree,newdata = test, type = "raw")
test$response <- as.factor(test$response)
cm_tree = confusionMatrix(pred_tree, data = test$response)
cm_tree

library(ROCR)
pred_tree = predict(model_tree,newdata = test, type = "prob")[,2]
pred_tree2 = prediction(pred_tree, test$response)

plot(performance(pred_tree2, 'tpr', 'fpr'), main ="TEST PREDICTION")

pred_tree = predict(model_tree,newdata = train, type = "prob")[,2]
pred_tree2 = prediction(pred_tree, train$response)

plot(performance(pred_tree2, 'tpr', 'fpr'), main ="TRAIN PREDICTION")

library(rpart.plot)
prp(model_tree$finalModel,digits = 4,roundint = F)


### 2)

model_tree = train(response ~ ., 
                   data = train, 
                   method = "rf",
                   trControl = train_control,
                   tuneLength = 50)

pred_tree = predict(model_tree,newdata = test, type = "raw")
cm_RF = confusionMatrix( pred_tree, data = test$response)
cm_RF

pred_tree = predict(model_tree,newdata = test, type = "prob")[,2]
pred_tree2 = prediction(pred_tree, test$response)

plot(performance(pred_tree2, 'tpr', 'fpr'), main ="TEST PREDICTION")

pred_tree = predict(model_tree,newdata = train, type = "prob")[,2]
pred_tree2 = prediction(pred_tree, train$response)

plot(performance(pred_tree2, 'tpr', 'fpr'), main ="TRAIN PREDICTION")


### 3)

train_control = trainControl(method = "cv",
                             number = 5,
                             verbose = T)

model_tree = train(response ~ ., 
                   data = train, 
                   method = "adaboost",
                   trControl = train_control,
                   tuneLength = 10)


pred_tree = predict(model_tree,newdata = test, type = "raw")
cm_boost = confusionMatrix( pred_tree, data = test$response)
cm_boost

pred_tree = predict(model_tree,newdata = test, type = "prob")[,2]
pred_tree2 = prediction(pred_tree, test$response)

plot(performance(pred_tree2, 'tpr', 'fpr'), main ="TEST PREDICTION")

pred_tree = predict(model_tree,newdata = train, type = "prob")[,2]
pred_tree2 = prediction(pred_tree, train$response)

plot(performance(pred_tree2, 'tpr', 'fpr'), main ="TRAIN PREDICTION")


maximus <- which.max(c(cm_RF$overall[[1]],cm_boost$overall[[1]], cm_tree$overall[[1]]))
maximus

# 4

c(cm_RF$overall[[1]],cm_boost$overall[[1]], cm_tree$overall[[1]])[maximus]

# I will let it choose for its self 