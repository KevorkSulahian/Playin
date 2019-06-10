#Problem 1

library(ISLR)
names(College)
attach(College)
# View(College)

library(caret)
# 1)
set.seed(1)
index <- createDataPartition(College$Private, p = .8, list = F)

train <- College[index,]
test <- College[-index,]

library(MASS)
## QDA
#2)
qda_fit <- qda(Private~., data = train)


qda_pred <- predict(qda_fit,test)
cm_test <- confusionMatrix(qda_pred$class, data = test$Private, positive = 'No')
# This is the acc
cm_test$overall[1]

# 3)
qda_pred <- predict(qda_fit,train)
cm_train <- confusionMatrix(qda_pred$class, data = train$Private, positive = 'No')
# This is the acc
cm_train$overall[1]

# 4)
cm_test
cm_train
max_acc <- which.max(c(cm_test$overall[1], cm_train$overall[1]))
max_acc

# We get the the train has overall better accuracy but this 
# can be simply because of overfitting 





# Problem 2
# install.packages('mlbench')
library(mlbench)
data(Shuttle)
attach(Shuttle)
# View(Shuttle)

#1)
set.seed(1)
index <- createDataPartition(Shuttle$Class,p = .8, list = F)

train <- Shuttle[index,]
test <- Shuttle[-index,]


# 2) IMPORTANT... in none of our class we ever changed the metrics, let alone create a new one
# I've spent most of the time during the final exam searching on how to create a new metric and
# apply it in a way that it could give us what we need but, I failed to do so and I don't 
# believe this is my fault. You could have at least e-mailed us about this new topic a day before

train_control <- trainControl(method = 'cv',
                              number = 5)
model_knn <- train(Class~.,
                   data=train,
                   method='knn',
                   trControl = train_control,
                   tuneLength= 20)
library(ggplot2)
ggplot(model_knn, metric = 'Accuracy')
model_knn$bestTune
# k = 5 was chosen

model_predict_train = predict(model_knn, newdata = train)
train_tree_cm <- confusionMatrix(model_predict_train, data = train$Class)
train_tree_cm$byClass
# 3)
model_predict_test = predict(model_knn, newdata = test)
test_tree_cm <-confusionMatrix(model_predict_test, data = test$Class)
test_tree_cm$byClass

second_max_boy <- which.max(c(test_tree_cm$overall[[1]], train_tree_cm$overall[[1]]))

c(test_tree_cm$overall[1], train_tree_cm$overall[1])[second_max_boy]

# similarly train is higher





# Problem 3
data("College")
attach(College)
# 1)
set.seed(1)
index <- createDataPartition(College$Private, p = .8, list = F)

train <- College[index,]
test <- College[-index,]

mygrid = expand.grid(cp = c(0))
train_control = trainControl(method = "cv",
                             number = 5,
                             classProbs = T,
                             savePredictions = T)
model_tree = train(Private ~., 
                   data = train, 
                   method = "rpart",
                   trControl = train_control,
                   tuneLength = 20)

library(rpart.plot)
prp(model_tree$finalModel,digits = 4,roundint = F)

pred_tree = predict(model_tree,newdata = test, type = "raw")
cm_tree = confusionMatrix(pred_tree, data = test$Private, positive = 'No')
cm_tree$byClass[1]

library(ROCR)
pred_tree = predict(model_tree,newdata = test, type = "prob")[,2]
pred_tree2 = prediction(pred_tree, test$Private)

plot(performance(pred_tree2, 'prec', 'rec'), main ="TEST PREDICTION", colorize = T)

#Boosting

# 2)

train_control = trainControl(method = "cv",
                             number = 3,
                             verbose = T)
model_tree = train(Private ~ ., 
                   data = train, 
                   method = "adaboost",
                   trControl = train_control,
                   tuneLength = 5)

pred_tree = predict(model_tree,newdata = test, type = "raw")
cm_boost = confusionMatrix( pred_tree, data = test$Private, positive = 'No')
cm_boost$byClass[1]

pred_tree.1 = predict(model_tree,newdata = test, type = "prob")[,2]
pred_tree2.1 = prediction(pred_tree.1, test$Private)

plot(performance(pred_tree2.1, 'prec', 'rec'), main ="TEST PREDICTION", colorize = T)


maximus <- which.max(c(cm_boost$overall[[1]], cm_tree$overall[[1]]))
maximus

c(cm_boost$overall[[1]], cm_tree$overall[[1]])[maximus]


plot(performance(pred_tree2, 'prec', 'rec'), main ="P", col= 'red')
plot(performance(pred_tree2.1, 'prec', 'rec'), add = T, col = 'blue')

# overall it's obvious that the boost  is much better