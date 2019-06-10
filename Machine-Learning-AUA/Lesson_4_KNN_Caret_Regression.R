rm(list = ls())
library(caret)
library(ggplot2)
library(ISLR)


train_index = createDataPartition(Credit$Balance, p = 0.8, list = F) 
train_index  

train_Data = Credit[train_index,]
test_Data = Credit[-train_index,]
dim(train_Data)  
dim(test_Data)

train_control = trainControl(method = "cv", 
                             number = 10,
                             verbose = T)

model_knn = train(Balance ~ ., 
                  data = train_Data, 
                  method = "knn", 
                  trControl = train_control,
                  tuneLength = 60,
                  metric = "RMSE")

ggplot(model_knn, metric = "RMSE")
model_knn
names(model_knn)
model_knn$results
model_knn$bestTune
# train RMSE

model_knn$bestTune
model_predict_train = predict(model_knn, newdata = train_Data)
sqrt(mean(abs(model_predict_train - train_Data$Balance)^2))

# test RMSE

model_predict_test = predict(model_knn, newdata = test_Data)
sqrt(mean(abs(model_predict_test - test_Data$Balance)^2))

# importance

importance = varImp(model_knn)
importance
ggplot(importance)
plot(importance)

# plots

fin_data = data.frame(x = 1:length(model_predict_train), 
                      y1 = model_predict_train, 
                      y2 = train_Data$Balance)
ggplot(data = fin_data) + geom_line(aes(x,y1), color = "red") +  geom_line(aes(x,y2))+
  coord_cartesian(xlim = c(0,320))
qplot(fin_data$x, abs(fin_data$y1 - fin_data$y2)/mean(abs(train_Data$Balance)), geom = c("line"))



