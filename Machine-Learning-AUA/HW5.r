library(knitr)
library(dplyr)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(GGally)
library(ggplot2)
library(caret)
library(glmnet)
library(boot)
library(MASS)
library(plotROC)
library(ROCR)
library(e1071)
library(rpart.plot)

rm(list = ls())

german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")


german_credit$response = as.factor(german_credit$response)
set.seed(1010101)
train_ind <- createDataPartition(german_credit$response, p = 0.8, list=FALSE)



train_Data <- german_credit[train_ind,]
test_Data <- german_credit[-train_ind,]



#ex 1 Tree
mygrid = expand.grid(cp = seq(0, 0.4, by = 0.01))

train_control = trainControl(method = "cv",
                             number = 10)

tree = train(response ~ ., 
                   data = train_Data, 
                   method = "rpart",
                   tuneLength = 30,
                   trControl = train_control,
                   tuneGrid = mygrid)

#tree
rpart.plot(tree$finalModel)
prp(tree$finalModel, digits = 4, roundint = F)

#conf matrix test
#Accuracy : 0.725 
#Pos Pred Value : 0.5806  
tree_raw = predict(tree, test_Data, type = "raw")
tree_prob = predict(tree, test_Data, type = "prob")

conf_matrix = confusionMatrix(tree_raw, test_Data$response, positive = '2')
conf_matrix 

#conf matrix train
tree_raw_train = predict(tree, train_Data, type = "raw")
tree_prob_train = predict(tree, train_Data, type = "prob")

conf_matrix_train = confusionMatrix(tree_raw_train, train_Data$response, positive = '2')
conf_matrix_train 

#roc curve test
roc_cruve_df = data.frame(response = test_Data$response, prob = tree_prob[,2] )

tt = ggplot(roc_cruve_df, aes(m = prob, d = response)) + geom_roc() + 
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC Curve")

tt
#AUC 0.715
calc_auc(tt)

#roc curve train
roc_cruve_df_train = data.frame(response = train_Data$response, prob = tree_prob[,2] )

tt_train = ggplot(roc_cruve_df_train, aes(m = prob, d = response)) + geom_roc() + 
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC Curve")

tt_train
calc_auc(tt_train)

#ex2 Random Forest

random_forest = train(response ~ ., 
                 data = train_Data, 
                 method = "rf",
                 ntree = 50 )

#Confusion Matrix test
#Accuracy : 0.74
#Pos Pred Value : 0.5909
random_forest_raw = predict(random_forest, test_Data, type = "raw")
random_forest_prob = predict(random_forest, test_Data, type = "prob")

conf_matrix = confusionMatrix(random_forest_raw, test_Data$response, positive = '2')
conf_matrix 

#Confusion Matrix train
random_forest_raw_train = predict(random_forest, train_Data, type = "raw")
random_forest_prob_train = predict(random_forest, train_Data, type = "prob")

conf_matrix_train = confusionMatrix(random_forest_raw_train, train_Data$response, positive = '2')
conf_matrix_train 

#roc curve test
roc_cruve_df = data.frame(response = test_Data$response, prob = random_forest_prob[,2] )

tt = ggplot(roc_cruve_df, aes(m = prob, d = response)) + geom_roc() + 
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC Curve")

tt
#AUC: 0.8001786
calc_auc(tt)

#roc curve Train
roc_cruve_df_train = data.frame(response = train_Data$response, 
                          prob = random_forest_prob[,2] )

tt_train = ggplot(roc_cruve_df_train, aes(m = prob, d = response)) + geom_roc() + 
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC Curve")
tt_train
calc_auc(tt_train)

#ex3 Boosting

boosting <- train(response ~ ., 
                        data = train_Data,
                        method = 'adaboost',
                        tuneGrid=expand.grid(nIter=10, method='adaboost'))

#Confusion Matrix test
#Accuracy : 0.705
#Pos Pred Value : 0.5094
boosting_raw = predict(boosting, test_Data, type = "raw")
boosting_prob = predict(boosting, test_Data, type = "prob")

conf_matrix = confusionMatrix(boosting_raw, test_Data$response, positive = '2')
conf_matrix 

##Confusion Matrix train
boosting_raw_train = predict(boosting, train_Data, type = "raw")
boosting_prob_train = predict(boosting, train_Data, type = "prob")

conf_matrix_train = confusionMatrix(boosting_raw_train, train_Data$response, positive = '2')
conf_matrix_train 

#roc curve test
roc_cruve_df = data.frame(response = test_Data$response, 
                          prob = boosting_prob[,2] )

tt = ggplot(roc_cruve_df, aes(m = prob, d = response)) + geom_roc() + 
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC Curve")

tt
#AUC: 0.5595833
calc_auc(tt)

#roc curve train
roc_cruve_df_train = data.frame(response = train_Data$response, 
                          prob = boosting_prob[,2] )

tt_train = ggplot(roc_cruve_df_train, aes(m = prob, d = response)) + geom_roc() + 
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC Curve")

tt_train  
calc_auc(train)

#ex 4

#Tree
#Accuracy : 0.725 
#Pos Pred Value : 0.5806  

#Random Forests
#Accuracy : 0.74
#Pos Pred Value : 0.5909

#Boosting
#Accuracy : 0.705
#Pos Pred Value : 0.5094

#Looking at the values of Accuracy and Pos Pred Value of the three mathods,
#we can see that the best model is Radom Forests
