german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

german_credit$response
german_credit$response = german_credit$response - 1
german_credit$response <- as.factor(german_credit$response)

# 1-5 part a
library(caret)
library(MASS)
set.seed(1)
trainIndex <- createDataPartition(german_credit$response, p = .8, list = F)

train <- german_credit[trainIndex,]
test <- german_credit[-trainIndex,]

lda.fit <- lda(response~., data = train)
# lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit,test)

lda_class <- lda.pred$class
table(lda_class,test$response)
mean(lda_class==test$response)


qda_fit <- qda(response~., data = train)

qda_pred <- predict(qda_fit,test)
qda_class <-predict(qda_fit,test)$class
table(qda_class,test$response)
mean(qda_class==test$response)

library(ROCR)


library(e1071)
model <- naiveBayes(response~.,data=train,laplace = 1)
names(model)
model$apriori

pred_test <- predict(model,newdata = test, type = 'raw')


logistic.fit <- glm(response~.,data = train, family = binomial)
logistic.pred<- predict(logistic.fit, newdata = test, type = 'response')


# knn

library(class)
set.seed(1)
ctrl <- trainControl(method = 'cv', number = 10)
knn_c <-train(response~., data = train, method='knn', trControl=ctrl, preProcess = c("center","scale"), tuneGrid = expand.grid(k=1:10))

knn_p <- predict(knn_c,newdata = test, type = 'prob') # kak ger kevo
mean(knn_p == test$response)


# 1-5 part 2

p_test <- prediction(lda.pred$posterior[,2], test$response)
perf <- performance(p_test, 'tpr','fpr')
plot(perf,colorize=T)
lda <-performance(p_test,'auc')@y.values[[1]]


p_test <- prediction(qda_pred$posterior[,2], test$response)
perf <- performance(p_test, 'tpr','fpr')
plot(perf,colorize=T)
qda <- performance(p_test,'auc')@y.values[[1]]

p_test <- prediction(pred_test[,2], test$response)
perf <- performance(p_test, 'tpr','fpr')
plot(perf,colorize=T)
bay <- performance(p_test,'auc')@y.values[[1]]

p_test <- prediction(logistic.pred, test$response)
perf <- performance(p_test, 'tpr','fpr')
plot(perf,colorize=T)
logistic <- performance(p_test,'auc')@y.values[[1]]


p_test <- prediction(knn_p[,2], test$response )
perf <- performance(p_test, 'tpr','fpr')
plot(perf,colorize=T)
knn <- performance(p_test,'auc')@y.values[[1]]



#6
which.max(c(lda,qda,bay,logistic,knn))

# 1 is lda



#7
table(lda_class,test$response)

# all are calculated in the line below
confusionMatrix(lda_class, test$response)


