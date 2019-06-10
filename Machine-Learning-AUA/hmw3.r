#homework 3
library(caret)

X <- rnorm(100)
e <- rnorm(100)

Y <- 3 + 1*X + 4*X^2 - 1*X^3 + e

df <- data.frame(Y, X)

ridge <- train(Y ~ poly(X, 10), data = df,
                     method = 'glmnet',
                     trControl = trainControl(method = 'cv', number = 10),
                     tuneGrid = expand.grid(alpha = 0,
                                            lambda = seq(0, 10, by = 0.1)))

plot(ridge)

lasso <- train(Y ~ poly(X, 10), data = df,
               method = 'glmnet',
               trControl = trainControl(method = 'cv', number = 10),
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = seq(0, 10, by = 0.1)))

plot(lasso)


postResample(predict(lasso, df), df$Y)
postResample(predict(ridge, df), df$Y)

# as it is visible the R^2 is almost the same which means they are similar
# 
