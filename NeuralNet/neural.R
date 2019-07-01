library(datasets)
names(infert)
summary(infert)
head(infert,5)
# View(infert)
library(neuralnet)

data("infert")
infert$education = NULL

nn <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden = c(2), err.fct = 'ce', linear.output = F)

nn

nn$result.matrix

out <- cbind(nn$covariate,nn$net.result[[1]])
dimnames(out) <- list(NULL, c("age", "parity","induced","spontaneous","nn-output"))
head(nn$generalized.weights[[1]])

plot(nn)
