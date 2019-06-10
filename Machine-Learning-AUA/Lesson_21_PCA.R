
rm(list=ls())

head(iris, 13)


# log transform 
log.ir = log(iris[, 1:4])
ir.species = iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca = prcomp(log.ir, center = TRUE, scale. = TRUE) 
ir.pca

summary(ir.pca)


# Predict PCs
# Just for illustration pretend the last two rows of the iris data 
# has just arrived and we want to see what is their PCs values:
predict(ir.pca, newdata = tail(log.ir, 2))

