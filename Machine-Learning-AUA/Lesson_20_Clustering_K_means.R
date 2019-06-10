
rm(list=ls())


# simulated example

set.seed(2)
x = matrix(rnorm(50*2), ncol =2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4
dev.new()
plot(x)
# k_means clustering with K=2


# sntart = 20 is showing how many random sets should be chosen
# We strongly recommend always running K-means clustering with a large value of nstart, such as 20 or 50
K = 2
km.out = kmeans(x, K, nstart = 20)
km.out

# The cluster assignments of the 50 observations are contained in
# km.out$cluster

km.out$cluster
dev.new()
plot(x, col = (km.out$cluster), main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =20, cex =2)


# for real data we don't know the number of clusters. So let us try with K=3

set.seed(4)
K = 3
km.out = kmeans(x, K, nstart = 20)
km.out
dev.new()
plot(x, col = (km.out$cluster + 1), main="K-Means Clustering Results with K=3", xlab ="", ylab="", pch =20, cex =2)


