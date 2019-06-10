#In the k-means based outlier detection technique the data are partitioned in to k groups 
#by assigning them to the closest cluster centers.

#Once assigned we can compute the distance or dissimilarity between each object and its cluster center 
# and pick those with largest distances as outliers.

data("iris")

iris2 <- iris[,1:4]
iris2

kmeans.result <- kmeans(iris2, centers=3)
kmeans.result$centers

kmeans.result$cluster

centers <- kmeans.result$centers[kmeans.result$cluster, ] 
# "centers" is a data frame of 3 centers 
#but the length of iris dataset so we can canlculate distance difference easily.

centers

distances <- sqrt(rowSums((iris2 - centers)^2))

outliers <- order(distances, decreasing=T)[1:5]


print(outliers) # these rows are 5 top outliers

print(iris2[outliers,])

plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch=19, col=kmeans.result$cluster, cex=1)

points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=15, cex=2)

points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=3)
