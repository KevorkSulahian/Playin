dimnames(USArrests)
data("USArrests")
apply(USArrests,2,mean)
apply(USArrests,2,var)

pca.out = prcomp(USArrests,scale = T)
pca.out 
names(pca.out)
biplot(pca.out,scale=0, cex=.7)

# k-means
set.seed(101)
x= matrix(rnorm(100*2),100,2)
xmean = matrix(rnorm(8,sd=4),4,2)
which = sample(1:4,100, replace = T)
x=x+xmean[which,]
plot(x,col=which, pch=19)

km.out =kmeans(x,4,nstart = 15)
km.out
plot(x,col=km.out$cluster, pch=1, lwd=2, cex=2)
points(x,col=which,pch=19)
points(x,col=c(4,3,2,1)[which],pch=19)


# hierchal learning

hc.complete= hclust(dist(x), method = 'complete')
plot(hc.complete)
hc.single= hclust(dist(x), method = 'single')
plot(hc.single)
hc.average= hclust(dist(x), method = 'average')
plot(hc.average)

hc.cut = cutree(hc.complete,4)
table(hc.cut, which)
table(hc.cut, km.out$cluster)

plot(hc.complete,labels=which)
