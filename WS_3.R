rm(list=ls())

# you will know here:
# + scale
# + pam
# + clusplot
# + rainbow
# + unique
# + as.numeric
# + as.factor
# + par
# + hclust
# + cutree

# Robust clustering with PAM, Hierarchical Clustering
find.package("cluster")
library("cluster")

find.package("ISLR")
#install.packages("ISLR")
library("ISLR")

# Exercise 1: PAM Clustering
row.names(USArrests) <- state.abb
pairs(USArrests)

clustmat <- scale(USArrests)
pam.out <- pam(clustmat, k=4)
clusplot(pam.out, labels=3, col.p=pam.out$clustering)

sp <- silhouette(pam.out)
str(sp, max.level = 1)
str(sp, max.level = 2)
sp[,'sil_width']
plot(sp, col=1:4)
mean(sp[,'sil_width'])
abline(v=mean(sp[,'sil_width']))

avesw.vec <- rep(NA, 7)
for(i in 2:7){
  avesw.vec[i] <- mean( silhouette(pam(USArrests, k=i))[,'sil_width'] )
}
plot(1:7, avesw.vec, type = 'b', ylim = c(0, 0.6))
#answer:
max(avesw.vec)
which.max(avesw.vec)
  
pam.out <- pam(USArrests, k=2)
clusplot(pam.out, labels=3, col.p=pam.out$clustering)
mean(sp[,'sil_width'])
plot(sp, col = 1:2)  
abline(v=mean(sp[,'sil_width']))

km.out = kmeans(USArrests, centers=2, nstart = 20)
table(km.out$cluster, pam.out$clustering)  

# Exercise 2: Silhouette plot for K-means  
n.clusters = 2
km.out = kmeans(USArrests, centers=n.clusters, nstart = 20)
sp <- silhouette(km.out$cluster, dist(USArrests))  
plot(sp, col = 1:n.clusters)  

# Exercise 3: Hierarchical Clustering  
library(ISLR)
nci.labs <- NCI60$labs; nci.labs
nci.data <- NCI60$data
str(nci.data, max.level = 2)
table(nci.labs)

## PCA
pr.out <- prcomp(nci.data, scale. = TRUE)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(cols))])
}

par(mfrow=c(1, 2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), 
     pch=19, xlab = 'Z1', ylab = 'Z2')
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), 
     pch=19, xlab = 'Z1', ylab = 'Z3')

summary(pr.out)
plot(pr.out)

# pve
pve <- pr.out$sdev ** 2
pve <- 100 * pve/sum(pve)

par(mfrow=c(1, 2))
plot(pve, type = 'o', ylab = 'PVE', xlab = 'PC', col='blue')  
plot(cumsum(pve), type = 'o', 
     ylab = 'cumulative PVE', xlab = 'PC', col='brown3')  

## Hierarchically
sd.data <- scale(nci.data)

data.dist = dist(sd.data)

par(mfrow=c(3,1))
plot(hclust(data.dist), labels=nci.labs, 
     main = 'Complete Linkage', xlab = '', sub = '', ylab = '')
plot(hclust(data.dist, method='average'), labels=nci.labs, 
     main = 'Average Linkage', xlab = '', sub = '', ylab = '')
plot(hclust(data.dist, method='single'), labels=nci.labs, 
     main = 'Single Linkage', xlab = '', sub = '', ylab = '')

# let's cut dendogram 
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow=c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h=139, col='red')

hc.out

# kmeans comparison
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

# PCA comparison
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs, 
     main = 'Hier. Clust on First Five Score Vectors')
table(cutree(hc.out, 4), nci.labs)

# Exercise 4: Hierarchical Clustering
df <- data.frame(
  X1 <- c(4, 5, 6, 10, 14, 15),
  X2 <- c(2, 5, 11, 2, 7, 9)
)
# a
df.dist <- round(dist(df), 1); df.dist

# b

# 5-6 (2.2), 1-2(3.2)
## 1-2-4(6.0), 5-6-3(9.2)
#### {1-2-4}-{5-6-3}(13.0)










