rm(list = ls())

# you will know here:
# + kmeans
# + filter
# + select
# + with
# + apply
# - unique
# - sort
# - which.min

# Exercise 1: K-Means in R: Simulated Data

set.seed(2)
x = matrix(rnorm(50*2), ncol=2); x
plot(x, pch=16)

x[1:25, 1] <- x[1:25, 1] + 2
x[1:25, 2] <- x[1:25, 2] - 2

plot(x, pch=16)

km.out <- kmeans(x, centers = 2, nstart = 1)
km.out$cluster
names(km.out)
km.out$totss # sum((x[,1] - mean(x[,1])) ** 2 ) + sum((x[,2] - mean(x[,2])) ** 2 )
km.out$tot.withinss
km.out$centers
plot(x, col=km.out$cluster+1, pch=16)
points(km.out$centers, col=2:3, pch=3)

set.seed(4)
km.out <- kmeans(x, centers = 3, nstart = 20)
plot(x, col=km.out$cluster+1, pch=16)
km.out$tot.withinss

km.out <- kmeans(x, centers = 4, nstart = 20)
plot(x, col=km.out$cluster+1, pch=16)
km.out$tot.withinss

## 3 clusters
x <- matrix(rnorm(150), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 2
x[1:25, 2] <- x[1:25, 2] - 2
x[50 + 1:25, 1] <- x[50 + 1:25, 1] + 2
x[50 + 1:25, 1] <- x[50 + 1:25, 1] + 2

km.out <- kmeans(x, 3, nstart = 20)
plot(x, col=km.out$cluster+1, pch=16)
points(km.out$centers, col=2:4, pch=3)

# Exercise 2: Clustering city locations
library(dplyr)
library(mdsr)

names(WorldCities)
dim(WorldCities)

BigCities <- filter(WorldCities, population >= 10**6)
names(BigCities)
BigCities <- select(BigCities, longitude, latitude)
names(BigCities)

set.seed(15)
city.km <- kmeans(BigCities, centers = 6)
with(BigCities, plot(longitude, latitude,
                     col=city.km$cluster, pch=16, cex=0.6))

# 2 Mathematical Exercises
## Exercise 3

### a
x = matrix(c(1, 4,  1, 3,  0, 4,  5, 1,  6, 2,  4, 0), ncol = 2, byrow = TRUE); x
ini.clust = rep(c(1, 2), 3); ini.clust
plot(x, col=ini.clust+2, pch=16)

### b: 
compute_centroids <- function(vector, clusters){
  all_unique_clusters = unique(sort(clusters))
  n = length(all_unique_clusters)
  res = matrix(rep(c(0, 0), n), ncol=2)
  for(i in 1:n){
    center <- apply(vector[clusters == all_unique_clusters[i],], 2, mean)
    res[i,] = center
  }
  return(res)
}

new_centroids = compute_centroids(x, ini.clust); new_clusters

### c
reassign_clussters <- function(vector, centroids){
  number_of_clusters = dim(centroids)[1]
  number_of_points = dim(vector)[1]
  
  new_clusters = rep(0, number_of_points)
  local_distances = rep(0, number_of_clusters)
  
  for(point in 1:number_of_points){
    for(k_centroid in 1:number_of_clusters){
      local_distances[k_centroid] = sum((vector[point,] - centroids[k_centroid,])**2)
    }
    new_clusters[point] = which.min(local_distances)
  }
  return(new_clusters)
}

ini.clust
new.clust = reassign_clussters(x, new_centroids); new.clust
plot(x, col=new.clust+2, pch=16)

### d
previous_cl = ini.clust
next_cl = rep(0, length(ini.clust))
plot(x, col=ini.clust, pch=16)
while(all(next_cl != previous_cl)){
  next_cl = previous_cl
  
  
  new_centroids = compute_centroids(x, next_cl)
  previous_cl = next_cl
  next_cl = reassign_clussters(x, new_centroids)
  
  Sys.sleep(2) #pauses for two seconds
  plot(x, col=next_cl, pch=16)
}

### e
plot(x, col=next_cl, pch=16)





