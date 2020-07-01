# K-Means Clustering

# 1 K-mans in R
## Exercise 1 K-Means in R: Simulated Data
set.seed(2)
x = matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 2
x[1:25, 2] <- x[1:25, 2] - 2
plot(x, pch=16)

km.out <- kmeans(x, centers = 2, nstart = 1)
km.out$cluster
names(km.out)
km.out$totss
km.out$tot.withinss
km.out$withinss
km.out$centers

plot(x, col=km.out$cluster+1, pch=16)
points(km.out$centers, col=2:3, pch=3)

km.out <- kmeans(x, centers = 2, nstart = 20)
plot(x, col=km.out$cluster+1, pch=16)
points(km.out$centers, col=2:3, pch=3)
km.out$tot.withinss

set.seed(4)
km.out <- kmeans(x, centers = 3, nstart = 20)
plot(x, col=km.out$cluster+1, pch=16)

km.out <- kmeans(x, centers = 4, nstart = 20)
plot(x, col=km.out$cluster+1, pch=16)
km.out$tot.withinss

# now data has 3 clusters
x <- matrix(rnorm(50*3), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 2
x[1:25, 2] <- x[1:25, 2] - 2
x[50+1:25,1] <- x[50+1:25,1] + 2
x[50+1:25,2] <- x[50+1:25,2] - 2
km.out <- kmeans(x, 3, nstart = 20)
plot(x, col=km.out$cluster+1, pch=16)
points(km.out$centers, col=2:4, pch=3)

km.out$cluster

## Exercise 2 Clustering City Locations
install.packages("dplyr")
library(dplyr)

install.packages("mdsr")
library(mdsr)

names(WorldCities)
BigCities <- filter(WorldCities, population >= 100000)
BigCities <- select(BigCities, longitude, latitude)
dim(BigCities)

set.seed(15)
city.km <- kmeans(BigCities, centers = 6)
with(BigCities, plot(longitude, latitude, 
                     col=city.km$cluster,
                     pch=16, cex=0.6))

# 2 Mathematical Exercises
## Exercise 3

df = data.frame("Obs." = 1:6,
                'X1'   = c(1, 1, 0, 5, 6, 4),
                'X2'   = c(4, 3, 4, 1, 2, 0),
                "Init" = c(1, 2, 1, 2, 1, 2))

plot(df[2:3], col=df$Init + 1, pch=16)

# computinig the centroid for each cluster

update_centers <-function(df_){
  n = dim(df_)[1]
  
  center_X1_1 = sum(df_$X1[df_$Init == 1])/n
  center_X1_2 = sum(df_$X1[df_$Init == 2])/n
  
  center_X2_1 = sum(df_$X2[df_$Init == 1])/n
  center_X2_2 = sum(df_$X2[df_$Init == 2])/n
  
  first  = c(center_X1_1, center_X1_2); first
  second = c(center_X2_1, center_X2_2); second
  
  return(rbind(first, second))
}

update_clusters <- function(df_, centers){
  n = dim(df_)[1]
  for(i in 1:n){
    first_dist = (df_[i,'X1'] - centers[1,1])^2 +
                 (df_[i,'X2'] - centers[1,2])^2
    
    second_dist = (df_[i,'X1'] - centers[2,1])^2 +
                  (df_[i,'X2'] - centers[2,2])^2
    
    if (first_dist <= second_dist){
      df_[i,'Init'] = 1
    }else{
      df_[i,'Init'] = 2
    }
  }
}


foo = update_centers(df); foo
update_clusters(df, foo); df

plot(df[2:3], col=df$Init + 1, pch=16)
