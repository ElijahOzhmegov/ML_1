rm(list = ls())

# you will know here:
# + pairs
# + prcomp
# + plot3d
# + apply
# + biplot
# + cumsum
# + seq

# + as.numeric
# + substr
# + gsub
# + colnames

# Higher Dimennsional K-Means, PCA

find.package("rgl")
install.packages("rgl", dependencies=TRUE)
library(rgl)

barplot(table(1:8), col=1:8)

palette()
colors()[1:10]

# color_blind_friendly palette (with black):
cbf <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(cbf)
barplot(table(1:8), col = 1:8)

## Exercise 1: K-means clustering with 3 dimenstions
set.seed(10)
x = matrix(rnorm(75*3), ncol = 3)
x[1:25, 1] = x[1:25, 1] + 5
x[51:75, 2] = x[51:75, 2] - 6
truth <- rep(1:3, c(25, 25, 25))
pairs(x, col=truth+1)
plot3d(x, size=5)

# c
km.out <- kmeans(x, centers=3, nstart=20)
km.out$tot.withinss

# d
table(km.out$cluster, truth)

# e 
plot3d(x, col=km.out$cluster+1, size=5)
plot3d(km.out$centers, add = TRUE, col = 2:4, type = 's')

# f
two_classes = km.out$cluster != 3; two_classes
plot3d(x[two_classes,], col = km.out$cluster[two_classes]+1, size=5)
print("Description: two presented clusters are divided by a line")

# Exercise 2: USA arrests data: PCA and clustering

# a
names(USArrests)

# b
mean_USArrests <- apply(USArrests, 2, mean); mean_USArrests
 std_USArrests <- apply(USArrests, 2, sd); std_USArrests

# c
pr.out = prcomp(USArrests, scale. = TRUE); pr.out
pr.out$scale
pr.out$sdev

sd(pr.out$x[,1])
sd(USArrests$Murder)

# d
cat("pr.out$scale is about sd of varibles of raw df, when pr.out$sdev is about sd 
    of PCA varibles")

# e
pr.var = pr.out$sdev ** 2; pr.var
pve = pr.var/sum(pr.var); pve

plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", 
     ylim=c(0,1), type = 'b')

plot(cumsum(pve), xlab="Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", ylim = c(0, 1), type='b')

# f
pr.out$rotation =- pr.out$rotation
pr.out$x =- pr.out$x

biplot(pr.out, scale = TRUE)
biplot(pr.out, xlab=state.abb)

# g
km.out <- kmeans(pr.out$x, centers = 2, nstart = 20)
plot(pr.out$x[,1:2], type = 'n')
text(pr.out$x[,1], pr.out$x[,2], labels = state.abb, col = km.out$cluster)

n_cl1 = sum(km.out$cluster == 1); n_cl1
n_cl2 = sum(km.out$cluster == 2); n_cl2

# h
wss.vec <- rep(NA, n <- 10)
for(k in 1:n){
  km.out <- kmeans(pr.out$x, centers = k, nstart = 20)
  wss.vec[k] <- km.out$tot.withinss
}

plot(wss.vec, type='b')

# i
km.out <- kmeans(pr.out$x, centers = 2, nstart = 20)
km.out$cluster
pairs(pr.out$x, col=km.out$cluster)

# j
df.km.out <- kmeans(USArrests, centers = 2, nstart = 20)
plot(USArrests[,1:2], type='n')
text(USArrests[,1], USArrests[,2], labels = state.abb, col = df.km.out$cluster)

df.n_cl1 = sum(km.out$cluster == 1); df.n_cl1
df.n_cl2 = sum(km.out$cluster == 2); df.n_cl2


df.wss.vec <- rep(NA, n<-10)
for(k in 1:n){
  km.out <- kmeans(USArrests, centers = k, nstart = 20)
  df.wss.vec[k] <- km.out$tot.withinss
}
plot(df.wss.vec, type='b')


df.km.out <- kmeans(USArrests, centers = 2, nstart = 20)
pairs(USArrests, col=df.km.out$cluster)

# ok, it's littel bit worse compared with PCA-ed data

# Exercise 3
decathlon <- read.csv2("Zehnkampf2017Hamburg.csv",
                       stringsAsFactors = FALSE)[-c(1:3, seq(10, 28, 2))]

names(decathlon)
row.names(decathlon)
dim(decathlon)

# a
## transform times to seconds
hilf <- decathlon$Zeit.400m; hilf
decathlon$Zeit.400m <- 60 * as.numeric(substr(hilf, 1, 2)) +
                       as.numeric(gsub(',', '.', substr(hilf, 4, nchar(hilf))))

hilf <- decathlon$Zeit.1500m; hilf
decathlon$Zeit.1500m <- 60 * as.numeric(substr(hilf, 1, 2)) +
                        as.numeric(gsub(',', '.', substr(hilf, 4, nchar(hilf))))

colnames(decathlon) <- c("YearOfBirth", "Class", "Points",
                         "Day1", "Day2", "Time.100m", "LongJump", 
                         "ShotPut", "HighJump", "Time.400m", "Hurdles", 
                         "Discus", "PoleVault", "JavelinThrow", "Time.1500m")
temp <- apply(decathlon, 1, function(x) sum(is.na(x))); temp
clustermat <- decathlon[temp==0, 6:15]
pairs(clustermat)

# b
clustermat
scaled_clustermat = scale(clustermat)
df.decathlon.km.out <- kmeans(scaled_clustermat, centers = 4, nstart = 20)
pairs(clustermat, col=df.decathlon.km.out$cluster)

print("BC data is not scaled???")

# c
df.decathlon.pr.out <- prcomp(clustermat, scale. = TRUE)
biplot(df.decathlon.pr.out, scale = TRUE)

print("x-axis is speed")
print("176, 31, 12, 37")

# d
plot3d(df.decathlon.pr.out$x[, 1:3], col = df.decathlon.km.out$cluster+1, size=5)

# e
df.decathlon.pr.var = df.decathlon.pr.out$sdev ** 2
pve = df.decathlon.pr.var / sum(df.decathlon.pr.var); pve
plot(cumsum(pve), type = 'b', ylim = c(0, 1))

# f (Completely not sure)
df.decathlon.km.out <- kmeans(df.decathlon.pr.out$x, centers = 2, nstart = 20)
df.decathlon.km.out <- kmeans(df.decathlon.pr.out$x[,1:3], centers = 2, nstart = 20)

biplot(df.decathlon.pr.out)
plot(df.decathlon.pr.out$x[,1:2], type = 'n')
text(df.decathlon.pr.out$x[,1], df.decathlon.pr.out$x[,2], col = df.decathlon.km.out$cluster+1)

plot3d(df.decathlon.pr.out$x[, 1:3], col = df.decathlon.km.out$cluster+1, size=5)
