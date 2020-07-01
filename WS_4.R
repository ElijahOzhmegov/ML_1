# ----- K-Nearest Neighbour Regression -----
rm(list = ls())
#install.packages('FNN')
library(rgl)
library(FNN)

# ----- here you will learn -----
# knn.reg
# expand.grid()
# persp()
# grey()
# trans3d()
# with()

# ----- 1 One explanatory variable -----
x <- 1:20
y <- x + rnorm(length(x), mean = 10)
xgrid <- data.frame(x)
plot(x, y, type = 'b')

k.knn = 1
knnr.out1 <- knn.reg(x, y=y, k=k.knn, test = xgrid)
round(cbind(x, y, fitted.1=knnr.out1$pred), 2)

k.knn = 3
knnr.out1 <- knn.reg(x, y=y, k=k.knn, test = xgrid)
round(cbind(x, y, fitted.2<-knnr.out1$pred), 2)
points(x, fitted.2, col='red', type = 'b')

### let's make out grid much thiner
xgrid    <- data.frame(seq(0, 21, 0.05))
knnr.out <- knn.reg(x, y=y, k=5, test = xgrid)
plot(x, y)
lines(xgrid[[1]], knnr.out$pred)

lm.out <- lm(y ~ x)
summary(lm.out)
plot(x, y)
abline(lm.out, col=2)
knnr.out <- knn.reg(x, test=xgrid, y=y, k=19)
lines(xgrid[,1], knnr.out$pred, col=3)

print("y = 0.94*x + 10.55")

# ----- 2 Two explanatory variables -----
fitdata<-as.data.frame(matrix(c(1,87, 42,6, 73, 43,7, 66, 44,15,62,54,
                                12, 68,45,4,92,46,12,60,50,13,70,46,14,
                                71,54,10,64,47),byrow=T,ncol=3))
summary(fitdata)
str(fitdata)
names(fitdata) <- c("fitness","weight","lungvol")
str(fitdata)

lm.fitness <- lm(fitness ~ weight + lungvol, data=fitdata)
summary(lm.fitness)

print("fitness = -0.2324*weight + 0.5893*lungvol - 1.7860")

# let's make some predictions 
m1 <- seq(55, 95, length=20)
m2 <- seq(40, 55, length=20)
Xgrid     <- expand.grid(weight=m1, lungvol=m2)
pred.grid <- predict(lm.fitness, newdata = Xgrid)
tt <- cbind(Xgrid, pred.grid)

res <- persp(m1, m2, matrix(pred.grid, nrow = length(m1)),
             border = grey(0.6), 
             xlab = "Weight", ylab = "Lung Volume", zlab = "Fitness",
             theta=0, phi=15)
points(trans3d(fitdata$weight, fitdata$lungvol, fitdata$fitness, pmat = res),
       pch=16, col=(2:3)[1.5+.5*sign(lm.fitness$residuals)])

with(tt, plot3d(lungvol, weight, pred.grid, 
                col="grey80", size=3, zlab='fitness'))
with(fitdata, plot3d(lungvol, weight, fitness, 
                     add=T, size=2, col='red', type = 's'))

# now let's look at KNN method
draw_KNN <- function(k.number){
  X        <- fitdata[, c('weight', 'lungvol')]
  knnr.out <- knn.reg(X,y=fitdata$fitness, k=k.number, test = Xgrid)
  
  res      <- persp(m1, m2, matrix(knnr.out$pred, nrow=length(m1)), 
                    border = grey(0.6), 
                    xlab = "Weight", ylab = "Lungvol", zlab = "fitness",
                    theta = 15, phi = 15)
  points(trans3d(fitdata$weight, fitdata$lungvol, fitdata$fitness, pmat = res),
         pch=16, col=c("DarkRed"[1.5 + .5*sign(lm.fitness$residuals)]))
  
  with(tt, 
       plot3d(lungvol, weight, knnr.out$pred,
              col = "grey80", size = 3, zlab = "fitness"))
  with(fitdata, 
       plot3d(lungvol, weight, fitness, 
              add = T, size = 2, col = "red", type = 's'))
}
draw_KNN(1)
draw_KNN(2)
draw_KNN(5)
draw_KNN(7)

# ----- 3 Mean squared error and the bias-variance trade off -----



















