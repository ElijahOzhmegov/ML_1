#R-code to investigate the bias variance trade off
rm(list = ls())
#a) Re-run the $K$ nearest neighbour regression model from Section 1 with k01.
x     <- 1:20
truey <- x+10
obsy  <- truey+rnorm(length(x))
xgrid <- data.frame(x)

knnr.out<-knn.reg(x,y=obsy,k=1,test=xgrid)

#b) Estimated MSE for this model
plot(x,obsy) #Data points as circles
points(xgrid$x,knnr.out$pred,pch=3) #fitted values as crosses
mean((obsy-knnr.out$pred)^2) #is the model MSE



#mse-true 
mean((truey-knnr.out$pred)^2)

#c) Repeat 10 times 
plot(x,truey,ylim=c(8,33)) 
for(i in 1:10){
  yinloop  <- truey + rnorm(length(x))
  knnr.out <- knn.reg(x,y=yinloop,k=1,test=xgrid)
  points(xgrid$x,knnr.out$pred,pch=3) #fitted values as crosses
}
#the estimate have a high variance

#d) Repeat 50 times and store the errors in a matrix
error.mat<-matrix(NA,20,50)
plot(x,truey,ylim=c(8,33), type = 'b', pch=19) 
for(i in 1:50){
  yinloop       <- truey + rnorm(length(x))
  knnr.out      <- knn.reg(x,y=yinloop,k=3,test=xgrid)
  error.mat[,i] <- knnr.out$pred-truey
  points(xgrid$x,knnr.out$pred,pch=3) #fitted values as crosses
}
#estimated bias
apply(error.mat,1,mean)
#squared bias over all points
sum((apply(error.mat,1,mean))^2)
#variance
sum((apply(error.mat,1,var)))

#Repeat the last part increasing k from 1 to 18 

#now we will store the squared bias and variance for k from 1 to 19 
sqbk <- vark<-rep(NA, 19)
for(k in 1:19){
  error.mat<-matrix(NA, 20, 50)
  for(i in 1:50){
    yinloop       <- truey + rnorm(length(x))
    knnr.out      <- knn.reg(x, y=yinloop, k=k, test=xgrid)
    error.mat[,i] <- knnr.out$pred - truey
  }
  #squared bias over all points
  sqbk[k] <- sum((apply(error.mat, 1, mean))^2)
  vark[k] <- sum((apply(error.mat, 1, var)))
}
#plot the squared bias against k
plot(sqbk,type="b")
#plot the variance against k
plot(vark,type="b")
#plot the sum of the two 
plot(sqbk+vark,type="b")
#zoom in to find a minimum
plot(sqbk[1:6]+vark[1:6],type="b")



