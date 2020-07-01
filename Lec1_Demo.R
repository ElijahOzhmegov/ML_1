#Lecture 1 Demo
set.seed(53)
x<-matrix(rnorm(20*2),ncol=2); x

#sort the rows so that they are ascending in x[,1]
#not necessary but better for the animated graphics
x<-x[order(x[,1]),]; x
plot(x,lwd=2)

#K=2 Two Clusters
set.seed(54)
ini.clust<-rbinom(20,1,0.5)+1
ini.clust
clust<-ini.clust
plot(x,col=clust,lwd=2)
WC1<-WC2<-0
for(i in 1:20){
  for(ii in 1:20){
    if(i==ii) next
    if(clust[i]==1 && clust[ii]==1) WC1<-WC1+sum((x[i,]-x[ii,])^2) 
    if(clust[i]==2 && clust[ii]==2) WC2<-WC2+sum((x[i,]-x[ii,])^2) 
  }
}
WC1<-WC1/sum(clust==1)
WC2<-WC2/sum(clust==2)
tot.withinss<-WC1+WC2
tot.withinss

Centr1<-apply(x[clust==1,],2,mean)
Centr2<-apply(x[clust==2,],2,mean)
WC<-0
for(i in 1:20){
  if(clust[i]==1) WC<-WC+sum((x[i,]-Centr1)^2) 
  if(clust[i]==2) WC<-WC+sum((x[i,]-Centr2)^2) 
}
WC
#WC is exactly half tot.withinss
#Plot the centroids
points(Centr1[1],Centr1[2],col=1,pch=2,lwd=2)
points(Centr2[1],Centr2[2],col=2,pch=2,lwd=2)

#reassign points to clusters
#repeat until converged
#To see the animated graphics in RStudio click on zoom
for(i in 1:20){
  clust[i] = (sum((x[i,]-Centr2)^2) <= sum((x[i,]-Centr1)^2)) + 1
  plot(x,col=clust,lwd=2)
  points(Centr1[1],Centr1[2],col=1,pch=2,lwd=2)
  points(Centr2[1],Centr2[2],col=2,pch=2,lwd=2)
  points(x[i,1],x[i,2],pch=16,col=clust[i])
  Sys.sleep(2) #pauses for two seconds
}

#fint the new centroids
Centr1<-apply(x[clust==1,],2,mean)
Centr2<-apply(x[clust==2,],2,mean)
points(Centr1[1],Centr1[2],col=1,pch=17,lwd=2)
points(Centr2[1],Centr2[2],col=2,pch=17,lwd=2)

WC<-0
for(i in 1:20){
  if(clust[i]==1) WC<-WC+sum((x[i,]-Centr1)^2) 
  if(clust[i]==2) WC<-WC+sum((x[i,]-Centr2)^2) 
}
WC

##repeat until converged


#K=3 Three clusters
set.seed(55)
ini.clust<-ceiling(runif(20)*3)
clust<-ini.clust
plot(x,col=clust,lwd=2)
Centr1<-apply(x[clust==1,],2,mean)
Centr2<-apply(x[clust==2,],2,mean)
Centr3<-apply(x[clust==3,],2,mean)
points(Centr1[1],Centr1[2],col=1,pch=2,lwd=2)
points(Centr2[1],Centr2[2],col=2,pch=2,lwd=2)
points(Centr3[1],Centr3[2],col=3,pch=2,lwd=2)
WC<-0
for(i in 1:20){
  if(clust[i]==1) WC<-WC+sum((x[i,]-Centr1)^2) 
  if(clust[i]==2) WC<-WC+sum((x[i,]-Centr2)^2) 
  if(clust[i]==3) WC<-WC+sum((x[i,]-Centr3)^2) 
}
print(WC)
better<-WC
while(better>0.00001){ #in lecture loop manually
  #reassign points to clusters
  #repeat until converged
  for(i in 1:20){
    clust[i]<-1
    dist<-sum((x[i,]-Centr1)^2)
    if(sum((x[i,]-Centr2)^2)<dist) 
    {
      dist<-sum((x[i,]-Centr2)^2)
      clust[i]<-2
    }
    if(sum((x[i,]-Centr3)^2)<dist)
    {
      dist<-sum((x[i,]-Centr3)^2)
      clust[i]<-3
    }
    plot(x,col=clust,lwd=2)
    points(Centr1[1],Centr1[2],col=1,pch=2,lwd=2)
    points(Centr2[1],Centr2[2],col=2,pch=2,lwd=2)
    points(Centr3[1],Centr3[2],col=3,pch=2,lwd=2)
  }
  
  Centr1<-apply(x[clust==1,],2,mean)
  Centr2<-apply(x[clust==2,],2,mean)
  Centr3<-apply(x[clust==3,],2,mean)
  points(Centr1[1],Centr1[2],col=1,pch=17,lwd=2)
  points(Centr2[1],Centr2[2],col=2,pch=17,lwd=2)
  points(Centr3[1],Centr3[2],col=3,pch=17,lwd=2)
  WCold<-WC
  WC<-0
  for(i in 1:20){
    if(clust[i]==1) WC<-WC+sum((x[i,]-Centr1)^2) 
    if(clust[i]==2) WC<-WC+sum((x[i,]-Centr2)^2) 
    if(clust[i]==3) WC<-WC+sum((x[i,]-Centr3)^2) 
  }
  print(WC)
  better<-abs(WC-WCold)
  Sys.sleep(4)
}

