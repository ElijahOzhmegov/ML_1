# Workshop 7: Classification: Logistic Regression, Specificity, ----------------
# Sensitivity and Roc ----------------------------------------------------------

rm(list = ls())
library(corrplot) 
library(pROC) 

# ----- here you will learn -----
# glm()
# table()
# corrplot
# roc()
# auc()
# ggroc()

# Exercise 1 Classification in R using logistic regression ---------------------
# AKA: lab 4.6.1 and 4.6.2 
{ # lab 4.6.1
  library(ISLR)
  names(Smarket)
  dim(Smarket)
  summary(Smarket)
  
  cor(Smarket[, -9])
  corrplot(cor(Smarket[, -9]))
  plot(Smarket$Volume)
}

{ # lab 4.6.2 
  glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
                  data = Smarket, family = binomial)
  summary(glm.fits)
  
  coef(glm.fits)
  summary(glm.fits)$coef
  
  glm.probs <- predict(glm.fits, type = "response")
  glm.probs[1:10]
  contrasts(Smarket$Direction)
  
  glm.pred <- rep('Down', 1250)
  glm.pred[glm.probs>.5] = 'Up'
  
  table(glm.pred, Smarket$Direction)
  glm.sensitivity = (507+145)/1250; glm.sensitivity
  mean(glm.pred==Smarket$Direction)
  
  train=(Smarket$Year<2005)
  Smarket.2005=Smarket[!train,]
  dim(Smarket.2005)
  Direction.2005=Smarket$Direction[!train]
  
  glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
                  data = Smarket, family = binomial, subset = train)
  glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
  
  glm.pred=rep('Down', 252)
  glm.pred[glm.probs>.5] = 'Up'
  table(glm.pred, Direction.2005)
  mean(glm.pred==Direction.2005)
  
  glm.fits=glm(Direction~Lag1+Lag2,data=Smarket, family = binomial,
               subset = train)
  glm.probs=predict(glm.fits, Smarket.2005, type="response")
  glm.pred=rep("Down", 252)
  glm.pred[glm.probs>.5] = "Up"
  table(glm.pred, Direction.2005)
  mean(glm.pred==Direction.2005)
  
  
  predict(glm.fits, newdata = data.frame(Lag1=c(1.2, 1.5),
                                         Lag2=c(1.1, -.8)),
          type = "response")
}

# Exercise 2: Classifier for Diabetes ------------------------------------------
load("Diabetes.rda")
str(Diabetes)

{ # a. how many observation are there?
  dim(Diabetes)[1]
}
{ # b. the frequency table for diabetes status 
  table(Diabetes$YN)
  prop.table(table(Diabetes$YN))
}
{ # c. mean and sd of BIM and Age
  apply(Diabetes[-1], 2, mean)
  apply(Diabetes[-1], 2, sd)
}
{ # d. plot a hist and scatter plot of BMI/Age
  library(ggplot2)
  ggplot(Diabetes,aes(x=BMI,group=YN,fill=YN))+
    geom_histogram(position="dodge",binwidth=1)+theme_bw()
  
  ggplot(Diabetes,aes(x=Age,group=YN,fill=YN))+
    geom_histogram(position="dodge",binwidth=1)+theme_bw()
  
  ggplot(Diabetes, aes(x=Age,y=BMI)) + geom_line()
}
{ # e. create a box plot of BMI/Age against YN
  ggplot(Diabetes, aes(y=BMI,x=YN)) + geom_boxplot()
  ggplot(Diabetes, aes(y=Age,x=YN)) + geom_boxplot()
}

{
  #install.packages("pROC")
  require(pROC)
  
  ##load the data file
  # load("Diabetes.rda")
  
  # Split the data into a training and test data set, with 2000 observations in 
  # the test data set.
  set.seed(50)
  n<-dim(Diabetes)[1]
  
  testidx<-sample(n,2000)
  test   <-Diabetes[testidx,]
  train  <-Diabetes[-testidx,]
  
  table(as.numeric(train$YN))
  # Fit the logistic regression model and look at the model summary.
  glm.obj<-glm(YN~BMI,data=train,family="binomial")
  summary(glm.obj)
  BMI.grid<-10:82
  glm.pred<-predict(glm.obj,newdata=data.frame(BMI=BMI.grid),type="response")
  plot(BMI.grid,glm.pred,type="l")
  
  # Define ``High Risk of Diabetes'' using a cut off of alpha=0.5. Obtain the 
  # classification matrix
  alpha<-0.5 
  fit1<-fitted(glm.obj)
  HiRisk<-fit1>alpha
  table(HiRisk)
  table(train$YN,HiRisk)
  
  prop.table(table(train$YN,HiRisk),1)
  #Which is the sensitivity and which is the specificity for alpha=0.5 
  prop.table(table(train$YN,HiRisk),2)
  sensitivity.di = prop.table(table(train$YN,HiRisk),2)[4]; sensitivity.di
  specificity.di = prop.table(table(train$YN,HiRisk),2)[1]; specificity.di
  
  
  ##plot the ROC curve and obtain the AUC
  require(pROC)
  roc.obj1 <- roc(train$YN,fit1)
  str(roc.obj1)
  ggroc(roc.obj1)
  auc(roc.obj1)
  
  
  # roc produces a vector of thresholds (alpha), specificiteis and sensitivites.
  # find the index of the threshold nearest to alpha=0.5
  idx<-which.min(abs(roc.obj1$thresholds-alpha))
  idx
  
  roc.obj1$thresholds[idx]
  roc.obj1$sensitivities[idx]
  roc.obj1$specificities[idx]
  # do these values concur (fit) with your answers above?
  
  ptest<- predict(glm.obj, newdata=test, type="response")
  test.roc.obj1 <- roc(test$YN,ptest)
  ggroc(list(train=roc.obj1,test=test.roc.obj1))
  auc(test.roc.obj1)
  #the training and test results are similar
  
  ###Repeat for model with just Age
  glm.obj2<-glm(YN~Age,data=train,family=binomial)
  ptest2<- predict(glm.obj2, newdata=test, type="response")
  roc.obj2 <- roc(train$YN, fitted(glm.obj2))
  ggroc(list("BMI"=roc.obj1,"Age"=roc.obj2))
  auc(roc.obj2)
  
  
  
  
  ###Repeat for model with BMI and Age
  glm.obj3<-glm(YN~Age+BMI,data=train,family=binomial)
  
  #Add further commands here
  roc.obj3 <- roc(train$YN, fitted(glm.obj3))
  ggroc(list("BMI"=roc.obj1,"Age"=roc.obj2, "BMI+Age"=roc.obj3))
  auc(roc.obj3)
  # Find the index for roc.obj3 which gives closest sensitivity to 0.5
  # Obtain the corresponding value of alpha and specificity.
  
  {
    # Define ``High Risk of Diabetes'' using a cut off of alpha=0.5. Obtain the 
    # classification matrix
    alpha<-0.5 
    fit3<-fitted(glm.obj3)
    HiRisk<-fit3>alpha
    table(HiRisk)
    table(train$YN,HiRisk)
    
    prop.table(table(train$YN,HiRisk),1)
    #Which is the sensitivity and which is the specificity for alpha=0.5 
    prop.table(table(train$YN,HiRisk),2)
    sensitivity.di = prop.table(table(train$YN,HiRisk),2)[4]; sensitivity.di
    specificity.di = prop.table(table(train$YN,HiRisk),2)[1]; specificity.di
  }
  
  
  
}









