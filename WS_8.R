# Workshop 8: Classification: Linear and quadratic discriminant analysis -------

rm(list = ls())

# Exercise 1 Bayes Classifier --------------------------------------------------

{ # a:  φ0(x), the density of X|Y =0 and for φ1(x), the density of X|Y =1.
  XY0 <- function(x) dnorm(x, mean = 4, sd=1) # phi
  XY1 <- function(x) dnorm(x, mean = 5, sd=1)
  
  PY0 <- PY1 <- 0.5
}

{ # b: Write down the expression for π1 (x) = P (Y =1|x) and simplify as much as possible.
  
  phi_0 <- function(x){
    PXY0 = XY0(x)
    PXY1 = XY1(x)
    
    PXY0 / (PXY0  + PXY1)
  }
  
  phi_1 <- function(x){
    PXY0 = XY0(x)
    PXY1 = XY1(x)
    
    PXY1 / (PXY0 + PXY1)
  }
  
  x=seq(-1, 9, 0.2)
  plot(x, XY0(x), type = 'l', col='red')
  points(x, XY1(x), type = 'l', col='green')
  
  plot(x, phi_0(x), type = 'l', col='red')
  points(x, phi_1(x), type = 'l', col='green')
  points(x=c(4.5, 4.5), y=c(0, 1), type = 'l')
}

# Exercise 2 -------------------------------------------------------------------

posterior<-function(x) dnorm(x, mean=4, sd=1)
curve(posterior, -1, 9)

{ # a:
  posterior <- function(x, pi0=0.5){
    phi_0 = dnorm(x, mean=4, sd=1)
    phi_1 = dnorm(x, mean=5, sd=1)
    
    phi_0 * pi0 / (phi_0 * pi0 + phi_1 * (1 - pi0))
  }
  curve(posterior, -1, 9, add = T)
}

{ # b
  posterior <- function(x, pi0=0.5, mu0=4, mu1=5){
    phi_0 = dnorm(x, mean=mu0, sd=1)
    phi_1 = dnorm(x, mean=mu1, sd=1)
    
    phi_0 * pi0 / (phi_0 * pi0 + phi_1 * (1 - pi0))
  }
  curve(posterior, -1, 9)
  
}

{ # c
  posterior <- function(x, pi0=0.5, mu0=4, mu1=5, s0=1, s1=1){
    phi_0 = dnorm(x, mean=mu0, sd=s0)
    phi_1 = dnorm(x, mean=mu1, sd=s1)
    
    phi_0 * pi0 / (phi_0 * pi0 + phi_1 * (1 - pi0))
  }
  curve(posterior, -1, 9)
  curve(posterior(x,pi0=0.95,mu0=11,mu1=10),6,17)
  
  curve(dnorm(x, mean = 10), 6, 17)
  curve(dnorm(x, mean = 11), 6, 17, add = T)
  
}

# Exercise 3 -------------------------------------------------------------------
library(MASS)
require(pROC)
load("Diabetes.rda")

{ # splitting into training and testing datasets
  set.seed(50)
  n<-dim(Diabetes)[1]
  
  testidx<-sample(n,2000)
  test   <-Diabetes[testidx,]
  train  <-Diabetes[-testidx,]
}

{ # a: LDA model using Age
  lda.fit1 <- lda(YN~Age,data=train)
  pred.fit1 = predict(lda.fit1, newdata = test) 
  roc.fit1 <- roc(test$YN, pred.fit1$posterior[,2])
  ggroc(roc.fit1)
  auc(roc.fit1)
}

{ # b: LDA model using BMI
  lda.fit2 <- lda(YN~BMI, data=train)
  
  prd.fit2 <- predict(lda.fit2, newdata = test) 
  roc.fit2 <- roc(test$YN, prd.fit2$posterior[,2])
  
  ggroc(roc.fit2)
  auc(roc.fit2)
  
  ggroc(list(Age=roc.fit1, BMI=roc.fit2))
}

{ # c: LDA model using BMI + Age
  lda.fit3 <- lda(YN~BMI+Age, data=train)
  
  prd.fit3 <- predict(lda.fit3, newdata = test) 
  roc.fit3 <- roc(test$YN, prd.fit3$posterior[,2])
  
  ggroc(roc.fit3)
  auc(roc.fit3)
  
  ggroc(list(Age=roc.fit1, BMI=roc.fit2, 'Age + BMI'=roc.fit3))
}

{ # d: QDA model using BMI + Age
  lda.fit4 <- qda(YN~BMI+Age, data=train)
  
  prd.fit4 <- predict(lda.fit4, newdata = test) 
  roc.fit4 <- roc(test$YN, prd.fit4$posterior[,2])
  
  ggroc(roc.fit4)
  auc(roc.fit4)
  
  ggroc(list(Age=roc.fit1, BMI=roc.fit2, 
             'Age + BMI'=roc.fit3, 'Age^2 + BMI^2'=roc.fit4))
}










