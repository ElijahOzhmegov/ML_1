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







