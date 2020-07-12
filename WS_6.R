# Workshop 6 -------------------------------------------------------------------
# Shrinkage Methods: Ridge regression and the Lasso ----------------------------

rm(list = ls())

library(tidyverse)
{ # Work through the few commands on page 244 of Lab 1 Section 6.5 to remove 
  # the NAs.
  library(ISLR)
  ?fix(Hitters)
  names(Hitters)
  dim(Hitters)
  sum(is.na(Hitters))
  
  hitters = na.omit(Hitters)
}

{ # Spend a few minutes getting to know the data. In particular produce a 
  # histogram of the outcome variable Salary and some scatter plots with Salary 
  # on the y-axis. As some of the variables have skewed distributions, you might
  # want to plot some axes on a log scale. The plot() command takes an argument 
  # log="x", log="y" or log="xy" to do this.
  str(hitters)
  hitters %>% 
    ggplot(aes(x=Salary)) + geom_histogram()
  
  
  hitters %>% 
    ggplot(aes(y=Salary,x=AtBat)) +
    geom_point()
  
  hitters %>% 
    ggplot(aes(y=Salary,x=CRuns)) +
    geom_point()
    
}

# Work through Section 6.6 Lab 2 pages 251 to page 255. A few extra commands ---
# have been suggested below; make a note of where they slot in before you 
# start on this section. Note that some of the numerical output values are 
# different in the Book, this is due to an update of the packages.

x = model.matrix(Salary~.,hitters)[,-1]
y = hitters$Salary

{ # Ridge Regression
  library(glmnet)
  
  grid = 10^seq(10,-2,length=100)
  ridge.mod = glmnet(x, y, alpha=0, lambda=grid)
  plot(grid,coef(ridge.mod)["AtBat",],log="x",typl="l")
  plot(ridge.mod,xvar="lambda")
  
  dim(coef(ridge.mod))
  
  ridge.mod$lambda[50] # lamda big
  coef(ridge.mod)[,50]
  
  ridge.mod$lambda[60] # lamda small
  coef(ridge.mod)[,60]
  
  sum(coef(ridge.mod)[,50] <  coef(ridge.mod)[,60])
  sum(coef(ridge.mod)[,50] >= coef(ridge.mod)[,60])
  
  # We can use the predict() function for a number of purposes. For instance, 
  # we can obtain the ridge regression coefficients for a new value of λ, say 50
  predict(ridge.mod,s=50,type="coefficients")[1:20,]
  
  
  # training and testing model
  set.seed(1)
  train = sample(1:nrow(x), nrow(x)/2)
  test  = (-train)
  y.test = y[test]
  
  # trainig a model
  ridge.mod  = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh =1e-12)
  ridge.pred = predict(ridge.mod,s=4,newx=x[test,])
  
  mean((ridge.pred-y.test)^2)
  mean((mean(y[train])-y.test)^2) # if we had instead simply fit a model with 
                                  #just an intercept
  
  # what if lamda is very big (infinity)
  ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
  mean((ridge.pred-y.test)^2)
  
  
  # lamda is EXACTLY 0
  ridge.pred=predict(ridge.mod, s=0, newx=x[test,], exact=T,
                     x=x[train,],y=y[train])
  mean((ridge.pred-y.test)^2)
  
  lm(y~x, subset=train)
  predict(ridge.mod, s=0, newx=x[test,], exact=T, # so when lambda = 0
                     x=x[train,],y=y[train],      # we hava just a linear model
                     type="coefficients")[1:20,]
  
  # cv for finding the right lambda
  set.seed (1)
  cv.out  = cv.glmnet(x[train ,],y[train],alpha=0)
  plot(cv.out)
  bestlam = cv.out$lambda.min
  bestlam
  
  # what's the MSE when out best lam is ${bestlam}
  ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,]) 
  mean((ridge.pred-y.test)^2)
  
  out = glmnet(x, y, alpha=0) # aplha 0 means ridge regression
  predict(out,type="coefficients",s=bestlam)[1:20,]
  # As expected, none of the coefficients are zero—ridge regression does 
  # not perform variable selection!
}

{ # The Lasso
  lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid) 
  plot(lasso.mod)
  
  set.seed (1)
  cv.out=cv.glmnet(x[train,], y[train], alpha=1)
  plot(cv.out)
  bestlam=cv.out$lambda.min
  lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
  mean((lasso.pred-y.test)^2)
  
  # the lasso has a substantial advantage over ridge regression in that the 
  # resulting coefficient estimates are sparse. Here we see that 12 of the 19 
  # coefficient estimates are exactly zero.
  out = glmnet(x,y,alpha=1,lambda=grid)
  lasso.coef = predict(out,type="coefficients",s=bestlam)[1:20,] 
  lasso.coef 
  
}

# Ridge regression and the lasso using the Auto data set -----------------------
library(boot)
# a chunk from the WS 5
glm.fit <- glm(mpg ~ poly(horsepower, 2) + year + weight + acceleration + displacement + cylinders, data=Auto)
summary(glm.fit)  








