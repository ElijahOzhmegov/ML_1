# ----- Cross-validation and bootstrapping -----
rm(list = ls())
# find.package('boot')
# install.packages('boot')
library(boot)
library(ISLR)

# ----- here you will learn -----
# sample()
# predict()
# poly()
# glm()
# cv.glm() are form boot library

# 1 Model evaluation using cross-validation -----
# 1.1 The Auto data set
?Auto
str(Auto, max.level=1)
names(Auto)
search()
attach(Auto)

## 2.3.5 Additional Graphical and Numerical Summaries
plot(cylinders, mpg)
cylinders = as.factor(cylinders)
plot(cylinders, mpg)

plot(cylinders, mpg, col='red', varwidth=TRUE, horizontal=TRUE,
     ylab="cylinders", xlab='MPG')

hist(mpg, col=2, breaks = 15)
pairs(Auto)
pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)

identify(horsepower, mpg, name)

summary(Auto)
detach(Auto)

# 1.2 Quadratic regression model
plot(mpg ~ horsepower, data=Auto)
lm.hp <- lm(mpg ~ horsepower, data=Auto)
abline(lm.hp)

lm.quad.hp <- lm(mpg ~ horsepower + I(horsepower ** 2), data=Auto)
summary(lm.quad.hp)

fq <- function(x)
  lm.quad.hp$coefficients[1] + 
  lm.quad.hp$coefficients[2]*x +
  lm.quad.hp$coefficients[3]*x**2

curve(fq, 40, 230, add=TRUE)  

# 1.3 Lab: Cross-validation
## 5.3.1 The Validation Set Approach
set.seed(2)
train=sample(392, 196)

lm.fit  <- lm(mpg ~ horsepower,          data=Auto, subset = train) # linear
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data=Auto, subset = train) # quadratic
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data=Auto, subset = train) # cubic

attach(Auto)

mean((mpg - predict(lm.fit,  Auto))[-train] ** 2)
mean((mpg - predict(lm.fit2, Auto))[-train] ** 2)
mean((mpg - predict(lm.fit3, Auto))[-train] ** 2)

detach(Auto)

{
  k = 5
  # lm.fit <- rep(NA, k)
  MSE    <- rep(NA, k)
  for(i in 1:k){
    lm.fit <- lm(mpg ~ poly(horsepower, i), data=Auto, subset = train)
    
    attach(Auto)
    MSE[i] <- mean((mpg - predict(lm.fit, Auto))[-train] ** 2)
    detach(Auto)
  }
  
  plot(1:k, MSE, 'b')
}

## 5.3.2 Leave-One-Out Cross-Validation

# here I want to make sure that glm and lm sometimes give the same result
glm.fit <- glm(mpg ~ horsepower, data=Auto)
coef(glm.fit)
lm.fit  <- lm(mpg ~ horsepower, data=Auto)
coef(lm.fit)

glm.fit <- glm(mpg ~ horsepower, data=Auto)
cv.err  <- cv.glm(Auto, glm.fit)
cv.err$delta

{
  k = 5
  cv.error <- rep(0, k)
  for(i in 1:k){
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
  }
  
  plot(1:k, cv.error)
}

## 5.3.3 k-Fold Cross-Validation
{
  k = 10
  cv.error <- rep(0, k)
  for(i in 1:k){
    glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.error[i] <- cv.glm(Auto, glm.fit, K=k)$delta[1]
  }
  
  plot(1:k, cv.error, 'b')
}

# 2 Model Selection using cross-validation

{ # defining a minimal model
  glm.fit <- glm(mpg ~ poly(horsepower, 2), data = Auto)
  cv.err  <- cv.glm(Auto, glm.fit)
  cv.err$delta
}

{ # Now add each of the other variables
  i = 0; k = 6
  cv.error <- rep(0, k)
  label    <- rep(NA, k)
  
  cv.error[i<-(i+1)] <- cv.err$delta[1]
  label[i] <- "Nothing"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Year"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + weight, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Weight"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + acceleration, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Acceleration"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + displacement, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Displacement"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + cylinders, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Cylinders"
  
  plot(as.factor(label), cv.error)
}

# Which new variable gives the lowest LOOCV MSE?
print("year")


{ # Now add each of the other variables
  i = 0; k = 5
  cv.error <- rep(0, k)
  label    <- rep(NA, k)
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year, data=Auto)
  cv.error[i<-(i+1)] <- cv.err$delta[1]
  label[i] <- "Nothing"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Weight"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + acceleration, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Acceleration"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + displacement, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Displacement"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + cylinders, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Cylinders"
  
  plot(as.factor(label), cv.error)
}

# Which new variable gives the lowest LOOCV MSE?
print("year + weight")

{ # Now add each of the other variables
  i = 0; k = 4
  cv.error <- rep(0, k)
  label    <- rep(NA, k)
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Nothing"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + acceleration, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Acceleration"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + displacement, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Displacement"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + cylinders, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Cylinders"
  
  plot(as.factor(label), cv.error)
}

# Which new variable gives the lowest LOOCV MSE?
print("year + weight + acceleration")

{ # Now add each of the other variables
  i = 0; k = 3
  cv.error <- rep(0, k)
  label    <- rep(NA, k)
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + acceleration, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Nothing"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + acceleration + displacement, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Displacement"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + acceleration + cylinders, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Cylinders"
  
  plot(as.factor(label), cv.error)
}

# Which new variable gives the lowest LOOCV MSE?
print("year + weight + acceleration + displacement")

{ # Now add each of the other variables
  i = 0; k = 2
  cv.error <- rep(0, k)
  label    <- rep(NA, k)
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + acceleration + displacement, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Nothing"
  
  glm.fit            <- glm(mpg ~ poly(horsepower, 2) + year + weight + acceleration + displacement + cylinders, data=Auto)
  cv.error[i<-(i+1)] <- cv.glm(Auto, glm.fit)$delta[1]
  label[i] <- "Cylinders"
  
  plot(as.factor(label), cv.error)
}

# Which new variable gives the lowest LOOCV MSE?
print("year + weight + acceleration + displacement + cylinders")

# by the last graph the last model is perfect, but summary says different
summary(glm.fit)

# 3 Programming Cross-Validation
rm(list=ls())
search()

{
    n <- 30
    x <- 1:n
    truey <- rep(c(10, 15, 8), each=n/3)
    plot(x, truey, type="l")

    set.seed(15535)
    yobs <- truey + rnorm(n)

    # plot the simulated data
    plot(x, yobs)
    lines(x, truey)
}

{ ### linear regression using loocv (0)
    # creare new data by leaving one observation out
    loo.x  <- x[-1]
    loo.y  <- yobs[-1]

    ## fit the linear regression model with the loo data
    loo.fit <- lm(loo.y ~ loo.x)

    # plot the resulting regression
    abline(loo.fit, col="grey")

    ## find the sum of squared error using the missing point
    (predict(loo.fit, newdata = data.frame(loo.x=x[1])) - yobs[1]) ** 2
}

{ ### linear regression using loocv
    loo.sse <- 0

    # plot the simulated data
    plot(x, yobs)
    lines(x, truey)

    ## loop over each observation
    for(i in n){
        # create new data by leaving one observation out
        loo.x <- x[-i]
        loo.y <- yobs[-i]

        ## fit the linear regression model with loo data
        loo.fit <- lm(loo.y ~ loo.x)

        # plot the resulting regression
        abline(loo.fit, col='grey')

        ## update the sum of squared error using the missing point 
        MSE_i   <- (predict(loo.fit, newdata=data.frame(loo.x=x[i])) - yobs[i]) ** 2
        loo.sse <- loo.sse + MSE_i
    }

    ### divide by n to get the mse estimate 
    cv.mse <- loo.sse/n
    print(cv.mse)
}

{ ### generic function
    loo.mse <- rep(NA, k <- 10)

    n <- length(x)

    for(kk in 1:k){
        loo.sse <- 0
        for(i in 1:n){
            # create new data by leaving one observation out
            loo.x <- x[-i]
            loo.y <- yobs[-i]

            ## fit the polynominal regression model with the loo data
            loo.fit <- lm(loo.y ~ poly(loo.x, degree=kk, raw=TRUE))

            ## update the sum of squared error using the missing point
            MSE_i   <- (predict(loo.fit, newdata=data.frame(loo.x=x[i])) - yobs[i]) ** 2
            loo.sse <- loo.sse + MSE_i
        }
        ### divide by n
        loo.mse[kk] <- loo.sse/n
    }

    plot(loo.mse, type='b')
}

print("k = 4 is the best!")

{ ### final fitted using k=4
    lm.obj <- lm(yobs ~ poly(x, degree=4))
    plot(x, lm.obj$fitted.values, col=2, type="l")
    points(x, yobs)
    lines(x, truey)

}

# The Bootstrap

{
    # Create the original sample of length 75
    xsamp <- rexp(75, 0.03)
    hist(xsamp, breaks=10, main="Histogram of sample", xlab='x')

    mean(xsamp)

    # take one resample (length 75 with replacement)
    # and calculate the mean of the resample
    resamp <- sample(xsamp, 75, replace=TRUE)
    mean(xsamp)
    B <- 100

    # 100 Bootstrap resamples
    bsmean  <- rep(NA, B)
    for(i in 1:B){
        resamp     <- sample(xsamp, 75, replace=TRUE)
        bsmean[i]  <- mean(resamp)
    }

    hist(bsmean, breaks=15, main="Histogram of the Bootstraped means", xlab="mean(x)")
    var(bsmean) # Bootstrap estimate of the variance 

    quantile(bsmean, c(0.025, 0.975))

    # how do you get a 90% CI?
    # answer:
    quantile(bsmean, c(0.05, 0.95))

    abline(v=quantile(bsmean, c(0.025, 0.975)), col=2)

}

### Estimating the Accuracy of a Liner Regression Model
library(boot)
#install.packages('ISLR')
library(ISLR)
attach(Auto)

{
    boot.fn <- function(data_, index)
        return(coef(lm(mpg~horsepower, data=data_, subset=index)))

    boot.fn(Auto, 1:392)
}

{   # boot.fn can be used in order to create bootstrap estimates 
    # for the intercept and slope terms by randomly sampling from 
    # among the observations with replacement
    set.seed(1)
    boot.fn(Auto, sample(392, 392, replace=TRUE))
    boot.fn(Auto, sample(392, 392, replace=TRUE))
}

boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef

















