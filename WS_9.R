# Workshop 9: Tree Models: regression trees ------------------------------------

rm(list = ls())

library(tidyverse)

# Exercise 1 Boston data set ---------------------------------------------------
library(MASS)
?Boston

{ # a: How many rows are in this data set? Each row represents a different 
  # suburb in Boston, Massachusetts.
  # How many columns are there? Use the function names() to output the 
  # variable names.
  
  nrow(Boston)
  ncol(Boston)
  names(Boston)
}

{ # b: The variable medv is our outcome variable used in the regression tree. 
  # This is the median value of owner occupied homes in each Boston suburb in 
  # $1000s. Obtain the summary Statistics and a boxplot for medv. 
  Boston %>% 
    ggplot(aes(y=medv)) +
    geom_boxplot()
  
  summary(Boston$medv) 
  boxplot(x=Boston$medv)
  
}

{ # c: Plot a scatterplots for each of the other (predictor) variables in this 
  # data set. Are there any which have a clear association with medv.
  
  pairs(Boston)
  print("rm")
  print("lstat")
  
  Boston %>% 
    ggplot(aes(x=medv, y=rm)) +
    geom_point()
  
  Boston %>% 
    ggplot(aes(x=medv, y=lstat)) +
    geom_point()
  
}

{ # d: How many of the suburbs in this data set border the Charles river? 
  # Obtain a boxplot of medv split by this variable?
  
  Boston %>% 
    mutate(chas=as.factor(chas)) %>% 
    filter(chas == 1) %>% 
    nrow()
  
  Boston %>% 
    ggplot(aes(fill=as.factor(chas), y=medv)) +
    geom_boxplot()
  
  { # old fashioned way
    sum(Boston$chas == 1)
    boxplot(data=Boston, medv~chas)
  }
}

{ # e: What is the median pupil-teacher ratio among the suburbs in this data set?
  Boston %>% 
    group_by(chas) %>% 
    summarise(m_pt = median(ptratio))
  
  { # old fashion way
    tapply(Boston$ptratio, Boston$chas, median)
  }
  
}

# Exercise 2 -------------------------------------------------------------------
# You will now fit a regression tree to the Boston data using medv as the 
# outcome variable. Remember that a regression trees is used when the outcome 
# variable is continuous. The majority of this exercise follows Lab 8.3.2 in 
# James et al. but using the rpart package instead of the tree package.
library(rpart)
library(rpart.plot)

{
  set.seed(1)
  train = sample(1:nrow(Boston), nrow(Boston)/2)
  
  tree.boston = Boston %>% 
    rpart(medv~., data = ., subset = train)
  
  print(tree.boston)
  
  rpart.plot(tree.boston)
  
  { # prunning 
    cpmatrix = tree.boston %>% printcp()
    tree.boston %>% plotcp()
    
    dotted.line = cpmatrix[,c('xerror', 'xstd')] %>% tail(n=1) %>%  sum()
    
    ind = which(cpmatrix[, c('xerror')] < dotted.line)[[1]]
    new_cp = cpmatrix[, 'CP'][(ind-1):ind] %>% mean()
    
    prune.boston = prune(tree.boston, cp=new_cp)
    prune.boston
    rpart.plot(prune.boston)
  }
  
  { # Compare the mean square error (MSE) for the unpruned and pruned tree 
    # (on train data?)
    pred.train = predict(tree.boston, newdata = Boston[train, ])
    mean((pred.train - Boston$medv[train])**2)
    
    pred.train = predict(prune.boston, newdata = Boston[train, ])
    mean((pred.train - Boston$medv[train])**2)
  }
  
  # Notice that for the test data the MSE for the pruned tree is only a little 
  # larger than for the unpruned tree, but we have gained a slightly simpler model.
  
  { # Compare the mean square error (MSE) for the unpruned and pruned tree 
    # (on test data)
    pred.test = predict(tree.boston, newdata = Boston[-train, ])
    mean((pred.test - Boston$medv[-train])**2)
    
    pred.test = predict(prune.boston, newdata = Boston[-train, ])
    mean((pred.test - Boston$medv[-train])**2)
  }
  
  { # Plot the observed median values medv against the pruned tree predictions 
    boston.test=Boston[-train,"medv"] 
    plot(pred.test, boston.test) 
    abline(c(0,1))
    }
}

# Exercise 3 Carseat data ------------------------------------------------------
library(ISLR)
{ # (a) Load this package and investigate the Carseats data in a similar way 
  # to Exercise 1
  
  { # a: How many rows are in this data set? 
    # How many columns are there? Use the function names() to output the 
    # variable names.
    
    nrow(Carseats)
    ncol(Carseats)
    names(Carseats)
    str(Carseats)
  }
  { # c: Plot a scatterplots for each of the other (predictor) variables in this 
    # data set. Are there any which have a clear association with medv.
    
    pairs(Carseats)
    
    Carseats %>% 
      ggplot(aes(x=Price, y=Sales)) +
      geom_point()
    
    Carseats %>% 
      ggplot(aes(x=CompPrice, y=Price)) +
      geom_point()
    
  }
}

{ # (b) Split the data into a 50-50 split for training and testing.
  train = sample(1:nrow(Carseats), nrow(Carseats)/2)
}

{ # (c) Obtain the rpart default regression tree, and plot it.
  carseats.tree = Carseats %>% 
    rpart(Sales ~ ., data = ., subset = train)
  
  print(carseats.tree)
  rpart.plot(carseats.tree)
}

{ # (d) Find an acceptable pruned tree and plot it.
  cpmatrix = carseats.tree %>% printcp()
  carseats.tree %>% plotcp()
  
  dotted.line = cpmatrix[,c('xerror', 'xstd')] %>% tail(n=1) %>%  sum()
  dotted.line
  ind = which(cpmatrix[, c('xerror')] < dotted.line)[[1]]
  ind
  new_cp = cpmatrix[, 'CP'][(ind-1):ind] %>% mean()
  new_cp
  
  prunned.carseats.tree = prune(tree=carseats.tree, cp = new_cp)
  prunned.carseats.tree %>% rpart.plot()
  
}

{ # (e) The first row in the data, which has been assigned to the training 
  # data, is
  Carseats[1,]
  predict(prunned.carseats.tree, newdata = Carseats[1, ])
  
  { # MSE
    pred.test = predict(carseats.tree, newdata = Carseats[-train, ])
    mean((pred.test - Carseats$Sales[-train]) ** 2)
    
    pred.test = predict(prunned.carseats.tree, newdata = Carseats[-train, ])
    mean((pred.test - Carseats$Sales[-train]) ** 2)
  }
}

{ # (f) Which leaf node has the most elements? Summarise the decision rules 
  # which lead to this leaf node and give it’s predicted value.
  pred.train = predict(prunned.carseats.tree, newdata = Carseats[train, ])
  
  Carseats[train, ] %>% 
    mutate(
      leaf = ifelse(ShelveLoc != "Good", "left", "right"),
      leaf = case_when(
        leaf == 'left'  & Price >= 106 ~ '6.1',
        leaf == 'left'  & Price <  106 ~ '8.6',
        leaf == 'right' & Price >= 110 ~ '8.9',
        leaf == 'right' & Price <  110 ~ '12'
      ),
      leaf = factor(leaf, levels = c('6.1', '8.6', '8.9', '12'))
    ) %>% 
    group_by(leaf) %>% 
    summarise(
      n=n(),
      pct = n/2
    )
  
  str(Carseats)
  
}
















