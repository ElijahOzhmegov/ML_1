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
    lapply(Boston, median)
    ?apply()
    ?tapply
    ?lapply
    tapply(Boston$ptratio, median)
    # I don't remeber it
    tapply(iris$Sepal.Width, iris$Species, median)
    tapply(Boston$ptratio, Boston$chas, median)
  }
  
}


















