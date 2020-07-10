# Workshop 10: Tree Models: Classification trees -------------------------------

rm(list = ls())

library(tidyverse)
library(magrittr)

# Exercise 1 -------------------------------------------------------------------
# Work through Lab 8 Section 8.3.1 on classification tres in James et al. 
# Page 323.

library(tree)
library(ISLR)

{ # we need a binary variable
  Carseats %<>% 
    mutate(
      High = ifelse(Sales > 9, "Yes", "No") %>% factor()
    )
}

{ # fitting tree
  tree.carseats = tree(High~.-Sales, Carseats)
  summary(tree.carseats)
  
  plot(tree.carseats)
  text(tree.carseats, pretty = 0)
  
  tree.carseats
  
}

{ # the same as above but with training and testing data set
  set.seed (2)
  
  train=sample(1:nrow(Carseats), nrow(Carseats)/2)
  Carseats.test=Carseats[-train,]
  
  High.test     = Carseats$High[-train]
  tree.carseats = tree(High~. -Sales, Carseats,subset=train)
  tree.pred     = predict(tree.carseats, Carseats.test, type="class")
  
  table(tree.pred, High.test)
  # accuracy
  (122 + 32)/200
  
}

{ # now let's apply prunning and try to see the difference 
  set.seed (3)
  cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
  names(cv.carseats)
  cv.carseats
  str(cv.carseats)
  
  par(mfrow=c(1,2))
  plot(cv.carseats$size, cv.carseats$dev ,type="b")
  plot(cv.carseats$k,    cv.carseats$dev ,type="b")
  
  # nine is the best 
  prune.carseats=prune.misclass(tree.carseats,best=9)
  
  plot(prune.carseats)
  text(prune.carseats, pretty = 0) # looks simplier 
  
  # compare with full tree
  tree.pred = predict(prune.carseats, Carseats.test, type="class")
  table(tree.pred, High.test)
  
  # accuracy was 77%, now
  (129 + 22)/200
  # it looks like we failed to prove the point 
  
}

rm(list = ls())
library(rpart)
library(rpart.plot)
library(data.table)

{ # rpart based 
  carseats <- ISLR::Carseats
  setDT(carseats)
  carseats[, high := as.factor(ifelse(Sales <= 8, "No", "Yes"))]
  setnames(carseats, names(carseats), str_to_lower(names(carseats)))
  str(carseats)
  
  set.seed(2)
  train <- sample(1:nrow(carseats), size = 200)
  test  <- carseats[-train]
  
  tree.carseats <- rpart(high ~ . - sales, 
                         data = carseats, 
                         method = "class", 
                         subset = train, 
                         control = list(cp = 0, 
                                        maxcompete = 0, 
                                        maxsurrogate = 0, 
                                        xval = 0, 
                                        minsplit = 10, 
                                        minbucket = 5))
  rpart.plot(tree.carseats)
  
  print(tree.carseats) #text version of tree
  printcp(tree.carseats) #prints only the cptable, or for no extra info whatsoever, tree.carseats$cptable
  
  set.seed(2)
  tree.carseats <- rpart(high ~ . - sales, data = carseats, subset = train, method = "class") #notice no control set
  preds <- xpred.rpart(tree.carseats) #Returns a column of predicted values
  #(the integer value of a factor) for each cp
  err  <- (preds - as.integer(carseats[train]$high))^2
  err  <- apply(err, 2, sum)
  cp_choose <- as.numeric(names(err)[which.min(err)])
  tree.carseats$cptable
  
  plotcp(tree.carseats)
  
  tree.pruned <- prune.rpart(tree.carseats, cp_choose)
  rpart.plot(tree.pruned)
  
  tree.pred <- predict(tree.pruned, newdata = test, type = "class")
  confusion.matrix <- table(tree.pred, test$high)
  confusion.matrix
  sum(diag(confusion.matrix)) / sum(confusion.matrix)
  
  { # rpart choose cp automatically
    set.seed(9)
    tree.carseats <- rpart(high ~ . - sales, data = carseats, subset = train, method = "class")
    cp_choose   <- tree.carseats$cptable[,1][which.min(tree.carseats$cptable[,4])]
    tree.pruned <- prune.rpart(tree.carseats, cp_choose)
    tree.pred   <- predict(tree.pruned, newdata = test, type = "class")
    confusion.matrix <- table(tree.pred, test$high)
    confusion.matrix
    
    sum(diag(confusion.matrix)) / sum(confusion.matrix)
    
    plotcp(tree.carseats)
    plotcp(tree.pruned)
    cp_choose
    rpart.plot(tree.pruned)
    
  }
  
}

# Exercise 2 -------------------------------------------------------------------
# Brexit referendum results: regression tree and classification tree
rm(list=ls())
{ # a
  load("data/Brexit.rda")
  
  # (i) How many districts are there?
  nrow(Brexit)
  
  # (ii) What are the variables in the data set?
  names(Brexit)
  str(Brexit)
  
  # (iii) What proportion of voters voted leave and what proportion voted 
  # remain?
  Brexit %>% 
    summarise(
      remain = sum(Remain),
      leave  = sum(Leave),
      total  = remain + leave,
      remain_p = remain/total,
      leave_p  = leave/total
    )
  
  # (iv) What proportion of districts “voted leave” i.e. had over 50% Leave 
  # in that district? Hint: Status
  
  Brexit %>% 
    group_by(Status) %>% 
    summarise(
      total = n(),
      prop = n()/382
    )
  
  # (v) Obtain a Boxplot for percentage of leave votes by Region.
  Brexit %>% 
    mutate(
      pc_leav_votes = Leave/(Leave + Remain)
    ) %>% 
    ggplot(aes(y=pc_leav_votes, x=Region, fill=Region)) +
    geom_boxplot()
  
  
}

{ # (b) Split the data into a training and a test data set with 300 
  # observations in the training set.
  
  train = sample(1:nrow(Brexit), 300)
  test  = (1:nrow(Brexit))[-train]
  
}

{ # (c) Use the variables Region,Electorate,Pct_Turnout,Votes_Cast and 
  # Pct_Rejected 
  # to fit a regression tree for the variable Pct_Leave. Plot the full tree, 
  # prune the tree and plot the pruned tree. Find the mean squared error for 
  # the pruned tree using the test data.
  
  tree.brexit = Brexit %>% 
    rpart(Pct_Leave ~ Region + Electorate + Pct_Turnout + Votes_Cast + Pct_Rejected,
          data = ., subset = train)
  tree.brexit$cptable
  rpart.plot(tree.brexit)
  plotcp(tree.brexit)
    
  # prunning
  cp_choose   <- tree.brexit$cptable[,1][which.min(tree.brexit$cptable[,4])]
  cp_choose
  tree.pruned <- prune.rpart(tree.brexit, cp_choose)
  
  dotted.line = tree.brexit$cptable[,c('xerror', 'xstd')] %>% tail(n=1) %>%  sum()
  ind = which(tree.brexit$cptable[, c('xerror')] < dotted.line)[[1]]
  new_cp = tree.brexit$cptable[, 'CP'][(ind-1):ind] %>% mean()
  new_cp
  small.tree.pruned <- prune.rpart(tree.brexit, new_cp)
  
  # plotting 
  rpart.plot(tree.brexit)
  rpart.plot(tree.pruned)
  rpart.plot(small.tree.pruned)
  
  # calculating MSE, comparing trees
  pred.full.tree = predict(tree.brexit, newdata = Brexit[test, ])
  mean((pred.full.tree - Brexit$Pct_Leave[test])**2)
  
  pred.pruned.tree = predict(tree.pruned, newdata = Brexit[test, ])
  mean((pred.pruned.tree - Brexit$Pct_Leave[test])**2)
  
  pred.small.tree = predict(small.tree.pruned, newdata = Brexit[test, ])
  mean((pred.small.tree - Brexit$Pct_Leave[test])**2)
  # Tim's approach is a total garbage
  
}

{ # (d) Fit a classification tree for Leave Status. Repeat the steps in part 
  # (c) and obtain a confusion matrix for the test data. What are the name(s) 
  # of the district(s) which is/are predicted as remain but actually voted leave?
  
  tree.brexit = Brexit %>% 
    rpart(Status ~ Region + Electorate + Pct_Turnout + Votes_Cast + Pct_Rejected,
          data = ., subset = train, method = "class")
  cp_choose   <- tree.brexit$cptable[,1][which.min(tree.brexit$cptable[,4])]
  tree.pruned <- prune.rpart(tree.brexit, cp_choose)
  
  pred.pruned.tree = predict(tree.pruned, newdata = Brexit[test, ], type="class")
  # sum((pred.pruned.tree == Brexit$Status[test]))/length(test)
  
  confusion.matrix <- table(pred.pruned.tree, Brexit$Status[test])
  confusion.matrix
  
  sum(diag(confusion.matrix)) / sum(confusion.matrix)
  
  # Я уже заебался решать это говно, убейте меня нежно
  
  foobar = Brexit[test,]
  foobar$pred = pred.pruned.tree
  str(foobar)
  
  foobar %>% 
    filter(Status != pred) %>% 
    filter(Status == "Leave") %>% 
    select(Region, Area)
  
}

{ # (e) Plot the ROC curve for the training data and obtain the AUC by adapting 
  # the commands below.
  require(pROC)
  predleavep<-predict(tree.pruned, newdata=Brexit[train, ], type = "prob")[,2] # this is the predicted probability of "Leave"
  roc.obj1 <- roc(Brexit$Status[train],predleavep)
  ggroc(roc.obj1)
  auc(roc.obj1)
  
}

# Exercise 3 Adjusting rpart parameters for unbalanced data --------------------
rm(list=ls())

load("data/Diabetes.rda")
str(Diabetes)
set.seed(666)

test  = sample(1:nrow(Diabetes), 2000)
train = (1:nrow(Diabetes))[-test]

{ # Use rpart to fit the default model. Notice that this has no splits.
  tree.diabetes = Diabetes %>% 
    rpart(YN~., data = ., subset = train, control = list(cp = 0))
  
  plotcp(tree.diabetes)
  
  # КАК ЖЕ, СУКА, Я ЗАЕБАЛСЯ 
  prun_my_bush <- function(tree){
    cp_choose   <- tree$cptable[,1][which.min(tree$cptable[,4])]
    tree.pruned <- prune.rpart(tree, cp_choose)
  }
  tree.pruned = prun_my_bush(tree.diabetes)
  plotcp(tree.pruned)
  
  # ROC
  # ТЕКСТОВЫЙ РЕДАКТОР В АРЕ ГОВНО1 КАК НЕ СТЫДНО ЭТИМ ПИДОРАМ??!!1
  predleavep<-predict(tree.pruned, newdata=Diabetes[test, ], type = "prob")[,2] 
  roc.obj1 <- roc(Diabetes$YN[test],predleavep)
  ggroc(roc.obj1)
  auc(roc.obj1)
  
}

{ # Loss matrix
  
  {
    x = 5
    lossmat<-matrix(c(0,x,1,0),2,2)
    rptree<-rpart(YN~BMI+Age, data = Diabetes, subset = train,
                  parms=list(loss=lossmat))
    rpart.plot(rptree)
  }
  
  predleavep<-predict(rptree, newdata=Diabetes[test, ], type = "prob")[,2] 
  roc.obj2 <- roc(Diabetes$YN[test],predleavep)
  ggroc(roc.obj2)
  auc(roc.obj2)
}

{ # Priors
  piNo = Diabetes[train,] %>% 
    group_by(YN) %>% 
    summarise(
      n = n()
    )
  
  piNo = piNo$n[1]/ sum(piNo$n)
  
  piNo = piNo - 0.1
  rptree2<-rpart(YN~BMI+Age, data = Diabetes, 
                subset = train, 
                parms=list(prior=c(piNo,1-piNo)))
  
  predleavep<-predict(rptree2, newdata=Diabetes[test, ], type = "prob")[,2] 
  roc.obj3 <- roc(Diabetes$YN[test],predleavep)
  ggroc(roc.obj3)
  auc(roc.obj3)
  
  ggroc(list(pruned=roc.obj1,
             loss  =roc.obj2,
             pi    =roc.obj3))
  
}













