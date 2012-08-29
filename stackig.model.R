Stacking.Results <- NULL
Stacking.Results[[1]] <- assess.meta.classifer.all(function(X,y) CV.Glmnet.with.NB(X,y)$model)
Stacking.Results[[2]] <- assess.meta.classifer.all(train.NB.normal)
Stacking.Results[[3]] <- assess.meta.classifer.all(train.rpart.model)
Stacking.Results[[4]] <- assess.meta.classifer.all(train.rf.model)
Stacking.Results[[5]] <- assess.combining.method.all(function(x) with(x,vote(class2)))
Stacking.Results[[6]] <-
  assess.combining.method.all(function(x) with(x,{
    prec <- apply(class1,2,function(x) precision(x,y1))
    weight <- -(log(prec)+log(1-prec))
    vote(class2,weight=weight)
  }))
f7 <- function(x) with(x,{
    X1 <- cBind(1*(Set[[essay_set]]$dtm[mask,]!=0),class1[,2:ncol(class1)])
    X2 <- cBind(1*(Set[[essay_set]]$dtm[-mask,]!=0),class2[,2:ncol(class2)])
    model <- CV.Glmnet.with.NB(X1,y1)$model
    predict(model,X2)$class
  })
Stacking.Results[[7]] <- assess.combining.method.all(f7)
Stacking.Results[[8]] <- assess.meta.classifer.all(train.logitboost.model)
Stacking.Results[[9]] <- assess.meta.classifer.all(function(X,y) CV.Glmnet.with.NB(X,y,glmnet.ctrl=list(alpha=0.8,standardize=TRUE))$model)
Stacking.Results[[10]] <- assess.meta.classifer.all(function(X,y) CV.Glmnet(X,y,glmnet.ctrl=list(alpha=0.8,standardize=FALSE,family="multinomial"))$model)
f11 <- function(x) with(x,{
  ## omit NB.Multinomial
  m <- length(unique(y1))
  omit <- m:(2*m-2)
  X1 <- prob1[,-omit]
  X2 <- prob2[,-omit]
  model <- train.NB.normal(X1,y1)
  predict(model,X2)$class
})
Stacking.Results[[11]] <- assess.combining.method.all(f11)
f12 <- function(x) with(x,{
  ## omit NB.Multinomial Glmnet.Multinomail
  m <- length(unique(y1))
  omit <- m:(3*m-3)
  X1 <- prob1[,-omit]
  X2 <- prob2[,-omit]
  model <- train.NB.normal(X1,y1)
  predict(model,X2)$class
})
Stacking.Results[[12]] <- assess.combining.method.all(f12)
f13 <- function(x) with(x,{
  ## omit NB.Multinomial SVM
  m <- length(unique(y1))
  omit <- c(m:(2*m-2),(3*m-2):(4*m-3))
  X1 <- prob1[,-omit]
  X2 <- prob2[,-omit]
  model <- train.NB.normal(X1,y1)
  predict(model,X2)$class
})
Stacking.Results[[13]] <- assess.combining.method.all(f13)
f14 <- function(x) with(x,{
  ## omit NB.Multinomial
  m <- length(unique(y1))
  omit <- m:(2*m-2)
  X1 <- prob1[,-omit]
  X2 <- prob2[,-omit]
  model <- train.rpart.model(X1,y1)
  predict(model,X2)$class
})
Stacking.Results[[14]] <- assess.combining.method.all(f14)
f15 <- function(x) with(x,{
  ## omit NB.Multinomial
  m <- length(unique(y1))
  omit <- m:(2*m-2)
  X1 <- prob1[,-omit]
  X2 <- prob2[,-omit]
  model <- CV.Glmnet.with.NB(X1,y1)$model
  predict(model,X2)$class
})
Stacking.Results[[15]] <- assess.combining.method.all(f15)
