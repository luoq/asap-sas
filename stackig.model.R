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
