Stacking.Results <- NULL
Stacking.Results[[1]] <- assess.meta.classifer.all(function(X,y) CV.Glmnet.with.NB(X,y)$model)
Stacking.Results[[2]] <- assess.meta.classifer.all(train.NB.normal)
