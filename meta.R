subset.model <- function(model,subset){
  model <- list(subset=subset,model=model)
  class(model) <- list("subset.model","list")
  model
}
predict.subset.model <- function(model,X){
  X <- X[,model$subset,drop=FALSE]
  predict(model$model,X)
}
train.calibrator.model <- function(X,y,train.base,train.calibrator,calibrate.on="class"){
  base.model <- train.base(X,y)
  base.pred <- predict(base.model,X)[[calibrate.on]]
  calibrator <- train.calibrator(base.pred,y)
  model <- list(base=base.model,calibrator=calibrator,calibrate.on=calibrate.on)
  class(model) <- list("calibrator.model","list")
  model
}
predict.calibrator.model <- function(model,X){
  base.pred <- predict(model$base,X)[[model$calibrate.on]]
  predict(model$calibrator,base.pred)
}
preprocess.wrap <- function(f,p){
  function(X,y,...){
    X <- p(X)
    res <- f(X,y,...)
    if(!is.null(res$model))
      res$model <- preprocess.model(res$model,p)
    res
  }
}
preprocess.model <- function(x,p){
  res <- list(model=x,p=p)
  class(res) <- c("preprocess.model",list)
  res
}
predict.preprocess.model <- function(model,X){
  X <- model$p(X)
  predict(model$model,X)
}
prebinarizer <- function(f) preprocess.wrap(f,binarizer)
