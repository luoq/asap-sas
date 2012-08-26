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
