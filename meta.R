subset.model <- function(model,subset){
  model <- list(subset=subset,model=model)
  class(model) <- list("subset.model","list")
  model
}
predict.subset.model <- function(model,X){
  X <- X[,model$subset,drop=FALSE]
  predict(model$model,X)
}
