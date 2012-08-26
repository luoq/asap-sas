require(glmnet)
train.Glmnet <- function(X,y,s=NULL,...){
  fit <- glmnet(X,y,...)
  model <- list(glmnet=fit,s=s)
  class(model) <- list("Glmnet","list")
  model
}
predict.Glmnet <- function(model,X){
  pred <- predict(model$glmnet,s=model$s,X)
  return(list(value=pred))
}
train.Glmnet.with.NB <- function(X,y,glmnet.ctrl=NULL)
  train.calibrator.model(X,y,
                         function(X,y) do.call(train.Glmnet,c(list(X,y),glmnet.ctrl)),
                         function(X,y) train.NB.normal(X,y,multi.model=TRUE))
CV.Glmnet.with.NB <- function(X,y,nlambda=100,glmnet.ctrl=list(alpha=0.8,standardize=FALSE),
                              cv.ctrl=NULL){
  fit <- do.call(glmnet,c(list(X,y,nlambda=nlambda),glmnet.ctrl))
  lambda <- fit$lambda
  res <- do.call(CV,c(list(X,y,
                           train.f=function(X,y,s) train.Glmnet.with.NB(X,y,glmnet.ctrl=c(list(s=s,lambda=lambda),glmnet.ctrl)),
                           parameter=list(s=lambda),
                           multi.models=TRUE,intrinsic.multi.training=TRUE,select.model=TRUE,return.multi.models=FALSE,retrain=FALSE),
                      cv.ctrl))
  s <- res$best.parameter
  base.model <- list(glmnet=fit,s=s)
  class(base.model) <- list("Glmnet","list")
  base.pred <- predict(fit,X,s=s)
  calibrator <- train.NB.normal(base.pred,y)
  model <- list(base=base.model,calibrator=calibrator,calibrate.on="value")
  class(model) <- list("calibrator.model","list")
  res$model <- model
  res
}
