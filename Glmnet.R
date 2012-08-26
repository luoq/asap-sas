require(glmnet)
train.Glmnet <- function(X,y,s=NULL,...){
  fit <- glmnet(X,y,...)
  model <- list(glmnet=fit,s=s)
  class(model) <- list("Glmnet","list")
  model
}
predict.Glmnet <- function(model,X){
  pred <- predict(model$glmnet,s=model$s,X)
  return(list(class=pred))
}
train.Glmnet.with.NB <- function(X,y,glmnet.ctrl=NULL,calibrator.ctrl=NULL)
  train.calibrator.model(X,y,
                         function(X,y) do.call(train.Glmnet,c(list(X,y),glmnet.ctrl)),
                         function(X,y) do.call(train.NB.normal,c(list(X,y,multi.model=TRUE),calibrator.ctrl)))
CV.Glmnet.with.NB <- function(X,y,nlambda=100,glmnet.ctrl=list(alpha=0.8,standardize=FALSE),
                              cv.ctrl=NULL,calibrator.ctrl=NULL){
  fit <- do.call(glmnet,c(list(X,y,nlambda=nlambda),glmnet.ctrl))
  lambda <- fit$lambda
  res <- do.call(CV,c(list(X,y,
                           train.f=function(X,y,s) train.Glmnet.with.NB(X,y,
                             glmnet.ctrl=c(list(s=s,lambda=lambda),glmnet.ctrl),
                             calibrator.ctrl=calibrator.ctrl),
                           parameter=list(s=lambda),
                           multi.model=TRUE,intrinsic.multi.training=TRUE,select.model=TRUE,return.multi.model=FALSE,retrain=FALSE),
                      cv.ctrl))
  s <- res$best.parameter
  base.model <- list(glmnet=fit,s=s)
  class(base.model) <- list("Glmnet","list")
  base.pred <- predict(fit,X,s=s)
  calibrator <- do.call(train.NB.normal,c(list(base.pred,y),calibrator.ctrl))
  model <- list(base=base.model,calibrator=calibrator,calibrate.on="class")
  class(model) <- list("calibrator.model","list")
  res$model <- model
  res
}
train.Glmnet.with.NB.CV <- function(X,y,train.base=TRUE,glmnet.ctrl=list(alpha=0.8,standardize=FALSE),cv.ctrl=list(K=5,split="random"),
                                    calibrator.ctrl=NULL){
  res <- do.call(CV,c(list(X,y,
                           train.f=function(X,y) do.call(train.Glmnet,c(list(X,y),glmnet.ctrl)),
                           parameter=NULL,measure.fun=NULL,return.result=TRUE,
                           multi.model=TRUE,intrinsic.multi.training=TRUE,select.model=FALSE,return.multi.model=FALSE,retrain=train.base),
                      cv.ctrl))
  if(train.base)
    base <- res$model
  else
    base <- NULL
  base.pred <- aggregate.CV.result(res)
  calibrator <- do.call(train.NB.normal,c(list(base.pred,y,multi.model=TRUE),calibrator.ctrl))
  model <- list(base=base,calibrator=calibrator,calibrate.on="class")
  class(model) <- list("calibrator.model","list")
  model
}
CV.Glmnet.with.NB.CV <- function(X,y,nlambda=100,glmnet.ctrl=list(alpha=0.8,standardize=FALSE),
                                 cv.ctrl=list(K=10),calibrator.ctrl=NULL){
  base.model <- do.call(train.Glmnet,c(list(X,y,nlambda=nlambda),glmnet.ctrl))
  lambda <- base.model$glmnet$lambda
  res <- do.call(CV,c(list(X,y,
                           train.f=function(X,y,s) train.Glmnet.with.NB.CV(X,y,
                             glmnet.ctrl=c(list(s=s,lambda=lambda),glmnet.ctrl),
                             calibrator.ctrl=calibrator.ctrl),
                           parameter=list(s=lambda),
                           multi.model=TRUE,intrinsic.multi.training=TRUE,select.model=TRUE,return.multi.model=FALSE,retrain=FALSE),
                      cv.ctrl))
  s <- res$best.parameter
  base.model$s <- s

  model <- train.Glmnet.with.NB.CV(X,y,train.base=FALSE,glmnet.ctrl=c(list(s=s,lambda=lambda),glmnet.ctrl),calibrator.ctrl=calibrator.ctrl)
  model$base <- base.model
  res$model <- model
  res
}
CV.Glmnet.with.NB.2 <- prebinarizer(CV.Glmnet.with.NB)
