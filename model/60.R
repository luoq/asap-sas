comment="glmnet on 1-3-gram,bintf,calibrated with NB,no standardize,use neldermead to select lambda"
require(Metrics)
require(glmnet)
require(neldermead)
require(boot)#for logit and inv.logit
source('general/util.R')
source('model/auxiliary.R')
return_each_fold_kappa <- FALSE
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
apply.model <- function(model,X){
  pred <- predict(model$fit,X,s=model$s)
  pred <- apply.multi.NB.normal(model$nb,pred)
}
train.model <- function(X,y){
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  fit <- glmnet(X,y,alpha=0.8,nlambda=100,standardize=FALSE,family="gaussian")
  lambda <- fit$lambda
  fits <- lapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],lambda=lambda,alpha=0.8,,standardize=FALSE,family="gaussian")
  })
  estimate.kappa <- function(s){
    s <- inv.logit(s[1,1])#s is a matrix here
    kappa <- sapply(1:K,function(k){
      omit <- all.folds[[k]]
      nb <- train.multi.NB.normal(predict(fits[[k]],X[-omit,,drop=FALSE],s=s),y[-omit])
      pred <- predict(fits[[k]],X[omit,,drop=FALSE],s=s)
      pred <- apply.multi.NB.normal(nb,pred)
      ScoreQuadraticWeightedKappa(pred,y[omit])
    })
    MeanQuadraticWeightedKappa(kappa)
  }
  temp <-fminsearch(function(x) -estimate.kappa(x),logit(lambda[length(lambda)]))
  s <- inv.logit(temp$x[1,1])
  kappa <- -temp$fval[1]

  nb <- train.multi.NB.normal(predict(fit,X,s=s),y)
  model <- list(fit=fit,s=s,nb=nb)
  return(list(model=model,kappa=kappa))
}
