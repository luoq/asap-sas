comment="nu-svr on 1-3-gram,bintf,calibrated with NB"
require(Metrics)
require(e1071)
source('general/util.R')
source('model/auxiliary.R')
return_each_fold_kappa <- FALSE
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
apply.model <- function(model,X){
  pred <- predict(model$fit,X)
  pred <- apply.NB.normal(model$nb,pred)
}
train.model <- function(X,y){
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  estimate.kappa <- function(nu){
    kappa <- sapply(1:K,function(k){
      omit <- all.folds[[k]]
      fit <- svm(X[-omit,,drop=FALSE],y[-omit],scale=FALSE,type="nu-regression",kernel="linear",nu=nu)
      nb <- train.multi.NB.normal(predict(fit,X[-omit,,drop=FALSE]),y[-omit])
      pred <- predict(fit,X[omit,,drop=FALSE])
      pred <- apply.NB.normal(nb,pred)
      ScoreQuadraticWeightedKappa(pred,y[omit])
    })
    MeanQuadraticWeightedKappa(kappa)
  }

  nus <- c(0.01,0.1,0.2,0.4,0.6,0.8)
  kappas <- sapply(nus,estimate.kappa)
  i <- which.max(kappas)
  nu <- nus[i]
  kappa <- kappas[i]

  fit <- svm(X,y,scale=FALSE,type="nu-regression",kernel="linear",nu=nu)
  nb <- train.multi.NB.normal(predict(fit,X),y)
  model <- list(fit=fit,nb=nb)
  return(list(model=model,kappa=kappa))
}
