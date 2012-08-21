comment="same as 40,random cv split"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
apply.model <- function(model,X){
  pred <- predict(model$fit,X,s=model$s)
  pred <- apply.multi.NB.normal(model$nb,pred)
}
train.model <- function(X,y){
  K <- 10
  n <- length(y)
  all.folds <- cv.kfold.random(n,K)

  fit <- glmnet(X,y,alpha=0.8,nlambda=100,standardize=FALSE,family="gaussian")
  lambda <- fit$lambda
  temp <- lapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],lambda=lambda,alpha=0.8,,standardize=FALSE,family="gaussian")
    nb <- train.multi.NB.normal(predict(fit,X[-omit,,drop=FALSE]),y[-omit])
    pred <- predict(fit,X[omit,,drop=FALSE])
    pred <- apply.multi.NB.normal(nb,pred)
    prec <- apply(pred,2,function(pred)
                  precision(pred,y[omit]))
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
    list(kappa=kappa,prec=prec)
  })
  kappa <- sapply(temp,function(x) x$kappa)
  prec <- sapply(temp,function(x) x$prec)

  mean.prec <- apply(prec,1,mean)
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  s <- lambda[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  prec <- c(prec[i,],mean.prec[i])
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  names(prec) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  nb <- train.multi.NB.normal(predict(fit,X,s=s),y)
  model <- list(fit=fit,s=s,nb=nb)
  return(list(model=model,kappa=kappa,prec=prec))
}
