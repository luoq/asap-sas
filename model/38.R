comment="glmnet on 1-3-gram with bintf,calibrated with NB and simple feature"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=TRUE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.and.predict <- function(sf,dtm,y,sf2,dtm2,lambda=lambda){
  dist0 <- estimate.conditional.normal.dist(sf,y,logprior=TRUE)
  logprior <- dist0$logprior
  levels <- dist0$levels
  L0 <- calc.conditional.normal.dist(dist0,sf2)
  L0 <- apply(L0,c(1,3),sum)
  L0 <- L0+outer(rep(1,nrow(sf2)),logprior)
  
  fit <- glmnet(dtm,y,lambda=lambda,alpha=0.8,family="gaussian")
  dist1 <- estimate.conditional.normal.dist(predict(fit,dtm),y,logprior=FALSE)
  L <- calc.conditional.normal.dist(dist1,predict(fit,dtm2),offset=L0)
  pred <- apply(L,c(1,2),which.max)
  matrix(levels[pred],nrow=nrow(pred))
}
train.model <- function(sf,dtm,y){
  sf <- as.matrix(sf)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  fit <- glmnet(dtm,y,alpha=0.8,nlambda=100,family="gaussian")
  lambda <- fit$lambda
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    pred <- train.and.predict(sf[-omit,,drop=FALSE],dtm[-omit,,drop=FALSE],y[-omit],sf[omit,,drop=FALSE],dtm[omit,,drop=FALSE],lambda=lambda)
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
  })
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  s <- lambda[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  dist <- train.multi.NB.normal(cbind(sf,predict(fit,dtm,s=s)),y)
  model <- list(fit=fit,s=s,dist=dist)
  return(list(model=model,kappa=kappa))
}
apply.model <- function(model,sf,dtm){
  sf <- as.matrix(sf)
  X <- cbind(sf,predict(model$fit,dtm,s=model$s))
  apply.NB.normal(model$dist,X)
}
