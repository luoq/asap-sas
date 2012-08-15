comment="NB on glmnet path on 1-3-gram,bintf"
require(Metrics)
require(glmnet)
source('general/util.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.NB.normal <- function(X,y,ZERO=1e-10){
  y <- as.factor(y)
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  MASK <- t(sapply(levels(y),function(i) y==i)*1)
  ns <- rowSums(MASK)
  S <- MASK %*% X
  S2 <-MASK %*% (X^2)
  means <- Diagonal(x=1/ns) %*% S
  vars <- Diagonal(x=1/(ns-1)) %*% (S2 - Diagonal(x=ns) %*% (means^2))
  means <- as.matrix(means)
  vars <- as.matrix(vars)
  vars[vars==0] <- ZERO
  return(list(levels=as.numeric(levels(y)),logprior=log(prop.table(ns)),means=means,vars=vars))
}
apply.NB.normal <- function(model,X){
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  with(model,{
    n <- nrow(X)
    L <- sapply(1:length(levels),function(i){
      logprior[i]-(X-outer(rep(1,n),means[i,]))^2/outer(rep(1,n),vars[i,])/2
    },simplify="array")
    L <- apply(L,c(1,3),sum)
    levels[apply(L,1,which.max)]
  })
}
train.model <- function(X,y){
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  fit <- glmnet(X,y,alpha=0.8,nlambda=100,family="gaussian")
  lambda <- fit$lambda
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],lambda=lambda,alpha=0.8,family="gaussian")
    nb <- train.NB.normal(predict(fit,X[-omit,,drop=FALSE]),y[-omit])
    pred <- predict(fit,X[omit,,drop=FALSE])
    pred <- apply.NB.normal(nb,pred)
    ScoreQuadraticWeightedKappa(pred,y[omit])
  })
  kappa <- append(kappa,MeanQuadraticWeightedKappa(kappa))
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  nb <- train.NB.normal(predict(fit,X),y)
  model <- list(fit=fit,nb=nb)
  return(list(model=model,kappa=kappa))
}
apply.model <- function(model,X){
  pred <- predict(model$fit,X)
  apply.NB.normal(model$nb,pred)
}
