comment="NB.Bernoulli on 1-gram"
require(Metrics)
source('general/util.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=1,local_weight="bintf",term_weight=NULL)
train.NB.Bernoulli <- function(X,y,laplace=LAPLACE){
  y <- as.factor(y)
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  MASK <- t(sapply(levels(y),function(i) y==i)*1)
  ns <- rowSums(MASK)
  logprior <- log(prop.table(ns))
  S <- MASK %*% X
  ps <- as.matrix(Diagonal(x=1/(ns+2*laplace)) %*% (S+laplace))
  Q <- t(log(ps)-log(1-ps))
  return(list(levels=as.numeric(levels(y)),logprior=logprior,Q=Q))
}
predict.NB.Bernoulli <- function(model,X){
  L <- X %*% model$Q
  L <- L+outer(rep(1,nrow(X)),model$logprior)
  model$levels[apply(L,1,which.max)]
}
train.model <- function(X,y){
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    nb <- train.NB.Bernoulli(X[-omit,,drop=FALSE],y[-omit])
    pred <- predict.NB.Bernoulli(nb,X[omit,,drop=FALSE])
    ScoreQuadraticWeightedKappa(pred,y[omit])
  })
  kappa <- append(kappa,MeanQuadraticWeightedKappa(kappa))
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  nb <- train.NB.Bernoulli(X,y)
  return(list(model=nb,kappa=kappa))
}
apply.model <- predict.NB.Bernoulli
