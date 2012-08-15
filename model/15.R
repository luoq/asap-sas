comment="glmnet on simple features and 1-3-gram,bintf,calibrated with NB"
require(Metrics)
require(glmnet)
source('general/util.R')
used_feature <- c(simple=TRUE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
source('model/auxiliary.R')
apply.model <- function(model,sf,dtm){
  X <- cBind(as.matrix(sf),dtm)
  pred <- predict(model$fit,X,s=model$s)
  pred <- apply.multi.NB.normal(model$nb,pred)
}
train.model <- function(sf,dtm,y){
  X <- cBind(as.matrix(sf),dtm)
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  fit <- glmnet(X,y,alpha=0.8,nlambda=100,family="gaussian")
  lambda <- fit$lambda
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],lambda=lambda,alpha=0.8,family="gaussian")
    nb <- train.multi.NB.normal(predict(fit,X[-omit,,drop=FALSE]),y[-omit])
    pred <- predict(fit,X[omit,,drop=FALSE])
    pred <- apply.multi.NB.normal(nb,pred)
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
  })
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  s <- lambda[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  nb <- train.multi.NB.normal(predict(fit,X,s=s),y)
  model <- list(fit=fit,s=s,nb=nb)
  return(list(model=model,kappa=kappa))
}
