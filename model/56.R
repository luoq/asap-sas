comment="glmnet on 1-3-gram with bintf,no standardize"
require(Metrics)
require(glmnet)
source('general/util.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
apply.model <- function(model,X){
  pred <- predict(model$fit,X,s=model$s)
  as.vector(round.range(pred,model$yrange[1],model$yrange[2]))
}
train.model <- function(X,y){
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  fit <- glmnet(X,y,alpha=0.8,nlambda=10,standardize=FALSE,family="gaussian")
  lambda <- fit$lambda
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],lambda=lambda,alpha=0.8,standardize=FALSE,family="gaussian")
    pred <- predict(fit,X[omit,,drop=FALSE])
    pred <- round.range(pred,yrange[1],yrange[2])
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
  })
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  s <- lambda[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  model <- list(fit=fit,s=s,yrange=yrange)
  return(list(model=model,kappa=kappa))
}
