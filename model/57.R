comment="glmnet on 1-3-gram with bintf,proportional assignment,no standardize"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y){
  levels <- as.numeric(levels(as.factor(y)))
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  fit <- glmnet(X,y,alpha=0.8,nlambda=10,standardize=FALSE,family="gaussian")
  lambda <- fit$lambda
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],lambda=lambda,alpha=0.8,standardize=FALSE,family="gaussian")
    pred <- predict(fit,X[omit,,drop=FALSE])
    proportion <- prop.table(table(y[-omit]))
    pred <- apply(pred,2,function(x) proportional.assignment(x,proportion,levels))
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
  })
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  s <- lambda[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")

  model <- list(fit=fit,s=s,levels=levels,proportion=prop.table(table(y)))
  return(list(model=model,kappa=kappa))
}
apply.model <- function(model,X){
  pred <- predict(model$fit,X,s=model$s)
  proportional.assignment(pred,model$proportion,model$levels)
}
