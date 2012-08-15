comment="glmnet on 1-3-gram,bintf,calibrated with NB,select alpha"
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
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  index <- seq(0,1,length=100)
  alphas <- seq(0,1,by=0.1)
  models <- lapply(alphas,function(alpha){
    kappa <- sapply(1:K,function(k){
      omit <- all.folds[[k]]
      fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],alpha=alpha,family="gaussian")
      nb <- train.multi.NB.normal(predict(fit,X[-omit,,drop=FALSE],s=index),y[-omit])
      pred <- predict(fit,X[omit,,drop=FALSE],s=index)
      pred <- apply.multi.NB.normal(nb,pred)
      kappa <- apply(pred,2,function(pred)
                     ScoreQuadraticWeightedKappa(pred,y[omit]))
    })
    mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
    i <- which.max(mean.kappa)
    s <- index[i]
    kappa <- c(kappa[i,],mean.kappa[i])
    names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
    return(list(s=s,kappa=kappa))
  })
  j <- which.max(sapply(models,function(x) x$kappa["mean"]))
  alpha <- alphas[j]
  kappa <- models[[j]]$kappa
  s <- models[[j]]$s

  fit <- glmnet(X,y,alpha=alpha,family="gaussian")
  nb <- train.multi.NB.normal(predict(fit,X,s=s),y)
  model <- list(alpha=alpha,fit=fit,s=s,nb=nb)
  return(list(model=model,kappa=kappa))
}
