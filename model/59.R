comment="multi glmnet binomial on 1-3-gram with bintf,no standardize"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.spliter <- function(X,y,levels,lambda){
  fit <- lapply(1:length(lambda),function(i){
    mask <- y>=levels[i]
    glmnet(X[mask,],as.factor(y[mask]==levels[i]),alpha=0.8,lambda=lambda[[i]],standardize=FALSE,family="binomial")
  })
}
apply.model <- function(model,X){
  pred <- sapply(1:length(model$s),function(i)
                predict(model$fit[[i]],X,s=model$s[i],type="class"))
  model$levels[first.true.index(pred=="TRUE")]
}
train.model <- function(X,y){
  levels <- as.numeric(levels(as.factor(y)))

  temp <- lapply(levels[1:(length(levels)-1)],function(i){
    mask <- y>=i
    cv.fit <- cv.glmnet(X[mask,],as.factor(y[mask]==i),alpha=0.8,nlambda=10,standardize=FALSE,family="binomial",nfolds=5,type.measure="auc")
    list(lambda=cv.fit$lambda,fit=cv.fit$glmnet.fit,s=cv.fit$lambda.min)
  })
  lambda <- lapply(temp,function(x) x$lambda)
  fit <- lapply(temp,function(x) x$fit)
  s <- sapply(temp,function(x) x$s)
  rm(temp)
    
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- train.spliter(X[-omit,,drop=FALSE],y[-omit],levels,lambda)
    model <- list(levels=levels,fit=fit,s=s)
    pred <- apply.model(model,X[omit,,drop=FALSE])
    ScoreQuadraticWeightedKappa(pred,y[omit],min(y),max(y))
  })
  kappa <- append(kappa,MeanQuadraticWeightedKappa(kappa))
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")

  model <- list(levels=levels,fit=fit,s=s)
  return(list(model=model,kappa=kappa))
}
