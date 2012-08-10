## glmnet on 1-gram
require(Metrics)
require(glmnet)
source('general/util.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
mingram <- 1
maxgram <- 1
apply.model <- function(model,dtm){
  X <- as.Matrix(dtm)
  pred <- predict(model$fit,X,s=model$s)
  as.vector(round.range(pred,model$yrange[1],model$yrange[2]))
}
train.model <- function(dtm,y){
  X <- as.Matrix(dtm)
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  index <- seq(0,1,length=100)
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],alpha=0.8,family="gaussian")
    pred <- predict(fit,X[omit,,drop=FALSE],s=index)
    pred <- round.range(pred,yrange[1],yrange[2])
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
  })
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  s <- index[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  fit <- glmnet(X,y,alpha=0.8,family="gaussian")
  model <- list(fit=fit,s=s,yrange=yrange)
  return(list(model=model,kappa=kappa))
}
