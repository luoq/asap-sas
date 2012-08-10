## LM_step on simple features
require(Metrics)
require(MASS)
source('general/util.R')
use_simple_feature=TRUE
used_feature <- c(simple=TRUE,dtm=FALSE,corpus=FALSE)
train.model.2 <- function(X,y){
  fit <- lm(y~.,data=cbind(y=y,as.data.frame(X)))
  fit <- stepAIC(fit,trace=0)
  return(list(fit=fit,yrange=range(y)))
}
apply.model <- function(model,sf){
  res <- unname(predict(model$fit,as.data.frame(sf)))
  round.range(res,model$yrange[1],model$yrange[2])
}
train.model <- function(sf,y){
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    model <- train.model.2(sf[-omit,,drop=FALSE],y[-omit])
    pred <- apply.model(model,sf[omit,,drop=FALSE])
    ScoreQuadraticWeightedKappa(pred,y[omit],min(y),max(y))
  })
  kappa <- append(kappa,MeanQuadraticWeightedKappa(kappa))
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  model <- train.model.2(sf,y)
  return(list(model=model,kappa=kappa))
}
