comment="random forest on simple features and dtm selected by lasso"
require(Metrics)
require(glmnet)
require(randomForest)
source('general/util.R')
used_feature <- c(simple=TRUE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
apply.model <- function(model,sf,dtm){
  X <- cBind(as.matrix(sf),dtm)
  X <- as.matrix(X[,model$mask])
  apply.model.2(model$rf,X)
}
train.model.2 <- function(X,y){
  randomForest(X,as.factor(y))
}
apply.model.2 <- function(model,X){
  factor2numeric(predict(model,X))
}
train.model <- function(sf,dtm,y){
  X <- cBind(as.matrix(sf),dtm)
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  index <- seq(0,1,length=100)
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],alpha=1,family="gaussian")
    pred <- predict(fit,X[omit,,drop=FALSE],s=index)
    pred <- round.range(pred,yrange[1],yrange[2])
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
  })
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  s <- index[i]
  fit <- glmnet(X,y,alpha=1,family="gaussian")
  mask <- (as.vector(coef(fit,s=s))!=0)
  mask <- mask[2:length(mask)]

  X <- as.matrix(X[,mask])
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    model <- train.model.2(X[-omit,,drop=FALSE],y[-omit])
    pred <- apply.model.2(model,X[omit,,drop=FALSE])
    ScoreQuadraticWeightedKappa(pred,y[omit],min(y),max(y))
  })
  kappa <- append(kappa,MeanQuadraticWeightedKappa(kappa))
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  model <- list(mask=mask,rf=train.model.2(X,y))
  return(list(model=model,kappa=kappa))
}
