comment="NB.Multinomial on 1-3-gram,feature selection by information gain"
require(Metrics)
source('general/util.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
train.NB.Multinomial <- function(X,y,laplace=1e-4){
  y <- as.factor(y)
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  MASK <- t(sapply(levels(y),function(i) y==i)*1)
  logprior <- log(prop.table(rowSums(MASK)))
  S <- MASK %*% X
  m <- ncol(X)
  ps <- as.matrix(Diagonal(x=1/(rowSums(S)+m*laplace)) %*% (S+laplace))
  return(list(levels=as.numeric(levels(y)),logprior=logprior,ps=ps))
}
predict.NB.Multinomial <- function(model,X){
  Q <- log(model$ps)
  L <- X %*% t(Q)
  L <- L+outer(rep(1,nrow(X)),model$logprior)
  model$levels[apply(L,1,which.max)]
}
train.and.predict <- function(X,y,X2,ord,ks,laplace=1e-4){
  y <- as.factor(y)
  levels=as.numeric(levels(y))
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  MASK <- t(sapply(levels(y),function(i) y==i)*1)
  logprior <- log(prop.table(rowSums(MASK)))
  S <- MASK %*% X
  sapply(ks,function(k){
    subset <- ord[1:k]
    S <- S[,subset,drop=FALSE]
    X2 <- X2[,subset,drop=FALSE]

    #train
    m <- ncol(S)
    ps <- as.matrix(Diagonal(x=1/(rowSums(S)+m*laplace)) %*% (S+laplace))
    
    #predict
    Q <- log(ps)
    L <- X2 %*% t(Q)
    L <- L+outer(rep(1,nrow(X2)),logprior)
    pred <- levels[apply(L,1,which.max)]
  })
}
train.model <- function(X,y){
  w <- informationGainMultinomial(y,X,laplace=1e-4)
  ord <- order(w,decreasing=TRUE)
  ks <- square.split(length(w),30)
  
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    nb <- train.NB.Multinomial(X[-omit,,drop=FALSE],y[-omit])
    pred <- predict.NB.Multinomial(nb,X[omit,,drop=FALSE])
    pred <- train.and.predict(X[-omit,,drop=FALSE],y[-omit],X[omit,,drop=FALSE],ord,ks)
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
  })
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- which.max(mean.kappa)
  subset <- ord[1:ks[i]]
  kappa <- c(kappa[i,],mean.kappa[i])
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  nb <- train.NB.Multinomial(X[,subset,drop=FALSE],y)
  model <- list(subset=subset,nb=nb)
  return(list(model=model,kappa=kappa))
}
apply.model <- function(model,X){
  X <- X[,model$subset,drop=FALSE]
  predict.NB.Multinomial(model$nb,X)
}
