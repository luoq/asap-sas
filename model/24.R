comment="NB.Multinomial on 1-gram,laplace=1"
require(Metrics)
source('general/util.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=1,local_weight="tf",term_weight=NULL)
LAPLACE <- 1
train.NB.Multinomial <- function(X,y,laplace=LAPLACE){
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
train.model <- function(X,y){
  yrange <- range(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    nb <- train.NB.Multinomial(X[-omit,,drop=FALSE],y[-omit])
    pred <- predict.NB.Multinomial(nb,X[omit,,drop=FALSE])
    ScoreQuadraticWeightedKappa(pred,y[omit])
  })
  kappa <- append(kappa,MeanQuadraticWeightedKappa(kappa))
  names(kappa) <- c(sapply(as.character(1:K),function(x) paste("fold",x,sep="")),"mean")
  
  nb <- train.NB.Multinomial(X,y)
  return(list(model=nb,kappa=kappa))
}
apply.model <- predict.NB.Multinomial
