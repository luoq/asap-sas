estimate.conditional.normal.dist <- function(X,y,ZERO=1e-12,logprior=FALSE){
  y <- as.factor(y)
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  MASK <- t(sapply(levels(y),function(i) y==i)*1)
  ns <- rowSums(MASK)
  S <- MASK %*% X
  S2 <-MASK %*% (X^2)
  means <- Diagonal(x=1/ns) %*% S
  vars <- Diagonal(x=1/(ns-1)) %*% (S2 - Diagonal(x=ns) %*% (means^2))
  means <- as.matrix(means)
  vars <- as.matrix(vars)
  vars[vars<ZERO] <- ZERO
  if(logprior)
    return(list(levels=as.numeric(levels(y)),logprior=log(prop.table(ns)),means=means,vars=vars))
  else
    return(list(levels=as.numeric(levels(y)),means=means,vars=vars))
}
calc.conditional.normal.dist <- function(model,X,offset=NULL){
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  L <- with(model,{
    n <- nrow(X)
    logP <- sapply(1:length(levels),function(i){
      -(X-outer(rep(1,n),means[i,]))^2/outer(rep(1,n),vars[i,])/2
    },simplify="array")
  })
  if(!is.null(offset))
    L <- aperm(outer(offset,rep(1,ncol(X))),c(1,3,2))+L
  L
}
train.NB.normal <- function(X,y,ZERO=1e-12,add.prior=TRUE,multi.model=FALSE){
  model <- estimate.conditional.normal.dist(X,y,ZERO=ZERO,logprior=TRUE)
  model$add.prior <- add.prior
  model$multi.model <- multi.model
  class(model) <- c("NB.normal",class(model))
  model
}
L.to.P <- function(L){
  P <- t(apply(L,1,function(x) x-mean(x)))
  P <- exp(P)
  P <- Diagonal(x=1/rowSums(P)) %*% P
  unname(as.matrix(P))
}
predict.NB.normal <- function(model,X){
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  if(model$multi.model){
    if(model$add.prior)
      L <- calc.conditional.normal.dist(model,X,offset=outer(rep(1,nrow(X)),model$logprior))
    else
      L <- calc.conditional.normal.dist(model,X)
    class <- apply(L,c(1,2),which.max)
    if(ncol(class)==1)
      model$levels[class]
    else
      matrix(model$levels[class],nrow=nrow(class))
    prob <- NULL ## this will not be used
  }
  else{
    L <- calc.conditional.normal.dist(model,X)
    L <- apply(L,c(1,3),sum)
    if(model$add.prior)
      L <- L+outer(rep(1,nrow(X)),model$logprior)
    class <- apply(L,1,which.max)
    model$levels[class]
    prob <- L.to.P(L)
  }
  return(list(class=class,prob=prob))
}
train.NB.Multinomial <- function(X,y,laplace=1e-3,subsets=NULL,ks=NULL,ord=NULL,weight.fun=informationGainMultinomial){
  y <- as.factor(y)
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  MASK <- t(sapply(levels(y),function(i) y==i)*1)
  logprior <- log(prop.table(rowSums(MASK)))
  S <- MASK %*% X
  if(is.null(subsets)&&is.null(ks)){
    m <- ncol(X)
    ps <- as.matrix(Diagonal(x=1/(rowSums(S)+m*laplace)) %*% (S+laplace))
    Q <- log(ps)
    model <- list(levels=as.numeric(levels(y)),logprior=logprior,Q=Q)
    class(model) <- c("NB.Multinomial",class(model))
    return(model)
  }
  else{
    if(is.null(subsets)){
      if(is.null(ord)){
        w <- weight.fun(y,X)
        ord <- order(w,decreasing=TRUE)
      }
      subsets <- lapply(ks,function(k) ord[1:k])
    }
    models <- lapply(subsets,function(subset){
      S <- S[,subset,drop=FALSE]
      X <- X[,subset,drop=FALSE]
      m <- ncol(S)
      ps <- as.matrix(Diagonal(x=1/(rowSums(S)+m*laplace)) %*% (S+laplace))
      Q <- log(ps)
      model <- list(levels=as.numeric(levels(y)),logprior=logprior,Q=Q)
      class(model) <- c("NB.Multinomial",class(model))
      subset.model(model,subset)
    })
    if(length(models)==1)
      return(models[[1]])
    else
      return(models)
  }
}
predict.NB.Multinomial <- function(model,X){
  L <- X %*% t(model$Q)
  L <- L+outer(rep(1,nrow(X)),model$logprior)
  class <- model$levels[apply(L,1,which.max)]
  prob <- L.to.P(L)
  return(list(class=class,prob=prob))
}
# Example: res <- CV.NB.Multinomial.Best.K(X,y,ks=seq(10,1000,by=10))
CV.NB.Multinomial.Best.K <- function(X,y,weight.fun=informationGainMultinomial,ks=NULL,cv.ctrl=NULL){
  if(is.character(weight.fun) && weight.fun=="informationGain2")
    weight.fun <- function(y,X) informationGain2(y,1*(X!=0))
  if(is.null(ks))
    do.call(CV,c(list(X=X,y=y,
                      train.f=function(X,y) train.NB.Multinomial(X,y,weight.fun=weight.fun)),
                      ,cv.ctrl))
  else
    do.call(CV,c(list(X=X,y=y,
                      train.f=function(X,y,ks) train.NB.Multinomial(X,y,weight.fun=weight.fun,ks=ks),parameter=list(ks=ks),
                      multi.models=TRUE,intrinsic.multi.training=TRUE),cv.ctrl))
}
