estimate.conditional.normal.dist <- function(X,y,ZERO=1e-10,logprior=FALSE){
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
train.multi.NB.normal <- function(X,y,ZERO=1e-12)
  estimate.conditional.normal.dist(X,y,ZERO=ZERO,logprior=TRUE)
calc.conditional.normal.dist <- function(model,X,offset=NULL){
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  L <- with(model,{
    n <- nrow(X)
    logP <- sapply(1:length(levels),function(i){
      -(X-outer(rep(1,n),means[i,]))^2/outer(rep(1,n),vars[i,])/2
      # -(X-outer(rep(1,n),means[i,]))^2/outer(rep(1,n),vars[i,])/2-outer(rep(1,n),logsds[i,])
    },simplify="array")
  })
  if(!is.null(offset))
    L <- aperm(outer(offset,rep(1,ncol(X))),c(1,3,2))+L
  L
}
apply.multi.NB.normal <- function(model,X,add.prior=TRUE){
  if(add.prior)
    L <- calc.conditional.normal.dist(model,X,offset=outer(rep(1,nrow(X)),model$logprior))
  else
    L <- calc.conditional.normal.dist(model,X)
  pred <- apply(L,c(1,2),which.max)
  matrix(model$levels[pred],nrow=nrow(pred))
}
apply.NB.normal <- function(model,X,add.prior=TRUE){
  L <- calc.conditional.normal.dist(model,X)
  L <- apply(L,c(1,3),sum)
  if(add.prior)
    L <- L+outer(rep(1,nrow(X)),model$logprior)
  pred <- apply(L,1,which.max)
  model$levels[pred]
}
