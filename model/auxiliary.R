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
  sds <- sqrt(vars)
  logsds <- log(sds)
  if(logprior)
    return(list(levels=as.numeric(levels(y)),logprior=log(prop.table(ns)),means=means,vars=vars,sds=sds,logsds=logsds))
  else
    return(list(levels=as.numeric(levels(y)),means=means,vars=vars,logsds=lodsds))
}
train.parallel.NB <- function(X,y,ZERO=1e-12)
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
apply.parallel.NB <- function(model,X){
  L <- calc.conditional.normal.dist(model,X,offset=outer(rep(1,nrow(X)),model$logprior))
  pred <- apply(L,c(1,2),which.max)
  matrix(model$levels[pred],nrow=nrow(pred))
}
