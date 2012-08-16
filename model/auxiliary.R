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
estimate.conditional.KDE <- function(X,y,logprior=FALSE){
  y <- as.factor(y)
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  dists <- lapply(levels(y),function(i){
    X <- X[y==i,,drop=FALSE]
  })
  res <- list(levels=as.numeric(levels(y)),dists=dists)
  if(logprior)
    res$logprior <- prop.table(table(y))
  return(res)
}
apply.density <- function(x,at,bw = 'nrd0'){
  if (is.character(bw)) {
    if (length(x) < 2)
      stop("need at least 2 points to select a bandwidth automatically")
    bw <- switch(tolower(bw), nrd0 = bw.nrd0(x), nrd = bw.nrd(x),
                 ucv = bw.ucv(x), bcv = bw.bcv(x), sj = , `sj-ste` = bw.SJ(x,
                                                            method = "ste"), `sj-dpi` = bw.SJ(x, method = "dpi"),
                 stop("unknown bandwidth rule"))
  }
  at <- matrix(at, ncol=1)
  p <- sapply(at,function(a) sum(dnorm(a,x,bw)))/length(x)
  return(log(p))
}
calc.conditional.KDE <- function(model,X,offset=NULL){
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  m <- ncol(X)
  L <- sapply(model$dists,function(D)
         sapply(1:m,function(i)
                apply.density(D[,i],X[,i])),simplify="array")
  if(!is.null(offset))
    L <- aperm(outer(offset,rep(1,ncol(X))),c(1,3,2))+L
  L
}
train.multi.NB.KDE <- function(X,y)
  estimate.conditional.KDE(X,y,logprior=TRUE)
apply.multi.NB.KDE <- function(model,X,add.prior=TRUE){
  if(add.prior)
    L <- calc.conditional.KDE(model,X,offset=outer(rep(1,nrow(X)),model$logprior))
  else
    L <- calc.conditional.KDE(model,X)
  pred <- apply(L,c(1,2),which.max)
  matrix(model$levels[pred],nrow=nrow(pred))
}
apply.NB.KDE <- function(model,X,add.prior=TRUE){
  L <- calc.conditional.KDE(model,X)
  L <- apply(L,c(1,3),sum)
  if(add.prior)
    L <- L+outer(rep(1,nrow(X)),model$logprior)
  pred <- apply(L,1,which.max)
  model$levels[pred]
}
