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
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  if(add.prior)
    L <- calc.conditional.normal.dist(model,X,offset=outer(rep(1,nrow(X)),model$logprior))
  else
    L <- calc.conditional.normal.dist(model,X)
  pred <- apply(L,c(1,2),which.max)
  if(ncol(pred)==1)
    model$levels[pred]
  else
    matrix(model$levels[pred],nrow=nrow(pred))
}
apply.NB.normal <- function(model,X,add.prior=TRUE){
  if(is.vector(X))
    X <- matrix(X,ncol=1)
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
  if(ncol(pred)==1)
    model$levels[pred]
  else
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
proportional.assignment <- function(x,proportion,levels){
  n <- length(x)
  ord <- order(x)
  p <- cumsum(floor(proportion*n))
  p <- c(0,p)
  for(i in 1:length(levels))
    x[ord[seq(p[i]+1,p[i+1])]] <- levels[i]
  return(x)
}
train.NB.Multinomial <- function(X,y,laplace=1e-3){
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
train.and.predict.multi.NB.Multinomial <- function(X,y,X2,ord,ks,laplace=1e-3){
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

    ##train
    m <- ncol(S)
    ps <- as.matrix(Diagonal(x=1/(rowSums(S)+m*laplace)) %*% (S+laplace))

    ##predict
    Q <- log(ps)
    L <- X2 %*% t(Q)
    L <- L+outer(rep(1,nrow(X2)),logprior)
    pred <- levels[apply(L,1,which.max)]
  })
}
train.cv.NB.Multinomial <- function(X,y,
                                    cv.ctrl=list(K=10,split="random",max.measure="kappa"),
                                    nb.ctrl=list(weight.fun=informationGainMultinomial,laplace=1e-3)){
  n <- length(y)
  all.folds <- if(cv.ctrl$split=="random")
    all.folds <-cv.kfold.random(n,cv.ctrl$K)
  else if(cv.ctrl$split=="sequential")
    cv.kfold.sequential(n,K)
  else if(cv.ctrl$split=="stratified")
    cv.kfold.stratified.random(y,cv.ctrl$K)
  else
    stop("no such split method")
  
  ks <- square.split(ncol(X),100)
  temp <- lapply(1:cv.ctrl$K,function(k){
    omit <- all.folds[[k]]
    w <- nb.ctrl$weight.fun(y[-omit],X[-omit,,drop=FALSE])
    ord <- order(w,decreasing=TRUE)
    pred <- train.and.predict.multi.NB.Multinomial(X[-omit,,drop=FALSE],y[-omit],X[omit,,drop=FALSE],ord,ks,laplace=nb.ctrl$laplace)
    prec <- apply(pred,2,function(pred)
                  precision(pred,y[omit]))
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
    list(kappa=kappa,prec=prec)
  })
  kappa <- sapply(temp,function(x) x$kappa)
  prec <- sapply(temp,function(x) x$prec)

  mean.prec <- apply(prec,1,mean)
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- if(cv.ctrl$max.measure=="kappa")
    which.max(mean.kappa)
  else if(cv.ctrl$max.measure=="precision")
    which.max(mean.prec)
  else
    stop("no such measure")
  kappa <- c(kappa[i,],mean.kappa[i])
  prec <- c(prec[i,],mean.prec[i])
  names(kappa) <- c(sapply(as.character(1:cv.ctrl$K),function(x) paste("fold",x,sep="")),"mean")
  names(prec) <- c(sapply(as.character(1:cv.ctrl$K),function(x) paste("fold",x,sep="")),"mean")

  w <- nb.ctrl$weight.fun(y,X)
  ord <- order(w,decreasing=TRUE)
  subset <- ord[1:ks[i]]

  nb <- train.NB.Multinomial(X[,subset,drop=FALSE],y,laplace=nb.ctrl$laplace)
  model <- list(subset=subset,nb=nb)
  return(list(model=model,kappa=kappa,prec=prec))
}
apply.NB.Multinomial <- function(model,X){
  X <- X[,model$subset,drop=FALSE]
  predict.NB.Multinomial(model$nb,X)
}
train.cv.glmnet.with.calibrator <- function(X,y,
                                    cv.ctrl=list(K=10,split="random",max.measure="kappa"),
                                    calibrator_type="nb",
                                    glmnet.ctrl=list(alpha=0.8,nlambda=100,standardize=FALSE)){
  levels <- as.numeric(levels(as.factor(y)))
  n <- length(y)
  all.folds <- if(cv.ctrl$split=="random")
    cv.kfold.random(n,cv.ctrl$K)
  else if(cv.ctrl$split=="sequential")
    cv.kfold.sequential(n,cv.ctrl$K)
  else if(cv.ctrl$split=="stratified")
    cv.kfold.stratified.random(y,cv.ctrl$K)
  else
    stop("no such split method")  
  
  fit <- glmnet(X,y,alpha=glmnet.ctrl$alpha,nlambda=glmnet.ctrl$nlambda,standardize=glmnet.ctrl$standardize,family="gaussian")
  lambda <- fit$lambda
  temp <- lapply(1:cv.ctrl$K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit],lambda=lambda,alpha=glmnet.ctrl$alpha,standardize=glmnet.ctrl$standardize,family="gaussian")
    pred <- predict(fit,X[omit,,drop=FALSE])
    if(calibrator_type=="nb"){
      nb <- train.multi.NB.normal(predict(fit,X[-omit,,drop=FALSE]),y[-omit])
      pred <- apply.multi.NB.normal(nb,pred)
    }
    else if(calibrator_type=="pa"){
      proportion <- prop.table(table(y[-omit]))
      pred <- apply(pred,2,function(x) proportional.assignment(x,proportion,levels))
    }
    else
      stop("no such calibrator")
    prec <- apply(pred,2,function(pred)
                  precision(pred,y[omit]))
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit]))
    list(kappa=kappa,prec=prec)
  })
  kappa <- sapply(temp,function(x) x$kappa)
  prec <- sapply(temp,function(x) x$prec)

  mean.prec <- apply(prec,1,mean)
  mean.kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  i <- if(cv.ctrl$max.measure=="kappa")
    which.max(mean.kappa)
  else if(cv.ctrl$max.measure=="precision")
    which.max(mean.prec)
  else
    stop("no such calibrator")
  s <- lambda[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  prec <- c(prec[i,],mean.prec[i])
  names(kappa) <- c(sapply(as.character(1:cv.ctrl$K),function(x) paste("fold",x,sep="")),"mean")
  names(prec) <- c(sapply(as.character(1:cv.ctrl$K),function(x) paste("fold",x,sep="")),"mean")
  
  calibrator <- if(calibrator_type=="nb")
    train.multi.NB.normal(predict(fit,X,s=s),y)
  else if(calibrator_type=="pa")
    prop.table(table(y))
  else
    stop("no such calibrator")
  model <- list(levels=levels,fit=fit,s=s,calibrator_type=calibrator_type,calibrator=calibrator)
  return(list(model=model,kappa=kappa,prec=prec))
}
apply.glmnet.with.calibrator <- function(model,X){
  pred <- predict(model$fit,X,s=model$s)
  if(model$calibrator_type=="nb")
    apply.multi.NB.normal(model$calibrator,pred)
  else if(model$calibrator_type=="pa")
    proportional.assignment(pred,model$calibrator,model$levels)
}
