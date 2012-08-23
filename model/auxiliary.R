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
apply.multi.NB.normal <- function(model,X,add.prior=TRUE,type="class"){
  if(is.vector(X))
    X <- matrix(X,ncol=1)
  if(add.prior)
    L <- calc.conditional.normal.dist(model,X,offset=outer(rep(1,nrow(X)),model$logprior))
  else
    L <- calc.conditional.normal.dist(model,X)
  if(type=="class"){
    pred <- apply(L,c(1,2),which.max)
    if(ncol(pred)==1)
      model$levels[pred]
    else
      matrix(model$levels[pred],nrow=nrow(pred))
  }
  else if(type=="probability")
    L.to.P.2(L)
  else if(type=="min.square.loss"){
    nc <- length(model$levels)
    d <- dim(L)
    dim(L) <- c(d[1]*d[2],d[3])
    P <- L.to.P(L)
    Loss <- P %*% outer(1:nc,1:nc,function(x,y) (x-y)^2)
    dim(Loss) <- d
    pred <- apply(Loss,c(1,2),which.min)
    if(ncol(pred)==1)
      model$levels[pred]
    else
      matrix(model$levels[pred],nrow=nrow(pred))
  }
  else
    stop("no such return type")
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
  Q <- log(ps)
  return(list(levels=as.numeric(levels(y)),logprior=logprior,Q=Q))
}
L.to.P <- function(L){
  P <- t(apply(L,1,function(x) x-mean(x)))
  P <- exp(P)
  P <- Diagonal(x=1/rowSums(P)) %*% P
  unname(as.matrix(P))
}
predict.NB.Multinomial <- function(model,X,type="class"){
  L <- X %*% t(model$Q)
  L <- L+outer(rep(1,nrow(X)),model$logprior)
  if(type=="class")
    model$levels[apply(L,1,which.max)]
  else if(type=="probability")
    L.to.P(L)
  else if(type=="min.square.loss"){
    nc <- length(model$levels)
    P <- L.to.P(L)
    Loss <- P %*% outer(1:nc,1:nc,function(x,y) (x-y)^2)
    model$levels[apply(L,1,which.min)]
  }
  else
    stop("no such return type")
}
train.and.predict.multi.NB.Multinomial <- function(X,y,X2,ord,ks,laplace=1e-3,criterion="max.probability"){
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
    Q <- log(ps)

    ##predict
    L <- X2 %*% t(Q)
    L <- L+outer(rep(1,nrow(X2)),logprior)
    pred <- if(criterion=="max.probability")
      levels[apply(L,1,which.max)]
    else if(criterion=="min.square.loss"){
      nc <- length(levels)
      P <- L.to.P(L)
      Loss <- P %*% outer(1:nc,1:nc,function(x,y) (x-y)^2)
      levels[apply(L,1,which.min)]
    }
    else
      stop("no such criterion")
  })
}
train.cv.NB.Multinomial <- function(X,y,
                                    cv.ctrl=list(K=10,split="random",max.measure="kappa"),
                                    nb.ctrl=list(weight.fun=informationGainMultinomial,laplace=1e-3,criterion="max.probability")){
  n <- length(y)
  all.folds <- if(cv.ctrl$split=="random")
    all.folds <-cv.kfold.random(n,cv.ctrl$K)
  else if(cv.ctrl$split=="sequential")
    cv.kfold.sequential(n,K)
  else if(cv.ctrl$split=="stratified")
    cv.kfold.stratified.random(y,cv.ctrl$K)
  else
    stop("no such split method")
  if(is.null(nb.ctrl$criterion))
    nb.ctrl$criterion="max.probability"

  ks <- square.split(ncol(X),100)
  temp <- lapply(1:cv.ctrl$K,function(k){
    omit <- all.folds[[k]]
    w <- nb.ctrl$weight.fun(y[-omit],X[-omit,,drop=FALSE])
    ord <- order(w,decreasing=TRUE)
    pred <- train.and.predict.multi.NB.Multinomial(X[-omit,,drop=FALSE],y[-omit],X[omit,,drop=FALSE],ord,ks,
                                                   laplace=nb.ctrl$laplace,criterion=nb.ctrl$criterion)
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
  model <- list(subset=subset,nb=nb,criterion=nb.ctrl$criterion)
  return(list(model=model,kappa=kappa,prec=prec))
}
apply.NB.Multinomial <- function(model,X,output.probability=FALSE){
  X <- X[,model$subset,drop=FALSE]
  if(output.probability)
    predict.NB.Multinomial(model$nb,X,type="probability")
  else if(model$criterion=="max.probability")
    predict.NB.Multinomial(model$nb,X)
  else if(model$criterion=="min.square.loss")
    predict.NB.Multinomial(model$nb,X,type=model$criterion)
  else
    stop("No such criterion")
}
train.cv.glmnet.with.calibrator <- function(X,y,
                                            transformer1=function(X,y) list(X=X,y=y,transformer2=identity),
                                            cv.ctrl=list(K=10,split="random",max.measure="kappa"),
                                            calibrator_type="nb",calibrator.ctrl=list(on.new=FALSE,new.fraction=0.5,refit=TRUE),
                                            glmnet.ctrl=list(alpha=0.8,nlambda=100,standardize=FALSE),
                                            nb.ctrl=list(criterion="max.probability")){
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

  temp <- transformer1(X,y)
  XX <- temp$X
  yy <- temp$y
  fit <- glmnet(XX,yy,alpha=glmnet.ctrl$alpha,nlambda=glmnet.ctrl$nlambda,standardize=glmnet.ctrl$standardize,family="gaussian")
  transformer2 <- temp$transformer2
  lambda <- fit$lambda
  temp <- lapply(1:cv.ctrl$K,function(k){
    omit <- all.folds[[k]]
    X1 <- X[-omit,,drop=FALSE]
    y1 <- y[-omit]
    X2 <- X[omit,,drop=FALSE]
    y2 <- y[omit]
    temp <- transformer1(X1,y1)
    XX1 <- temp$X
    yy1 <- temp$y
    transformer2 <- temp$transformer2
    XX2 <- transformer2(X2)
    if(calibrator_type=="nb"){
      if(calibrator.ctrl$on.new){
        mask <- sample(nrow(XX1),nrow(XX1)*(1-calibrator.ctrl$new.fraction))
        XX1.1 <- XX1[mask,,drop=FALSE]
        yy1.1 <- yy1[mask]
        XX1.2 <- XX1[-mask,,drop=FALSE]
        yy1.2 <- yy1[-mask]
        fit.1 <- glmnet(XX1.1,yy1.1,lambda=lambda,alpha=glmnet.ctrl$alpha,standardize=glmnet.ctrl$standardize,family="gaussian")
        nb <- train.multi.NB.normal(predict(fit.1,XX1.2),yy1.2)
        fit <- if(calibrator.ctrl$refit)
          glmnet(XX1,yy1,lambda=lambda,alpha=glmnet.ctrl$alpha,standardize=glmnet.ctrl$standardize,family="gaussian")
        else
          fit.1
      }
      else{
        fit <- glmnet(XX1,yy1,lambda=lambda,alpha=glmnet.ctrl$alpha,standardize=glmnet.ctrl$standardize,family="gaussian")
        nb <- train.multi.NB.normal(predict(fit,XX1),yy1)
      }
    }
    else
      fit <- glmnet(XX1,yy1,lambda=lambda,alpha=glmnet.ctrl$alpha,standardize=glmnet.ctrl$standardize,family="gaussian")
    pred <- predict(fit,XX2)
    if(calibrator_type=="nb"){
      pred <- if(nb.ctrl$criterion=="min.square.loss")
        apply.multi.NB.normal(nb,pred,type=nb.ctrl$criterion)
      else
        apply.multi.NB.normal(nb,pred)
    }
    else if(calibrator_type=="pa"){
      proportion <- prop.table(table(yy1))
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
    stop("no such measure")
  s <- lambda[i]
  kappa <- c(kappa[i,],mean.kappa[i])
  prec <- c(prec[i,],mean.prec[i])
  names(kappa) <- c(sapply(as.character(1:cv.ctrl$K),function(x) paste("fold",x,sep="")),"mean")
  names(prec) <- c(sapply(as.character(1:cv.ctrl$K),function(x) paste("fold",x,sep="")),"mean")

  calibrator <- if(calibrator_type=="nb")
    if(calibrator.ctrl$on.new){
      mask <- sample(nrow(XX),nrow(XX)*(1-calibrator.ctrl$new.fraction))
      XX.1 <- XX[mask,,drop=FALSE]
      yy.1 <- yy[mask]
      XX.2 <- XX[-mask,,drop=FALSE]
      yy.2 <- yy[-mask]
      fit.1 <- glmnet(XX.1,yy.1,lambda=lambda,alpha=glmnet.ctrl$alpha,standardize=glmnet.ctrl$standardize,family="gaussian")
      if(!calibrator.ctrl$refit)
        fit <- fit.1
      train.multi.NB.normal(predict(fit.1,XX.2,s=s),yy.2)
    }
    else
      train.multi.NB.normal(predict(fit,XX,s=s),yy)
  else if(calibrator_type=="pa")
    prop.table(table(y))
  else
    stop("no such calibrator")
  model <- list(levels=levels,transformer2=transformer2,fit=fit,s=s,calibrator_type=calibrator_type,calibrator=calibrator,criterion=nb.ctrl$criterion)
  return(list(model=model,kappa=kappa,prec=prec))
}
apply.glmnet.with.calibrator <- function(model,X){
  XX <- model$transformer2(X)
  pred <- predict(model$fit,XX,s=model$s)
  if(model$calibrator_type=="nb")
    if(model$criterion=="min.square.loss")
      apply.multi.NB.normal(model$calibrator,pred,type=model$criterion)
    else
      apply.multi.NB.normal(model$calibrator,pred)
  else if(model$calibrator_type=="pa")
    proportional.assignment(pred,model$calibrator,model$levels)
}
nbm.all.transformer <- function(X,y,laplace=1e-3,weight.fun=informationGainMultinomial,output.probability=FALSE){
  levels <- as.numeric(levels(as.factor(y)))
  ks <- square.split(ncol(X),100)
  train.nbs <- function(X,y){
    w <- weight.fun(y,X)
    ord <- order(w,decreasing=TRUE)
    y <- as.factor(y)
    levels=as.numeric(levels(y))
    MASK <- t(sapply(levels(y),function(i) y==i)*1)
    logprior <- log(prop.table(rowSums(MASK)))
    S <- MASK %*% X
    lapply(ks,function(k){
      subset <- ord[1:k]
      S <- S[,subset,drop=FALSE]
      m <- ncol(S)
      ps <- as.matrix(Diagonal(x=1/(rowSums(S)+m*laplace)) %*% (S+laplace))
      Q <- log(ps)
      nb <- list(levels=levels,logprior=logprior,Q=Q)
      list(subset=subset,nb=nb,criterion="max.probability")
    })
  }
  models <- lapply(levels[1:(length(levels)-1)],function(i)
                   train.nbs(X,1*(y<=i)))
  models <- do.call(c,models)
  nb.features <- if(output.probability)
    sapply(models,function(model) apply.NB.Multinomial(model,X,output.probability=TRUE)[,1])
  else
    sapply(models,function(model) apply.NB.Multinomial(model,X))
  colnames(nb.features) <- sapply(as.character(1:ncol(nb.features)),function(x) paste("nb.",x,sep=""))
  mask <- apply(nb.features,2,sd)>1e-10
  models <- models[mask]
  nb.features <- nb.features[,mask]
  nb.names <- colnames(nb.features)
  transformer2 <- function(X){
    nb.features <- if(output.probability)
      sapply(models,function(model) apply.NB.Multinomial(model,X,output.probability=TRUE)[,1])
    else
      sapply(models,function(model) apply.NB.Multinomial(model,X))
    colnames(nb.features) <- nb.names
    cBind(nb.features,1*(X!=0))
  }
  list(X=cBind(nb.features,1*(X!=0)),y=y,transformer2=transformer2)
}
train.cv.glmnet.with.selected.nbms <- function(X,y,
                                               fit.on.test=FALSE,
                                               cv.ctrl=list(K=10,split="random",max.measure="kappa"),
                                               nb.ctrl=list(weight.fun=informationGainMultinomial,laplace=1e-3,output.probability=FALSE),
                                               glmnet.ctrl=list(alpha=0.8,nlambda=100,standardize=FALSE),
                                               calibrator_type="nb"){
  levels <- as.numeric(levels(as.factor(y)))
  select.k <- function(X,y){
    temp <- train.cv.NB.Multinomial(X,y,
                                    cv.ctrl=list(K=10,split="random",max.measure="precision"),
                                    nb.ctrl=list(weight.fun=nb.ctrl$weight.fun,laplace=nb.ctrl$laplace))
    length(temp$model$subset)
  }
  ks <- sapply(levels[1:(length(levels)-1)],function(i) select.k(X,1*(y<=i)))

  transformer1 <- function(X,y){
    if(fit.on.test){
      n <- nrow(X)
      index <- sample(n,ceiling(n*0.3))
      X2 <- X[-index,,drop=FALSE]
      y2 <- y[-index]
      X <- X[index,,drop=FALSE]
      y <- y[index]
    }
    nbs <- lapply(1:length(ks),function(i){
      y <- 1*(y<=levels[i])
      w <- nb.ctrl$weight.fun(y,X)
      ord <- order(w,decreasing=TRUE)
      subset <- ord[1:ks[i]]
      X <- X[,subset,drop=FALSE]
      nb <- train.NB.Multinomial(X,y,laplace=nb.ctrl$laplace)
      list(nb=nb,subset=subset,criterion="max.probability")
    })
    transformer2 <- function(X){
      nb.features <- if(nb.ctrl$output.probability)
        sapply(nbs,function(model) apply.NB.Multinomial(model,X,output.probability=TRUE)[,1])
      else
        sapply(nbs,function(model) apply.NB.Multinomial(model,X))
      colnames(nb.features) <- sapply(as.character(1:ncol(nb.features)),function(x) paste("nb.",x,sep=""))
      cBind(nb.features,1*(X!=0))
    }
    if(fit.on.test)
      list(X=transformer2(X2),y=y2,transformer2=transformer2)
    else
      list(X=transformer2(X),y=y,transformer2=transformer2)
  }
  train.cv.glmnet.with.calibrator(X,y,
                                  transformer1=transformer1,
                                  cv.ctrl=cv.ctrl,glmnet.ctrl=glmnet.ctrl,calibrator_type=calibrator_type)
}
