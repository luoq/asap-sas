comment="glmnet calibrated by NB on 1-3-bingram and NBM probability"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
LAPLACE <- 1e-3
get_nbs <- function(X,y,subsets,levels,laplace)
  lapply(1:length(subsets),function(i){
    subset <- subsets[[i]]
    y <- 1*(y<=levels[i])
    X <- X[,subset,drop=FALSE]
    train.NB.Multinomial(X,y,laplace=laplace)
  })
get_nb.features <- function(nbs,subsets,X)
  sapply(1:length(nbs),function(i){
    subset <- subsets[[i]]
    nb <- nbs[[i]]
    X <- X[,subset,drop=FALSE]
    predict.NB.Multinomial(nb,X,type="probability")[,1]
  })
train.model <- function(X,y,
                        cv.ctrl=list(K=5,split="random",max.measure="kappa"),
                        calibrator_type="nb",
                        glmnet.ctrl=list(alpha=0.8,nlambda=100,standardize=FALSE),
                        nb.ctrl=list(weight.fun=informationGainMultinomial,laplace=1e-3)){
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

  select.subset <- function(X,y){
    temp <- train.cv.NB.Multinomial(X,y,
                                    cv.ctrl=list(K=5,split="random",max.measure="precision"),
                                    nb.ctrl=list(weight.fun=informationGainMultinomial,laplace=1e-3))
    temp$model$subset
  }
  levels.init <- levels[1:(length(levels)-1)]
  subsets <- lapply(levels.init,function(i) select.subset(X,1*(y<=i)))

  nbs <- get_nbs(X,y,subsets,levels,laplace=nb.ctrl$laplace)
  nb.features <- get_nb.features(nbs,subsets,X)

  XX <- cBind(nb.features,X)
  fit <- glmnet(XX,y,alpha=glmnet.ctrl$alpha,nlambda=glmnet.ctrl$nlambda,standardize=glmnet.ctrl$standardize,family="gaussian")
  lambda <- fit$lambda
  temp <- lapply(1:cv.ctrl$K,function(k){
    omit <- all.folds[[k]]
    X1 <- X[-omit,,drop=FALSE]
    X2 <- X[omit,,drop=FALSE]
    y1 <- y[-omit]
    y2 <- y[omit]

    nbs <- get_nbs(X1,y1,subsets,levels,laplace=nb.ctrl$laplace)
    nb.features1 <- get_nb.features(nbs,subsets,X1)
    nb.features2 <- get_nb.features(nbs,subsets,X2)
    XX1 <- cBind(nb.features1,X1)
    XX2 <- cBind(nb.features2,X2)
    
    fit <- glmnet(XX1,y1,lambda=lambda,alpha=glmnet.ctrl$alpha,standardize=glmnet.ctrl$standardize,family="gaussian")
    pred2 <- predict(fit,XX2)
    if(calibrator_type=="nb"){
      pred1 <- predict(fit,XX1)
      nb <- train.multi.NB.normal(pred1,y1)
      pred <- apply.multi.NB.normal(nb,pred2)
    }
    else if(calibrator_type=="pa"){
      proportion <- prop.table(table(y1))
      pred <- apply(pred2,2,function(x) proportional.assignment(x,proportion,levels))
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
    train.multi.NB.normal(predict(fit,XX,s=s),y)
  else if(calibrator_type=="pa")
    prop.table(table(y))
  else
    stop("no such calibrator")
  model <- list(levels=levels,subsets=subsets,nbs=nbs,fit=fit,s=s,calibrator_type=calibrator_type,calibrator=calibrator)
  return(list(model=model,kappa=kappa,prec=prec))
}
apply.model <- function(model,X){
  nb.features <- get_nb.features(model$nbs,model$subsets,X)
  XX <- cBind(nb.features,X)
  pred <- predict(model$fit,XX,s=model$s)
  if(model$calibrator_type=="nb")
    apply.multi.NB.normal(model$calibrator,pred)
  else if(model$calibrator_type=="pa")
    proportional.assignment(pred,model$calibrator,model$levels)
}
