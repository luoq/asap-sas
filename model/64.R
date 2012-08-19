comment="C-svc on 1-gram,bintf"
require(Metrics)
require(e1071)
source('general/util.R')
source('model/auxiliary.R')
return_each_fold_kappa <- FALSE
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=1,local_weight="bintf",term_weight=NULL)
apply.model <- function(model,X)
  factor2numeric(predict(model,X))
train.model <- function(X,y){
  y.numeric <- y
  y <- as.factor(y)
  K <- 5
  n <- length(y)
  all.folds <- split(1:n,rep(1:K,length=n))

  estimate.kappa <- function(c){
    kappa <- sapply(1:K,function(k){
      omit <- all.folds[[k]]
      fit <- try(svm(X[-omit,,drop=FALSE],y[-omit],scale=FALSE,type="C-classification",kernel="linear",cost=c),silent=TRUE)
      if (inherits(fit, "try-error")) return(NaN) #no feasible solution will be catched
      pred <- factor2numeric(predict(fit,X[omit,,drop=FALSE]))
      ScoreQuadraticWeightedKappa(pred,y.numeric[omit])
    })
    if(any(is.nan(kappa)))
      return(-2)
    MeanQuadraticWeightedKappa(kappa)
  }

  cs <- 10^(-4:4)
  kappas <- sapply(cs,estimate.kappa)
  names(kappas) <- cs
  print(kappas)
  i <- which.max(kappas)
  c <- cs[i]
  kappa <- kappas[i]

  fit <- svm(X,y,scale=FALSE,type="C-classification",kernel="linear",cost=c)
  return(list(model=fit,kappa=kappa))
}
