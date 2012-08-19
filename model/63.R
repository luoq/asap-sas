comment="nu-svc on 1-gram,bintf"
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

  estimate.kappa <- function(nu){
    nu
    kappa <- sapply(1:K,function(k){
      omit <- all.folds[[k]]
      fit <- try(svm(X[-omit,,drop=FALSE],y[-omit],scale=FALSE,type="nu-classification",kernel="linear",nu=nu),silent=TRUE)
      if (inherits(fit, "try-error")) return(NaN) #no feasible solution will be catched
      pred <- factor2numeric(predict(fit,X[omit,,drop=FALSE]))
      ScoreQuadraticWeightedKappa(pred,y.numeric[omit])
    })
    if(any(is.nan(kappa)))
      return(-2)
    MeanQuadraticWeightedKappa(kappa)
  }

  # temp <-fminsearch(function(x) -estimate.kappa(x),logit(0.2))
  # nu <- inv.logit(temp$x[1,1])
  # kappa <- -temp$fval[1]
  nus <- c(0.01,0.02,0.05,0.1,0.2,0.4,0.6,0.8)
  kappas <- sapply(nus,estimate.kappa)
  names(kappas) <- nus
  print(kappas)
  i <- which.max(kappas)
  nu <- nus[i]
  kappa <- kappas[i]

  fit <- svm(X,y,scale=FALSE,type="nu-classification",kernel="linear",nu=nu)
  return(list(model=fit,kappa=kappa))
}
