Bagging <- function(X,y,train.classifier,B=25,n=length(y)){
  N <- length(y)
  temp <- replicate(B,{
    mask <- sample(N,n,replace=TRUE)
    classifier <- train.classifier(X[mask,,drop=FALSE],y[mask])
    pred <- rep(NA,N)
    pred[-mask] <- predict(classifier,X[-mask,,drop=FALSE])$class
    list(classifier=classifier,pred=pred)
  },simplify=FALSE)
  classifiers <- lapply(temp,function(x) x$classifier)
  preds <- sapply(temp,function(x) x$pred)

  levels <- as.numeric(levels(as.factor(y)))
  counts <- sapply(levels,function(i) rowSums(preds==i,na.rm=TRUE))
  pred.oob <- levels[apply(counts,1,which.max)]
  kappa <- ScoreQuadraticWeightedKappa(pred.oob,y)
  prec <- precision(pred.oob,y)
  model <- list(levels=levels,classifiers=classifiers)
  class(model) <- c("Bagging",class(model))
  list(model=model,kappa=kappa,prec=prec)
}
predict.Bagging <- function(model,X){
  preds <- sapply(model$classifiers,function(f) predict(f,X)$class)
  levels <- model$levels
  counts <- sapply(levels,function(i) rowSums(preds==i,na.rm=TRUE))
  pred <- levels[apply(counts,1,which.max)]
  list(class=pred)
}
