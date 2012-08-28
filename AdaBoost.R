AdaBoost <- function(X,y,learner,T){
  n <- length(y)
  D <- rep(1,n)/n
  hs <- NULL
  betas <- NULL
  Ds <- D
  for(t in 1:T){
    h <- learner(X,y,weights=D)
    pred <- predict(h,X)$class
    mask <- pred==y
    epsilon <- sum(D[!mask])
    if(epsilon > 1/2)
      break
    beta <- epsilon/(1-epsilon)
    D[mask] <- D[mask]*beta
    D <- D/sum(D)
    Ds <- cbind(Ds,D)
    hs <- c(hs,list(h))
    betas <- c(betas,beta)
  }
  model <- list(hs=hs,minus.log.betas=-log(betas),Ds=Ds)
  class(model) <- c("AdaBoost.model","list")
  model
}
AdaBoost.V1 <- function(X,y,learner,T,self.cv=TRUE,cv.ctrl=NULL){
  n <- length(y)
  D <- rep(1,n)/n
  hs <- NULL
  betas <- NULL
  Ds <- D
  for(t in 1:T){
    res <- 
      if(self.cv)
        learner(X,y,weights=D)
      else
        do.call(CV,c(list(X,y,weights=D,train.f=learner),cv.ctrl))
    h <- res$model
    pred <- as.vector(aggregate.CV.result(res,what="class"))
    mask <- pred==y
    epsilon <- sum(D[!mask])
    if(epsilon > 1/2)
      break
    beta <- epsilon/(1-epsilon)
    D[mask] <- D[mask]*beta
    D <- D/sum(D)
    Ds <- cbind(Ds,D)
    hs <- c(hs,list(h))
    betas <- c(betas,beta)
  }
  model <- list(hs=hs,minus.log.betas=-log(betas),Ds=Ds)
  class(model) <- c("AdaBoost.model","list")
  model
}
predict.AdaBoost.model <- function(model,X){
  ys <- sapply(model$hs,function(h) predict(h,X)$class)
  levels <- as.numeric(levels(as.factor(ys)))
  scores <- sapply(levels,function(i)
                   (ys==i) %*% model$minus.log.betas)
  class <- levels[apply(scores,1,which.max)]
  return(list(class=class))
}
