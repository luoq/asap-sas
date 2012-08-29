get.Stacking.data <- function(X,y,learners.info=NULL){
  temp <- lapply(learners.info,function(x){
    res <- x$learner(X,y)
    class <- aggregate.CV.result(res,what="class")
    prob <- aggregate.CV.result(res,what="prob")
    if(is.null(x$remove.one) || x$remove.one)
      prob <- prob[,1:(ncol(prob)-1)]
    list(f=res$model,class=class,prob=prob)
  })
  remove.one <- sapply(learners.info,function(x) is.null(x$remove.one) || x$remove.one)
  fs <- lapply(temp,function(x) x$f)
  prob <- lapply(temp,function(x) x$prob)
  prob <- do.call(cbind,prob)
  class <- lapply(temp,function(x) x$class)
  class <- do.call(cbind,class)
  model <- list(fs=fs,remove.one=remove.one)
  class(model) <- c("Stacking.data.model","list")
  list(prob=prob,class=class,model=model)
}
predict.Stacking.data.model <- function(model,X){
  temp <- lapply(1:length(model$fs),function(i){
    f <- model$fs[[i]]
    remove.one <- model$remove.one[[i]]
    res <- predict(f,X)
    class <- res$class
    prob <- res$prob
    if(remove.one)
      prob <- prob[,1:(ncol(prob)-1)]
    list(class=class,prob=prob)
  })
  class <- lapply(temp,function(x) x$class)
  class <- do.call(cbind,class)
  prob <- lapply(temp,function(x) x$prob)
  prob <- do.call(cbind,prob)
  list(prob=prob,class=class)
}
get.CV.Stacking.data <- function(X,y,f,K=5,split="random"){
  n <- length(y)
  all.folds <- if(split=="random")
    all.folds <-cv.kfold.random(n,K)
  else if(split=="sequential")
    cv.kfold.sequential(n,K)
  else if(split=="stratified")
    cv.kfold.stratified.random(y,K)
  else
    stop("no such split method")
  ret <- list(all.folds=all.folds)

  res <- lapply(1:K,function(k){
    omit <- all.folds[[k]]
    X1 <- X[-omit,,drop=FALSE]
    y1 <- y[-omit]
    X2 <- X[omit,,drop=FALSE]
    y2 <- y[omit]

    res1 <- f(X1,y1)
    class1 <- res1$class
    prob1 <- res1$prob
    model <- res1$model
    res2 <- predict(model,X2)
    class2 <- res2$class
    prob2 <- res2$prob
    list(model=model,prob1=prob1,class1=class1,prob2=prob2,class2=class2,y1=y1,y2=y2)
  })
}
assess.meta.classifer <- function(train.f,result){
  kappas <- sapply(result,function(x){
    with(x,{
      f <- train.f(prob1,y1)
      pred2 <- predict(f,prob2)$class
      ScoreQuadraticWeightedKappa(pred2,y2)
    })
  })
  mean.kappa <- MeanQuadraticWeightedKappa(kappas)
  return(list(kappa=kappas,mean.kappa=kappa))
}
