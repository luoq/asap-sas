## Test
## res <- CV(X,y,train.f=train.NB.Multinomial, parameter=list(ks=square.split(length(y),10)), multi.models=TRUE,intrinsic.multi.training=TRUE)
## res <- CV(X,y,train.f=train.NB.Multinomial)
##
CV <- function(X,y,K=10,split="random",
               measure.fun=c(ScoreQuadraticWeightedKappa,precision),mean.fun=c(mean,MeanQuadraticWeightedKappa),main.measure=1,
               measure.names=c("kappa","precision"),
               train.f,parameter=NULL,
               multi.models=FALSE,select.model=multi.models,intrinsic.multi.training=FALSE,return.multi.models=TRUE,retrain=TRUE){
  n <- length(y)
  all.folds <- if(split=="random")
    all.folds <-cv.kfold.random(n,K)
  else if(split=="sequential")
    cv.kfold.sequential(n,K)
  else if(split=="stratified")
    cv.kfold.stratified.random(y,K)
  else
    stop("no such split method")
  res <- lapply(1:K,function(k){
    omit <- all.folds[[k]]
    X1 <- X[-omit,,drop=FALSE]
    y1 <- y[-omit]
    X2 <- X[omit,,drop=FALSE]
    y2 <- y[omit]

    if(multi.models){
      if(!intrinsic.multi.training){
        if(is.null(parameter))
          stop("a list of parameter must be supplied")
        else
          fs <- lapply(parameter,function(p) do.call(train.f,c(list(X1),list(y1),p)))
        res <- lapply(fs,function(f) predict(f,X2))
        class <- sapply(res,function(x) x$class)
        prob <- NULL #not need this
      }
      else{
        if(is.null(parameter))
          f <- train.f(X1,y1)
        else
          f <- do.call(train.f,c(list(X1),list(y1),parameter))
        if(!return.multi.models){
          res <- predict(f,X2)
          class <- res$class
          prob <- NULL
        }
        else{
          res <- lapply(f,function(f) predict(f,X2))
          class <- sapply(res,function(x) x$class)
          prob <- NULL #not need this
        }
      }
    }
    else{
      if(is.null(parameter))
        f <- train.f(X1,y1)
      else
        f <- do.call(train.f,c(list(X1),list(y1),parameter))
      res <- predict(f,X2)
      class <- res$class
      prob <- res$prob
    }
    if(!multi.models){
      names(measure) <- measure.names
      measure <- sapply(measure.fun,function(f) f(class,y2))
    }
    else{
      measure <- apply(class,2,function(x) sapply(measure.fun,function(f) f(x,y2)))
      rownames(measure) <- measure.names
    }
    list(measure=measure,class=class,prob=prob)
  })
  measure <- sapply(res,function(x) x$measure,simplify="array")
  if(multi.models){
    d <- dim(measure)
    mean.measure <- array(dim=d[1:2])
    for (i in 1:d[1])
      for(j in 1:d[2])
        mean.measure[i,j] <- mean.fun[[i]](measure[i,j,])
    rm(d)
    rownames(mean.measure) <- measure.names
  }
  else{
    mean.measure <- sapply(1:length(measure.fun),function(i) mean.fun[[i]](measure[i,]))
    names(mean.measure) <- measure.names
  }
  ret <- list(mean.measure=mean.measure,measure=measure,result=res)

  if(select.model){
    i <- which.max(mean.measure[main.measure,])
    ret$best.index <- i
    parameter.i <- if(intrinsic.multi.training)
      parameter[[1]][[i]] #assume the first is what matters
    else
      parameter[[i]]
    ret$best.parameter <- parameter.i
    ret$best.measure <- measure[,i,]
    ret$best.mean.measure <- mean.measure[,i]
  }
  if(retrain){
    model <-
      if(!multi.models){
        if(is.null(parameter))
          train.f(X,y)
        else
          do.call(train.f,c(list(X),list(y),parameter))
      }
      else{
        if(!intrinsic.multi.training)
          do.call(train.f,c(list(X),list(y),parameter.i))
        else{
          parameter.new <- parameter
          parameter.new[[1]] <- parameter.i #assume the first is what matters
          do.call(train.f,c(list(X),list(y),parameter.new))
        }
      }
    ret$model <- model
  }
  
  class(ret) <- c("CV.result",class(ret))
  return(ret)
}
