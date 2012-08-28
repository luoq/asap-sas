## Test
## res <- CV(X,y,train.f=train.NB.Multinomial, parameter=list(ks=square.split(length(y),10)), multi.model=TRUE,intrinsic.multi.training=TRUE)
## res <- CV(X,y,train.f=train.NB.Multinomial)
##
CV <- function(X,y,weights=NULL,
               K=10,split="random",
               measure.fun=c(ScoreQuadraticWeightedKappa,precision),mean.fun=c(MeanQuadraticWeightedKappa,mean),main.measure=1,
               measure.names=c("kappa","precision"),
               train.f,parameter=NULL,retrain=TRUE,
               multi.model=FALSE,select.model=multi.model,intrinsic.multi.training=FALSE,return.multi.model=multi.model,
               return.result=!multi.model,return.best.result=multi.model){
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
    base.parameter <-
      if(is.null(weights))
        list(X1,y1)
      else
        list(X1,y1,weights=weights[-omit])

    f <-
      if(!multi.model || intrinsic.multi.training)
        do.call(train.f,c(base.parameter,parameter))
      else
        lapply(parameter,function(p) do.call(train.f,c(base.parameter,p)))
    res <-
      if(return.multi.model){
        res <- lapply(f,function(f) predict(f,X2))
        class <- sapply(res,function(x) x$class)
        prob <- sapply(res,function(x) x$prob,simplify="array")
        prob <- aperm(prob,c(1,3,2))
        list(class=class,prob=prob)
      }
      else
        predict(f,X2)
  })
  if(return.result)
    ret$result <- res
  if(!is.null(measure.fun)){
    measure <- sapply(1:K,function(i) with(res[[i]],{
      y <- y[all.folds[[i]]]
      if(!multi.model){
        measure <- sapply(measure.fun,function(f) f(class,y))
        names(measure) <- measure.names
      }
      else{
        measure <- apply(class,2,function(x) sapply(measure.fun,function(f) f(x,y)))
        rownames(measure) <- measure.names
      }
      measure
    }),
                      simplify="array")
    if(multi.model){
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
    ret$mean.measure <- mean.measure
    ret$measure <- measure
  }

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
    if(return.best.result)
      ret$best.result <- lapply(res,function(x) with(x,list(class=class[,i],prob=prob[,i,])))
  }
  if(retrain){
    base.parameter <-
      if(is.null(weights))
        list(X1,y1)
      else
        list(X,y,weights=weights)
    f <- if(!multi.model)
      do.call(train.f,c(base.parameter,parameter))
    else if(intrinsic.multi.training){
      parameter.new <- parameter
      parameter.new[[1]] <- parameter.i #assume the first is what matters
      do.call(train.f,c(base.parameter,parameter.new))
    }
    else
      do.call(train.f,c(base.parameter,parameter.i))
    ret$model <- f
  }

  class(ret) <- c("CV.result",class(ret))
  return(ret)
}
aggregate.CV.result <- function(x,what="class"){
  index <- c(x$all.folds,recursive=TRUE)
  location <- if(is.null(x$best.result)) "result" else "best.result"
  data <- do.call(rbind,lapply(x[[location]],function(x) {
    res <- x[[what]]
    if(is.vector(res))
      res <- matrix(res,ncol=1)
    res
  }))
  data[index,] <- data
  rownames(data)[index] <- rownames(data)
  data
}
