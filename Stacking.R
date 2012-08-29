Stacking <- function(X,y,learners.info=NULL,learner2){
  temp <- lapply(learners.info,function(l){
    res <- l(X,y)
    data <- aggregate.CV.result(res,what="prob")
    if(!is.null(l$remove.one) || l$remove.one)
      data <- data[,1:(ncol(data)-1)]
    list(f=res$model,d=data)
  })
  remove.one <- sapply(learners.info,function(x) is.null(x$remove.one) || x$remove.one)
  fs <- lapply(temp,function(x) x$f)
  data <- lapply(temp,function(x) x$d)
  data <- do.call(cbind,data)
  f2 <- learner2(data,y)
  model <- list(fs=fs,f2=f2,remove.one=remove.one)
  class(model) <- c("Stacking.model","list")
  model
}
predict.Stacking.model <- function(model,X){
  data <- lapply(1:length(model$fs),function(i){
    f <- fs[[i]]
    remove.one <- model$remove.one[[i]]
    d <- predict(f,X)$prob
    if(remove.one)
      d <- d[,1:(ncol(d)-1)]
    d
  })
  data <- do.call(cbind,data)
  predict(model$f2,data)
}
