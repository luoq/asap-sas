Stacking <- function(X,y,learners.info=NULL,learner2){
  temp <- lapply(learners.info,function(l){
    if(!is.null(l$self.cv) && l$self.cv)
      cv.l <- l$learner
    else
      stop("not implemented")
    res <- cv.l(X,y)
    data <- aggregate.CV.result(res,what="prob")
    data <- data[,1:(ncol(data)-1)]
    list(f=res$model,d=data)
  })
  fs <- lapply(temp,function(x) x$f)
  data <- lapply(temp,function(x) x$d)
  data <- do.call(cbind,data)
  f2 <- learner2(data,y)
  model <- list(fs=fs,f2=f2)
  class(model) <- c("Stacking.model","list")
  model
}
predict.Stacking.model <- function(model,X){
  data <- lapply(model$fs,function(f){
    d <- predict(f,X)$prob
    d <- d[,1:(ncol(d)-1)]
  })
  data <- do.call(cbind,data)
  predict(model$f2,data)
}
