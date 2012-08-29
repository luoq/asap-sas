get.Stacking.data <- function(X,y,learners.info=NULL){
  temp <- lapply(learners.info,function(l){
    res <- l(X,y)
    class <- aggregate.CV.result(res,what="class")
    prob <- aggregate.CV.result(res,what="prob")
    if(!is.null(l$remove.one) || l$remove.one)
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
