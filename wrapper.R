require(rpart)
train.rpart.model <- function(X,y,multi.model=FALSE){
  multi.model <- multi.model && !is.vector(X)
  model <- if(!multi.model){
    data <- data.frame(as.factor(y),X)
    colnames(data) <- c("y",make.names(1:(ncol(data)-1)))
    rpart(y~.,data=data,y=FALSE)
  }
  else
    apply(X,2,function(x) train.rpart.model(x,y))
  model <- list(rpart=model,multi.model=multi.model)
  class(model) <- c("rpart.model","list")
  model
}
predict.rpart.model <- function(model,X){
  if(!model$multi.model){
    newdata <- data.frame(X)
    colnames(newdata) <- make.names(1:ncol(newdata))
    class <- factor2numeric(predict(model$rpart,newdata=newdata,type="class"))
    prob <- predict(model$rpart,newdata=newdata,type="prob")
    list(class=class,prob=prob)
  }
  else{
    res <- lapply(1:length(model$rpart),function(i) predict(model$rpart[[i]],X[,i]))
    class <- sapply(res,function(x) x$class)
    prob <- sapply(res,function(x) x$prob,simplify="array")
    prob <- aperm(prob,c(1,3,2))
    list(class=class,prob=prob)
  }
}
