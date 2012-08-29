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
D <- function(X,y){
  K <- 10
  main.measure <- 2
  learners.info <-
    list(
         list(learner=prebinarizer(
                function(X,y)
                CV.Glmnet.with.NB(X,y,cv.ctrl=list(K=K,main.measure=main.measure)))),
         list(learner=function(X,y)
              CV.NB.Multinomial.Best.K(X,y,weight.fun="informationGain2",
                                       ks=4+square.split(ncol(X)-4,100),
                                       cv.ctrl=list(K=K,main.measure=main.measure))),
         list(learner=prebinarizer(
                function(X,y)
                CV.Glmnet(X,y,glmnet.ctrl=list(alpha=0.8,standardize=FALSE,family="multinomial"),
                          cv.ctrl=list(K=K,main.measure=main.measure)))),
         list(learner=prebinarizer(
                function(X,y)
                CV(X,y,train.f=function(X,y) train.LiblineaR.model(X,y,type=5,cost=0.1),
                   K=K,main.measure=main.measure)),
              remove.one=FALSE))
  get.Stacking.data(X,y,learners.info=learners.info)
}
CV.D <- function(X,y)
  get.CV.Stacking.data(X,y,D)
get.all.stacking.data <- function(){
  mclapply(Set,function(x) {
    CV.D(x$dtm,x$y)
  })
}
report.stacking.1 <- function(k){
  kappa <- sapply(Stacking.Results[[k]],function(x) x$mean.kappa)
  prec <- sapply(Stacking.Results[[k]],function(x) x$mean.prec)
  data <- rbind(c(kappa,MeanQuadraticWeightedKappa(kappa)),
                c(prec,mean(prec)))
  colnames(data) <- c(as.character(1:(ncol(data)-1)),"mean")
  data <- as.data.frame(data)
  print(round(data,3))
}
report.stacking <- function(ID=NULL){
  cat("\n")
  if(is.null(ID))
    ID <- length(Stacking.Results)
  if(length(ID)==1 && ID==0)
    ID <- 1:length(Stacking.Results)
  sapply(ID,function(i) {
    report.stacking.1(i)
    cat("\n")
    1
  })
}
assess.combining.method <- function(f,result){
  res <- lapply(result,function(x){
    pred2 <- f(x)
    with(x,{
      kappa <- ScoreQuadraticWeightedKappa(pred2,y2)
      prec <- precision(pred2,y2)
      list(kappa=kappa,prec=prec)
    })
  })
  kappas <- sapply(res,function(x) x$kappa)
  precs <- sapply(res,function(x) x$prec)
  mean.kappa <- MeanQuadraticWeightedKappa(kappas)
  mean.prec <- mean(precs)
  return(list(kappa=kappas,mean.kappa=mean.kappa,prec=precs,mean.prec=mean.prec))
}
assess.meta.classifer <- function(train.f,result)
  assess.combining.method(
                          function(x){
                            with(x,{
                              f <- train.f(prob1,y1)
                              predict(f,prob2)$class
                            })
                          },result)
assess.meta.classifer.all <- function(train.f)
  mclapply(Stacking.data,function(x) assess.meta.classifer(train.f,x))
assess.combining.method.all <- function(f)
  mclapply(Stacking.data,function(x) assess.combining.method(f,x))
