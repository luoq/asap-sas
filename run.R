require(Metrics)
require(parallel)
require(Matrix)
require(tm)
save.results <- function()
  save(Results,Y.test,file='model/model.RData')
logging <- function(ID){
  kappas <- sapply(Results[[ID]]$Assessmen,function(x) x$kappa)
  kappas <- c(kappas,MeanQuadraticWeightedKappa(kappas))
  info <- data.frame(id=ID,description=Results[[ID]]$description)
  info <- cbind(info,matrix(kappas,nrow=1))
  write.table(info ,file="model/log.txt",append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}
seed1 <- function(k) 27459+k^3
test.mask <- function(k){
  set.seed(seed1(k))
  n <- length(Set[[k]]$y)
  mask <- sample(n,floor(n*0.8))
  -mask
}
pred.table <- function(k){
  pred <- sapply(1:length(Results),function(id)
                 Results[[id]]$Assessment[[k]]$test.result$class)
  colnames(pred) <- sapply(as.character(1:length(Results)),function(x) paste("p",x,sep=""))
  y <- Y.test[[k]]
  count <- correct.count(pred,y)
  most <- majority(pred)
  data <- cbind(pred,y=y,c=count,m=most)
  rownames(data) <- Set[[k]]$id[test.mask(k)]
  as.data.frame(data)
}
get.cv.result.1 <- function(ID,k,what="prob",remove.one=TRUE){
  data <- aggregate.CV.result(Results[[ID]]$FullModel[[k]],what)
  if(remove.one)
    data <- data[,1:(ncol(data)-1)]
  colnames(data) <- sapply(1:ncol(data),function(i) paste(as.character(ID),".",as.character(i),sep=""))
  as.data.frame(data)
}
get.cv.result <- function(k,ids){
  data <- lapply(ids,function(id) {
    remove.one <- id!=19
    get.cv.result.1(id,k,remove.one=remove.one)
  })
  data <- do.call(cbind,data)
  data <- cbind(data,y=Set[[k]]$y)
  data
}
get.ture.y.on.test.set <- function()
  lapply(1:length(Set),function(k) Set[[k]]$y[test.mask(k)])
report <- function(ID=NULL){
  cat("\n")
  if(is.null(ID))
    ID <- length(Results)
  if(length(ID)==1 && ID==0)
    ID <- 1:length(Results)
  sapply(ID,function(i) {
    report.1(i)
    cat("\n")
    1
  })
}
report.1 <- function(ID){
  add.mean <- function(x){
    mean <- apply(x,1,MeanQuadraticWeightedKappa)
    cbind(x,mean)
  }
  if(ID>length(Results))
    stop("No such result")
  cat(sprintf("Model %d\n",ID))
  cat(sprintf("%s\n",Results[[ID]]$description))

  res <- data.frame()
  if(!is.null(Results[[ID]]$Assessment)){
    test <- sapply(Results[[ID]]$Assessment,function(x) x$kappa)
    test.prec <- sapply(1:length(Results[[ID]]$Assessment),function(k)
                        precision(Results[[ID]]$Assessmen[[k]]$test.result$class,Y.test[[k]]))
    res <- rbind(res,test=test)
    res <- rbind(res,test.prec=test.prec)
    if("CV.result" %in% class(Results[[ID]]$Assessment[[1]]$train.result)){
      found <- FALSE
      for(name in c("best.mean.measure","mean.measure")){
        if(!found && !is.null(Results[[ID]]$Assessment[[1]]$train.result[[name]])){
          train.cv <- sapply(Results[[ID]]$Assessment,function(x) x$train.result[[name]]["kappa"])
          train.cv.prec <- sapply(Results[[ID]]$Assessment,function(x) x$train.result[[name]]["precision"])
          train.cv.prec <- c(train.cv.prec,NA)
          res <- rbind(res,train.cv=train.cv)
          res <- rbind(res,train.cv.prec=train.cv.prec)
          found <- TRUE
        }
      }
    }
  }
  if(!is.null(Results[[ID]]$FullModel)){
    if("CV.result" %in% class(Results[[ID]]$FullModel[[1]]))
      found <- FALSE
      for(name in c("best.mean.measure","mean.measure")){
        if(!found && !is.null(Results[[ID]]$FullModel[[1]][[name]])){
          full.cv <- sapply(Results[[ID]]$FullModel,function(x) x[[name]]["kappa"])
          full.cv.prec <- sapply(Results[[ID]]$FullModel,function(x) x[[name]]["precision"])
          full.cv.prec <- c(full.cv.prec,NA)
          res <- rbind(res,full.cv=full.cv)
          res <- rbind(res,full.cv.prec=full.cv.prec)
          found <- TRUE
        }
      }
  }
  res <- add.mean(res)
  colnames(res) <- c(as.character(1:(length(res)-1)),"mean")
  print(format(res,digits=3))
  "-"
}
run <- function(ID,train.on.full=TRUE,model.assessment=!train.on.full,debug=FALSE) {
  if(debug)
    mclapply <- lapply
  numberOfEssaySet <- length(Set)
  source(paste("model/",as.character(ID),".R",sep=""))
  used.feature=used.feature[c("simple","dtm","corpus")]
  if(ID>length(Results))
    length(Results) <<- ID
  Results[[ID]]$description <<- description
  Results[[ID]]$used.feature <<- used.feature
  Results[[ID]]$dtm.feature.ctrl <<- dtm.feature.ctrl

  if(used.feature["dtm"]){
    ctrl <- dtm.feature.ctrl
    for(i in 1:numberOfEssaySet){
      mask <- (Set[[i]]$ngram>=dtm.feature.ctrl$mingram) & (Set[[i]]$ngram<=dtm.feature.ctrl$maxgram)
      Set[[i]]$terms <- Set[[i]]$terms[mask]
      Set[[i]]$dtm <- Set[[i]]$dtm[,mask]
      normalize <- "normalize" %in% names(dtm.feature.ctrl) && dtm.feature.ctrl$normalize
      Set[[i]]$dtm <- apply_weight(Set[[i]]$dtm,dtm.feature.ctrl$local_weight,dtm.feature.ctrl$term_weight,normalize)
      if(train.on.full){
        Set[[i]]$dtm.public <- Set[[i]]$dtm.public[,mask]
        Set[[i]]$dtm.public <- apply_weight(Set[[i]]$dtm.public,dtm.feature.ctrl$local_weight,dtm.feature.ctrl$term_weight,normalize)
      }
    }
    rm(i,mask)
  }

  name.to.object <- function(x)
    eval.parent(parse(text=x),2)
  assess <- function(k){
    set.seed(seed1(k))
    with(Set[[k]],{
      n <- length(y)
      mask <- sample(n,floor(n*0.8))

      train.result <- do.call(train.model,lapply(c(list("simple_feature[mask,]","dtm[mask,]","corpus[mask]")[used.feature],"y[mask]"),name.to.object))
      test.result <- do.call(predict,c(list(train.result$model),lapply(list("simple_feature[-mask,]","dtm[-mask,]","corpus[-mask]")[used.feature],name.to.object)))
      pred <- test.result$class
      kappa <- ScoreQuadraticWeightedKappa(pred,y[-mask])
      list(train.result=train.result,test.result=test.result,kappa=kappa)
    })
  }
  train.full <- function(k){
    set.seed(84565+k^5)
    with(Set[[k]],
         do.call(train.model,lapply(c(list("simple_feature","dtm","corpus")[used.feature],"y"),name.to.object)))

  }
  pred.public <- function(k){
    with(Set[[k]],{
      res <- do.call(predict,c(list(Results[[ID]]$FullModel[[k]]$model),
                               lapply(list("simple_feature.public","dtm.public","corpus.public")[used.feature],name.to.object)))
      data.frame(id=id.public,essay_score=res$class)
    })}
  write.public <- function(ID){
    pred <- do.call(rbind,Results[[ID]]$PublicPrediction)
    pred <- pred[order(pred$id),]
    write.csv(pred,
              ,file=paste("model/",as.character(ID),".public.csv",sep="")
              ,quote=FALSE,row.names=FALSE)
  }

  if(model.assessment){
    Results[[ID]]$Assessment <<- mclapply(1:numberOfEssaySet,assess)
    logging(ID)
  }
  if(train.on.full){
    Results[[ID]]$FullModel <<- mclapply(1:numberOfEssaySet,train.full)
    Results[[ID]]$PublicPrediction <<- mclapply(1:numberOfEssaySet,pred.public)
    write.public(ID)
  }
  save.results()
}
load.test.example <- function(k,n=500,test.ratio=0.2){
  train.index <- 1:n
  test.index <- n+(1:floor(n*test.ratio))
  X <<- Set[[k]]$dtm[train.index,]
  mask <- colSums(X)>0
  X <<- X[,mask]
  y <<- Set[[k]]$y[train.index]
  X1 <<- Set[[k]]$dtm[test.index,]
  X1 <<- X1[,mask]
  y1 <<- Set[[k]]$y[test.index]
}
