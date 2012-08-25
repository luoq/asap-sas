require(Metrics)
require(parallel)
require(Matrix)
require(tm)
load.results <- function(){
  data.file <- "model/model.RData"
  if(!exists("Results")){
    if(file.exists(data.file))
      load(data.file)
    else{
      Results <<- NULL
    }
  }
}
save.results <- function()
  save(Results,file="model/model.RData")
logging <- function(ID){
  kappas <- sapply(Results[[ID]]$Assessmen,function(x) x$kappa)
  kappas <- c(kappas,MeanQuadraticWeightedKappa(kappas))
  info <- data.frame(id=ID,description=Results[[ID]]$description)
  info <- cbind(info,matrix(kappas,nrow=1))
  write.table(info ,file="model/log.txt",append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}
run <- function(ID,model.assessment=TRUE,train.on.full=TRUE,predict.public=train.on.full){
  numberOfEssaySet <- length(Set)
  source(paste("model/",as.character(ID),".R",sep=""))
  used.feature=used.feature[c("simple","dtm","corpus")]
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
      if(predict.public){
        Set[[i]]$dtm.public <- Set[[i]]$dtm.public[,mask]
        Set[[i]]$dtm.public <- apply_weight(Set[[i]]$dtm.public,dtm.feature.ctrl$local_weight,dtm.feature.ctrl$term_weight,normalize)
      }
    }
    rm(i,mask)
  }

  name.to.object <- function(x)
    eval.parent(parse(text=x),2)
  assess <- function(k){
    set.seed(27459+k^3)
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
    Results[[ID]]$Assessment <- mclapply(1:numberOfEssaySet,assess)
    logging(ID)
  }
  if(train.on.full)
    Results[[ID]]$FullModel <- mclapply(1:numberOfEssaySet,train.full)
  if(predict.public){
    Results[[ID]]$PublicPrediction <- mclapply(1:numberOfEssaySet,pred.public)
    write.public(ID)
  }
}
