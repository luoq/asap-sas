require(Metrics)
require(parallel)
require(Matrix)
require(tm)
return_each_fold_kappa <- TRUE
source("general/feature.R")
run <- function(k){
  source(paste("model/",as.character(k),".R",sep=""))

  used_feature=used_feature[c("simple","dtm","corpus")]
  if(used_feature["dtm"]){
    ctrl <- dtm_features_ctrl
    for(i in 1:numberOfEssaySet){
      mask <- (Set[[i]]$ngram>=ctrl$mingram) & (Set[[i]]$ngram<=ctrl$maxgram)
      Set[[i]]$terms <- Set[[i]]$terms[mask]
      Set[[i]]$dtm <- Set[[i]]$dtm[,mask]
      Set[[i]]$dtm.public <- Set[[i]]$dtm.public[,mask]
      normalize <- "normalize" %in% names(ctrl) && ctrl$normalize
      Set[[i]]$dtm <- apply_weight(Set[[i]]$dtm,ctrl$local_weight,ctrl$term_weight,normalize)
      Set[[i]]$dtm.public <- apply_weight(Set[[i]]$dtm.public,ctrl$local_weight,ctrl$term_weight,normalize)
    }
    rm(i,mask,ctrl)
  }
  
  res <- mclapply(1:numberOfEssaySet,function(k){
    set.seed(84565+k^5)
    with(Set[[k]], do.call(train.model,c(list(simple_feature,dtm,corpus)[used_feature],list(y))))
  })
  models <- lapply(res,function(x) x$model)
  kappas <- sapply(res,function(x) x$kappa)
  if(return_each_fold_kappa){
    colnames(kappas) <- as.character(1:numberOfEssaySet)
    mean.kappas <- kappas["mean",]
  }
  else{
    names(kappas) <- as.character(1:numberOfEssaySet)
    mean.kappas <- kappas
  }
  mean.kappas <- c(mean.kappas,MeanQuadraticWeightedKappa(mean.kappas))
  info <- data.frame(id=k,comment=comment)
  info <- cbind(info,matrix(mean.kappas,nrow=1))
  write.table(info ,file="kappa.csv",append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
  
  pred.public <- mclapply(1:numberOfEssaySet,function(k){
    with(Set[[k]],{
      pred <- do.call(apply.model,c(list(models[[k]]),list(simple_feature.public,dtm.public,corpus.public)[used_feature]))
      data.frame(id=id.public,essay_score=pred)
    })
  })
  pred.public <- do.call(rbind,pred.public)
  pred.public <- pred.public[order(pred.public$id),]
  write.csv(pred.public,
            ,file=paste("model/","public",as.character(k),".csv",sep="")
            ,quote=FALSE,row.names=FALSE)

  save(models,kappas,comment,pred.public,file=paste("model/",as.character(k),".RData",sep=""))
  # return(list(models=models,kappas=kappas,pred.public=pred.public))
}
