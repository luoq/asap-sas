require(Metrics)
require(parallel)
require(Matrix)
require(tm)
source("general/feature.R")
run <- function(k){
  source(paste("model/",as.character(k),".R",sep=""))

  used_feature=used_feature[c("simple","dtm","corpus")]
  if(used_feature["dtm"]){
    for(i in 1:numberOfEssaySet){
      nwords <- unname(sapply(Set[[i]]$terms,function(x) nspace(x)+1))
      mask <- (nwords>=dtm_features_ctrl$mingram) & (nwords<=dtm_features_ctrl$maxgram)
      Set[[i]]$terms <- Set[[i]]$terms[mask]
      Set[[i]]$dtm <- Set[[i]]$dtm[,mask]
      Set[[i]]$dtm.public <- Set[[i]]$dtm.public[,mask]
      Set[[i]]$dtm <- apply_weight(Set[[i]]$dtm,dtm_features_ctrl$local_weight,dtm_features_ctrl$term_weight)
      Set[[i]]$dtm.public <- apply_weight(Set[[i]]$dtm.public,dtm_features_ctrl$local_weight,dtm_features_ctrl$term_weight)
    }
    rm(i,nwords,mask)
  }
  
  res <- mclapply(1:numberOfEssaySet,function(k){
    set.seed(84565+k^5)
    with(Set[[k]], do.call(train.model,c(list(simple_feature,dtm,corpus)[used_feature],list(y))))
  })
  models <- lapply(res,function(x) x$model)
  kappas <- sapply(res,function(x) x$kappa)
  colnames(kappas) <- as.character(1:numberOfEssaySet)
  mean.kappas <- kappas["mean",]
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
