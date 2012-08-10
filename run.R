require(Metrics)
run <- function(k){
  source(paste("model/",as.character(k),".R",sep=""))
  used_feature=used_feature[c("simple","dtm","corpus")]
  res <- lapply(1:numberOfEssaySet,function(k){
    with(Set[[k]], do.call(train.model,c(list(simple_feature,dtm,corpus)[used_feature],list(y))))
  })
  models <- lapply(res,function(x) x$model)
  kappas <- sapply(res,function(x) x$kappa)
  colnames(kappas) <- as.character(1:numberOfEssaySet)
  mean.kappas <- kappas["mean",]
  mean.kappas <- c(k,mean.kappas,MeanQuadraticWeightedKappa(mean.kappas))
  write.table(matrix(mean.kappas,nrow=1),file="kappa.csv",append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
  
  pred.public <-lapply(1:numberOfEssaySet,function(k){
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

  save(models,kappas,pred.public,file=paste("model/",as.character(k),".RData",sep=""))
  return(list(models=models,kappas=kappas,pred.public=pred.public))
}
