run <- function(k){
  source(paste("model/",as.character(k),".R",sep=""))
  res <- lapply(1:numberOfEssaySet,function(k){
    with(Set[[k]],{
      if(use_simple_feature)
        train.model(corpus,simple_feature,y)
      else
        train.model(corpus,y)
    })
  })
  models <- lapply(res,function(x) x$model)
  kappas <- sapply(res,function(x) x$kappa)
  colnames(kappas) <- as.character(1:numberOfEssaySet)
  
  pred.public <-lapply(1:numberOfEssaySet,function(k){
    with(Set[[k]],{
      pred <- 
        if(use_simple_feature)
          apply.model(models[[k]],corpus.public,simple_feature.public)
        else
          apply.model(modles[[k]],corpus.public)
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
