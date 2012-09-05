require(parallel)
require(Metrics)
source('feature.R')
source('model.R')
source('meta.R')
source('wrapper.R')
source('NB.R')
source('CV.R')
source('Glmnet.R')
source('util.R')
source('feature.R')
train.txt.file <- "../data/train_rel_2.tsv"
test.txt.file <- "../data/private_leaderboard.tsv"

model.store.file <- "data/model.RData"
train.feature.store.file <- "data/train.feature.RData"
test.feature.store.file <- "data/test.feature.RData"
prediciton.store.file <- "data/prediciton.csv"

numberOfEssaySet <- 10
{
  if(file.exists(model.store.file))
    load(model.store.file)
  else{
    if(file.exists(train.feature.store.file))
      load(train.feature.store.file)
    else{
      train.txt <- read.delim(train.txt.file,stringsAsFactors=FALSE)
      train.feature <- mclapply(1:numberOfEssaySet,function(k){
        essay.set <- k
        id <- with(train.txt,Id[EssaySet==k])
        corpus <- Corpus(VectorSource(with(train.txt,EssayText[EssaySet==k])))
        y <- with(train.txt,Score1[EssaySet==k])
        dtm <- as.Matrix(get_dtm(corpus,ngram=3,minDoc=floor(0.005*length(y)),maxDoc=floor(0.80*length(y))))
        terms <- colnames(dtm)
        list(id=id,dtm=dtm,y=y,terms=terms)
      })
      save(train.feature,file=train.feature.store.file)
    }
    Models <- mclapply(1:numberOfEssaySet,function(k){
      set.seed(84565+k^5)
      with(train.feature[[k]],train.model(dtm,y))
    })
    Terms <- lapply(train.feature,function(x) x$terms)
    save(Terms,Models,file=model.store.file)
  }
}
{
  if(file.exists(test.feature.store.file))
    load(test.feature.store.file)
  else{
    test.txt <- read.delim(test.txt.file,stringsAsFactors=FALSE)
    test.feature <- mclapply(1:numberOfEssaySet,function(k){
      essay.set <- k
      id <- with(test.txt,Id[EssaySet==k])
      corpus <- Corpus(VectorSource(with(test.txt,EssayText[EssaySet==k])))
      dtm <- as.Matrix(get_dtm(corpus,dictionary=Terms[[k]],ngram=3))
      list(id=id,dtm=dtm)
    })
    save(test.feature,file=test.feature.store.file)
  }
}
pred <- mclapply(1:numberOfEssaySet,function(k){
  pred <- predict(Models[[k]],test.feature[[k]]$dtm)$class
  data.frame(id=test.feature[[k]]$id,essay_score=pred)
})
pred <- do.call(rbind,pred)
pred <- pred[order(pred$id),]
write.csv(pred,file=prediciton.store.file,quote=FALSE,row.names=FALSE)
