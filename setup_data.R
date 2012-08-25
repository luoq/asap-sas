require(tm)
require(parallel)
source("general/util.R")
source("general/feature.R")
train_set <- read.delim("../data/train_rel_2.tsv",stringsAsFactors=FALSE)
public_test_set <- read.delim("../data/public_leaderboard_rel_2.tsv",stringsAsFactors=FALSE)
numberOfEssaySet <- 10
Set=vector(mode="list",length=numberOfEssaySet)
Set <- mclapply(1:numberOfEssaySet,function(k){
  within(list(),{
    essay_set <- k
    corpus <- Corpus(VectorSource(with(train_set,EssayText[EssaySet==k])))
    corpus.public <- Corpus(VectorSource(with(public_test_set,EssayText[EssaySet==k])))
    id.public <- with(public_test_set,Id[EssaySet==k])
    y <- with(train_set,Score1[EssaySet==k])
    
    # simple_feature <- extract.simpleFeatrure(corpus)
    # simple_feature.public <- extract.simpleFeatrure(corpus.public)
    dtm <- as.Matrix(get_dtm(corpus,ngram=3,minDoc=floor(0.005*length(y)),maxDoc=floor(0.80*length(y))))
    terms <- colnames(dtm)
    ngram <- sapply(terms,wordNumber)
    dtm.public <- as.Matrix(get_dtm(corpus.public,dictionary=terms,ngram=5))
  })
})
save(numberOfEssaySet,Set,file="data.RData")
