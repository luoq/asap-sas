require(tm)
source("general/feature.R")
train_set <- read.delim("../data/train_rel_2.tsv",stringsAsFactors=FALSE)
public_test_set <- read.delim("../data/public_leaderboard_rel_2.tsv",stringsAsFactors=FALSE)
numberOfEssaySet <- 10
Set=vector(mode="list",length=numberOfEssaySet)
for( k in 1:numberOfEssaySet){
  Set[[k]]$essay_set <- k
  Set[[k]]$corpus <- Corpus(VectorSource(with(train_set,EssayText[EssaySet==k])))
  Set[[k]]$simple_feature <- extract.simpleFeatrure(Set[[k]]$corpus)
  Set[[k]]$y <- with(train_set,Score1[EssaySet==k])
  Set[[k]]$corpus.public <- Corpus(VectorSource(with(public_test_set,EssayText[EssaySet==k])))
  Set[[k]]$simple_feature.public <- extract.simpleFeatrure(Set[[k]]$corpus.public)
  Set[[k]]$id.public <- with(public_test_set,Id[EssaySet==k])
  Set[[k]]$dtm <- get_dtm(Set[[k]]$corpus)
  Set[[k]]$terms <- Set[[k]]$dtm$dimnames$Terms
  Set[[k]]$dtm.public <- get_dtm(Set[[k]]$corpus.public,dictionary=Set[[k]]$terms)
  Set[[k]]$dtm <- as.Matrix(Set[[k]]$dtm)
  Set[[k]]$dtm.public <- as.Matrix(Set[[k]]$dtm.public)
}
# data.dir <- function(k)
#   paste("exp/",as.character(k),"/",sep="")
# data.file <- function(k)
#   paste(data.dir(k),".RData",sep="")
# for( k in 1:numberOfEssaySet){
#   dir.create(data.dir(k),showWarnings=FALSE,recursive=TRUE)
#   system(paste("ln -sf ../Rprofile.common ",data.dir(k),".Rprofile",sep=""))
#   with(Set[[k]],save(list=ls(),file=data.file(k)))
# }
save(numberOfEssaySet,Set,file="data.RData")

numberOfEssaySet <- 2
Set <- Set[1:2]
save(numberOfEssaySet,Set,file="data.fastcheck.RData")
