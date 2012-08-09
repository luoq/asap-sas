data.file <- function(k)
  paste("data/",as.character(k),".RData",sep="")
train_set <- read.delim("../data/train_rel_2.tsv",stringsAsFactors=FALSE)
numberOfEssaySet <- 10
Set=vector(mode="list",length=numberOfEssaySet)
for( k in 1:numberOfEssaySet){
  Set[[k]]$essay_set <- k
  Set[[k]]$corpus <- Corpus(VectorSource(with(train_set,EssayText[EssaySet==k])))
  Set[[k]]$simple_feature <- extract.simpleFeatrure(Set[[k]]$corpus)
  Set[[k]]$y <- with(train_set,Score1[EssaySet==k])
  with(Set[[k]],save(list=ls(),file=data.file(k)))
}
