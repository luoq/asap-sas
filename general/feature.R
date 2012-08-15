require(tm)
characterNumber <- function(txt)
  nchar(gsub("\\W+","",txt))
misspelledNumber <- function(txt){
  require(Aspell)
  n <- 0
  spellDoc(txt,function (...) n <<- n+1)
  n
}
misspelledWords <- function(txt){
  require(Aspell)
  words <- NULL
  spellDoc(txt,function (word,...) words <<- c(words,word))
  words
}
sentenceNumber <- function(txt){
  require('openNLP')
  length(sentDetect(txt))
}
extract.simpleFeatrure <- function(corpus){
  preprocess_corpus <- function(corpus){
    corpus <- tm_map(corpus,function(x) gsub("[[:punct:]]+", " ", x))
    corpus
  }
  result <- NULL
  result$Nword <- wordNumber(corpus)
  result$NwordRoot4 <- result$Nword^(1/4)
  result$Nchar <- characterNumber(corpus)
  result$NcharAvg <- with(result,Nchar/Nword)
  # result$Nmisspell <- sapply(corpus,misspelledNumber)
  result$Nsent <- sapply(corpus,sentenceNumber)
  result$SentLen <-  with(result,Nword/Nsent)

  dtm <- DocumentTermMatrix(preprocess_corpus(corpus))
  result$DiffWord <- apply(dtm>0,1,sum)
  terms <- Terms(dtm)
  for(i in 5:10){
    mask <- (nchar(terms)>=i)
    result[[paste("W",as.character(i),sep="")]] <-
      apply(dtm[,mask],1,sum)
  }
  
  as.data.frame(result)
}
get_dtm <- function(corpus,ngram=3,dictionary=NULL){
  preprocess_corpus <- function(corpus){
    corpus <- tm_map(corpus,function(x) gsub("[[:punct:]]+", " ", x))
    corpus <- tm_map(corpus,stemDocument)
    corpus
  }
  require(RWeka)
  corpus <- preprocess_corpus(corpus)
  default.ctrl <- list(
                       tokenize=function(x) NGramTokenizer(x,control=Weka_control(max=ngram)),
                       removePunctuation=FALSE,
                       removeNumbers=FALSE,
                       #stopwords=stopwords("en"),
                       stemming=FALSE
                       )
  if(is.null(dictionary))
    ctrl <- c(default.ctrl,list(bounds=list(global=c(4,Inf))))# about half the terms only belong to one document
  else
    ctrl <- c(default.ctrl,list(dictionary=dictionary))
  DocumentTermMatrix(corpus,control=ctrl)
}
idf <- function(M){
  n <- nrow(M)
  idf <- log2(n)-log2(colSums(M>0))+1
}
apply_weight <- function(M,local_weight="tf",term_weight=idf,normalize=FALSE){
  if(normalize)
    M <- Diagonal(x=1/(rowSums(M))) %*% M
  ## compute weights first then apply local transfrom then term transform
  if(is.numeric(term_weight))
    weights <- term_weight
  else if(is.function(term_weight))
    weights <- term_weight(M)
  else
    weights <- NULL
  
  if(local_weight=="logtf")
    M@x <- log(M@x+1)+1
  else if(local_weight=="bintf")
    M <- (M>0)*1
  
  if(!is.null(weights))
    M <- M %*% Diagonal(x=weights)
  M
}
build_lsa <- function(X,dim_calc=dim_share(0.8)){
  if(!is.function(dim_calc)){
    if(is.integer(dim_calc))
      k <- dim_calc
    else
      k <- ceiling(dim_calc*nrow(X))
    ## space <<- irlba(X,nu=dim_calc,nv=dim_calc)
    space <- svd(X,nu=k,nv=k)
    space$d <- space$d[1:k]# This is needed if using svd because the size of d is not trunced
  }
  else{
    SVD <- svd(X)
    k <- max(dim_calc(SVD$d),3)# at least 3 if trunc_dim=1 space$u is not a matrix
    space <- NULL
    space$u <- SVD$u[,1:k]
    space$d <- SVD$d[1:k]
    space$v <- SVD$v[,1:k]
  }
  space$k <- k
  space
}
fold_in_lsa <- function(M,space){
  X <- (M %*% space$v) %*% Diagonal(x=1/space$d)
  as.matrix(X)
}
