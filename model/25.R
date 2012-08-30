description="bagging L1 regularized L2 loss SVC"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
base <- function(X,y) {
  type <- sample(c(0,5,6),1)
  cost <- sample(c(1,0.1,0.01),1)
  train.LiblineaR.model(X,y,type=type,cost=cost)
}
train.model <- function(X,y)
  Bagging(X,y,base,B=99,n=ceiling(length(y)))
