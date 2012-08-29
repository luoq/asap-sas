description="bagging glmnet on L1 regularized L2 loss SVC"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
base <- function(X,y) train.LiblineaR.model(X,y,type=5,cost=0.1)
L <- function(X,y)
  Bagging(X,y,base,B=25,n=ceiling(length(y)))$model
train.model <- function(X,y)
  CV(X,y,train.f=L,K=10)
