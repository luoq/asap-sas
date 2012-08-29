description="bagging CV.Glmnet.with.NB"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
base <- function(X,y) CV.Glmnet.with.NB(X,y,cv.ctrl=list(K=2))$model
L <- function(X,y)
  Bagging(X,y,base,B=9,n=ceiling(length(y)))$model
train.model <- function(X,y)
  CV(X,y,train.f=L,K=10)
