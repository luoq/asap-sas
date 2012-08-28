description="AdaBoost on Glmnet.with.NB"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
L <- function(X,y){
  model <- AdaBoost(X,y,function(...)
                    CV.Glmnet.with.NB(...,cv.ctrl=list(K=5))$model,
                    5)
}
train.model <- function(X,y,K=10)
  CV(X,y,train.f=L)
