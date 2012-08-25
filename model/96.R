comment="LiblineaR 5 on 1-3-gram,bintf"
require(Metrics)
require(LiblineaR)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y)
  train.cv.f(X,y,
             train.LiblineaR,parameters=direct.prod(type=5,cost=c(1000,100,10,1,0.1,0.01,0.001)),
             cv.ctrl=list(K=10,split="random",max.measure="kappa"))
apply.model <- predict
