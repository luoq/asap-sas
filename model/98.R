comment="LiblineaR on 1-gram,bintf"
require(Metrics)
require(LiblineaR)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=1,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y)
  train.cv.f(X,y,
             train.LiblineaR,parameters=direct.prod(type=0:7,cost=c(1000,100,10,1,0.1,0.01,0.001)),
             cv.ctrl=list(K=10,split="random",max.measure="kappa"))
apply.model <- predict
