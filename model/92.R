comment="bagging glmnet on 1-3-gram,bintf max precision"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
return_each_fold_kappa <- FALSE
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y)
  Bagging(X,y,function(X,y)
          train.split.glmnet.with.calibrator(X,y,split.ctrl=list(train.ratio=0.7,split="random",max.measure="precision")),
          B=25,n=ceiling(length(y)))
apply.model <- predict.Bagging
