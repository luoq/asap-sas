comment="glmnet on 1-3-gram,bintf and nbms calibrated with NB,no standardize"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
train.model <- function(X,y)
  train.cv.glmnet.with.calibrator(X,y,
                                  transformer1=nbm.all.transformer,
                                  cv.ctrl=list(K=10,split="random",max.measure="kappa"),
                                  glmnet.ctrl=list(alpha=0.8,nlambda=100,standardize=FALSE),
                                  calibrator_type="nb")
apply.model <- apply.glmnet.with.calibrator
