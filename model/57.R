comment="glmnet on 1-3-gram with bintf,proportional assignment,no standardize"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y)
  train.cv.glmnet.with.calibrator(X,y,
                          cv.ctrl=list(K=5,split="sequential",max.measure="kappa"),
                          glmnet.ctrl=list(alpha=0.8,nlambda=100,standardize=FALSE),
                          calibrator_type="pa")
apply.model <- apply.glmnet.with.calibrator
