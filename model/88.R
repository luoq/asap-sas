comment="glmnet calibrated by NB on 1-3-bingram and NBM prob,fit on test"
require(Metrics)
require(glmnet)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
LAPLACE <- 1e-3
train.model <- function(X,y)
  train.cv.glmnet.with.selected.nbms(X,y,
                                     fit.on.test=TRUE,
                                     cv.ctrl=list(K=10,split="random",max.measure="kappa"),
                                     nb.ctrl=list(weight.fun=function(X,y) informationGainMultinomial(X,y,laplace=LAPLACE),
                                       laplace=LAPLACE,output.probability=TRUE),
                                     glmnet.ctrl=list(alpha=0.8,nlambda=100,standardize=FALSE),
                                     calibrator_type="nb")
apply.model <- apply.glmnet.with.calibrator
