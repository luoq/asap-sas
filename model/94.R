comment="bagging NB.Multinomial"
require(Metrics)
source('general/util.R')
source('model/auxiliary.R')
return_each_fold_kappa <- FALSE
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
train.model <- function(X,y)
  Bagging(X,y,function(X,y)
          train.split.NB.Multinomial(X,y,split.ctrl=list(train.ratio=0.5,split="random",max.measure="kappa")),
          B=25,n=ceiling(length(y)))
apply.model <- predict.Bagging
