comment="NB.Multinomial on 1-3-gram,IG,laplace=1e-3"
require(Metrics)
source('general/util.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
LAPLACE <- 1e-3
train.model <- function(X,y)
  train.cv.NB.Multinomial(X,y,K=10,split="random",
                          weight.fun=function(X,y) informationGainMultinomial(X,y,laplace=LAPLACE),
                          max.measure="kappa")
apply.model <- apply.model.NB.Multinomial
