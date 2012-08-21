comment="NB.Multinomial on 1-3-gram,binary IG,laplace=1e-3"
require(Metrics)
source('general/util.R')
source('model/auxiliary.R')
used_feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm_features_ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
LAPLACE <- 1e-3
train.model <- function(X,y)
  train.cv.NB.Multinomial(X,y,cv.ctrl=list(K=10,split="random", max.measure="kappa"),
                          nb.ctrl=list(weight.fun=function(X,y) informationGain2(X!=0,y,laplace=LAPLACE),laplace=LAPLACE))
apply.model <- apply.NB.Multinomial
