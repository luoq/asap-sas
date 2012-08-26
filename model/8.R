description="CV.Glmnet.with.NB.CV logtf no prior in nb"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="logtf",term_weight=NULL)
train.model <- function(X,y)
  CV.Glmnet.with.NB.CV(X,y,
                       glmnet.ctrl=list(alpha=0.8,standardize=FALSE),
                       calibrator.ctrl=list(add.prior=FALSE))
