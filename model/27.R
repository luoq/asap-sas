description="CV.Glmnet.with.NB stratified sampling"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y) CV.Glmnet.with.NB(X,y,cv.ctrl=list(K=10,split="stratified"))
