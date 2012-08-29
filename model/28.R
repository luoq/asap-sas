description="Glmnet multinomial stratified sampling"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y) CV.Glmnet(X,y,
                                       glmnet.ctrl=list(alpha=0.8,standardize=FALSE,family="multinomial"),
                                       cv.ctrl=list(K=5,split="stratified"))
