description="L2-regularized L1-loss support vector classification"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y) CV(X,y,
                                train.f=train.LiblineaR.model,
                                parameter=direct.prod(type=3,cost=10^(-4:4)),
                                multi.model=TRUE)
