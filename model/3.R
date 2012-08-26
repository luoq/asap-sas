description="NB.Bernoulli K Best by InformationGain"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="bintf",term_weight=NULL)
train.model <- function(X,y)
  CV.NB.Bernoulli.Best.K(X,y,
                         ks=4+square.split(ncol(X)-4,100)
                         ## ks=4+square.split(as.integer(0.5*ncol(X))-4,100)
                         ## ks=as.integer(seq(5,ncol(X),length=100))
                         )
