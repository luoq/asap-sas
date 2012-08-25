description="NB.Multinomial Best by InformationGainMultinomial"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
train.model <- function(X,y)
  CV.NB.Multinomial.Best.K(X,y,ks=square.split(length(y),100))
