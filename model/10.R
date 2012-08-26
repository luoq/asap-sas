description="Stacking on Glmnet.with.NB, NB.Multinomial"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
train.model <- function(X,y){
  ks=4+square.split(ncol(X)-4,100)
  learners.info <-
    list(
         list(learner=CV.Glmnet.with.NB.2,self.cv=TRUE),
         list(learner= function(X,y) CV.NB.Multinomial.Best.K(X,y,ks=ks), self.cv=TRUE)
         )
  learner2 <- train.NB.normal
  model <- Stacking(X,y,learners.info=learners.info,learner2=learner2)
  list(model=model)
}
