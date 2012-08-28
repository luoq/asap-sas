description="Stacking on Glmnet.with.NB, NB.Multinomial, NB.Bernoulli  combined by glmnet"
used.feature <- c(simple=FALSE,dtm=TRUE,corpus=FALSE)
dtm.feature.ctrl <- list(mingram=1,maxgram=3,local_weight="tf",term_weight=NULL)
L <- function(X,y){
  ks=4+square.split(ncol(X)-4,100)
  learners.info <-
    list(
         list(learner=CV.Glmnet.with.NB.2,self.cv=TRUE),
         list(learner= function(X,y) CV.NB.Multinomial.Best.K(X,y,ks=ks), self.cv=TRUE),
         list(learner= function(X,y) CV.NB.Bernoulli.Best.K.2(X,y,ks=ks),self.cv=TRUE)
         )
  learner2 <- function(X,y) CV.Glmnet.with.NB(X,y,glmnet.ctrl=list(alpha=0))$model
  model <- Stacking(X,y,learners.info=learners.info,learner2=learner2)
}
train.model <- function(X,y,K=5)
  CV(X,y,train.f=L)
