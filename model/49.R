D <- function(X,y){
  K <- 10
  main.measure <- 2
  learners.info <-
    list(
         list(learner=prebinarizer(
                function(X,y)
                CV.Glmnet.with.NB.2(X,y,cv.ctrl=list(K=K,main.measure=main.measure)))),
#         list(learner=function(X,y)
#             CV.NB.Multinomial.Best.K(X,y,weight.fun="informationGain2",
#                                       ks=4+square.split(ncol(X)-4,100),
#                                       cv.ctrl=list(K=K,main.measure=main.measure))),
#         list(learner=prebinarizer(
#                function(X,y)
#              CV.Glmnet(X,y,glmnet.ctrl=list(alpha=0.8,standardize=FALSE,family="multinomial"),
#                         cv.ctrl=list(K=K,main.measure=main.measure)))),
         list(learner=prebinarizer(
                function(X,y)
                CV(X,y,train.f=function(X,y) train.LiblineaR.model(X,y,type=5,cost=0.1),
                   K=K,main.measure=main.measure)),
              remove.one=FALSE))
  get.Stacking.data(X,y,learners.info=learners.info)
}
CV.D <- function(X,y)
  get.CV.Stacking.data(X,y,D)
