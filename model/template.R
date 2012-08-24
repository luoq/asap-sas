train.split.f <- function(X,y,
                          split.ctrl=list(train.ratio=0.8,split="random",max.measure="kappa")){
  N <- length(y)
  omit <- switch(split.ctrl$split,
                 "random"=sample(N,ceiling(N*(1-split.ctrl$train.ratio))),
                 "sequential"=rev(seq(N,length=ceiling(N*(1-split.ctrl$train.ratio)),by=-1)))
  X1 <- X[-omit,,drop=FALSE]
  y1 <- y[-omit]
  X2 <- X[omit,,drop=FALSE]
  y2 <- y[omit]
  
  measure <- if(split.ctrl$max.measure=="kappa"){
    apply(pred,2,function(pred)
          precision(pred,y[omit]))
  }
  else if(split.ctrl$max.measure=="precision"){
    apply(pred,2,function(pred)
          ScoreQuadraticWeightedKappa(pred,y[omit]))
  }
  else
    stop("no such measure")
  i <- which.max(measure)

  class(model) <- c("",class(model))
  model
}
