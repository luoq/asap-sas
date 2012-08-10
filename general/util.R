round.range <- function(x,min,max) {
  x<-round(x)
  x[x>max] <- max
  x[x<min] <- min
  return(x)
}
round.residual <- function(x){
  mask <- x<0
  x <- abs(x)
  mask1 <- x<0.5
  x[mask1] <- 0
  x[!mask1] <- ceiling(x[!mask1]-0.5)
  x[mask] <- -x[mask]
  x
}
which.kmax <- function(x,k){
  if(k==1)
    return(which.max(x))
  else{
    res <- rep(0,k)
    res[1] <- which.max(x)
    temp <- which.kmax(x[-res[1]],k-1)
    mask <- temp>=res[1]
    temp[mask] <- temp[mask]+1
    res[2:k] <- temp
    return(res)
  }
}
factor2numeric <- function(F)
  as.numeric(levels(F)[F])
order.rows <- function(X)
  t(apply(X,1,order,decreasing=TRUE))
barplot.compare <- function(a,b){
  tab <- table(a,b)
  barplot(tab,col=rainbow(5),legend=rownames(tab))
}
normalize <- function(X){
  L <- sqrt(rowSums(X^2))
  X <- Diagonal(x=1/L) %*% X
  X
}
dim_share <- function(share=0.5){
  function(x) {
    s=sum(x)
    d=0
    i=1
    while(d<share){
      d=d+x[i]/s
      i=i+1
    }
    return(i-1)
  }
}

select.step <- function(cv,cv.error){
  k <- which.min(cv)
  cv.min <- cv[k]
  cv.sd <- cv.error[k]
  for(i in 1:length(cv))
    if(cv[i]>=cv.min-cv.sd && cv[i]<=cv.min+cv.sd)
      return(i)
}

as.Matrix <- function(X)
  UseMethod("as.Matrix")
as.Matrix.DocumentTermMatrix <- function(X){
  require(Matrix)
  Y <- spMatrix(nrow=X$nrow,ncol=X$ncol,i=X$i,j=X$j,x=X$v)
  dimnames(Y) <- list(X$dimnames$Docs,X$dimnames$Terms)
  return(Y)
}
as.dtm <- function(X){
  require(slam)
  Y <- as.simple_triplet_matrix(X)
  names(dimnames(Y)) <- c("Docs","Terms")
  class(Y) <- c("DocumentTermMatrix", "simple_triplet_matrix")
  Y
}
first.true.index <- function(M)
  apply(M,1,function(x){
    n <- length(x)
    for(i in 1:n)
      if(x[i]>0.5)
        return(i)
    return(n+1)
  })
add.laplace <- function(x,laplace,n=length(x)){
  (x+laplace)/(sum(x)+n*laplace)
}
kfold <- function(n,k){
  ord <- order(runif(n))
  split(ord,rep(1:k,length=n))
}
square.split <- function(n,k){
  a <- (1:k)^2
  a <- a/a[length(a)]
  x <- ceiling(n*a)
  i <- 1
  ## The leading sequence may keep constant.check it
  while(x[i]==x[i+1]){
    x[i+1] <- x[i+1]+1
    i <- i+1
  }
  x
}
entropy <- function(P){
  sum(sapply(P,function(x) -x*log(x)))
}
joint.entropy <- function(C,x){
  laplace <- 1e-4
  C <- factor(C)#ensure C[x==i] has same levels as C
  x <- factor(x)
  P <- prop.table(table(x))
  H <- sapply(levels(x),function(i) entropy(add.laplace(table(C[x==i]),laplace)))
  H <- crossprod(P,H)
}
informationGain <- function(C,X){
  H0 <- entropy(prop.table(table(C)))
  m <- ncol(X)
  H1 <- sapply(1:m,function(j) joint.entropy(C,X[,j]))
  H <- H0-H1
  names(H) <- colnames(X)
  H
}
informationGain2 <- function(y,X,laplace=1e-4){#X is binary
  X <- X!=0
  N <- length(y)
  y <- as.factor(y)
  ny <- table(y)
  H0 <- entropy(prop.table(ny))
  A <- t(sapply(levels(y),function(i) y==i)) * 1
  freq <- A %*% X
  freq <- as.matrix(freq)
  H1 <- apply(freq,2,function(x){
    sum(x)/N*entropy(add.laplace(x,laplace))+
      (1-sum(x)/N)*entropy(add.laplace(ny-x,laplace))
  })
  H0-H1
}
informationGainMultinomial <- function(y,X){
  laplace <- 1e-4
  y <- as.factor(y)
  A <- t(sapply(levels(y),function(i) y==i)) * 1
  freq <- A %*% X
  freq <- as.matrix(freq)
  ny <- rowSums(freq)
  N <- sum(ny)

  H0 <- entropy(prop.table(ny))
  H1 <- apply(freq,2,function(x){
    sum(x)/N*entropy(add.laplace(x,laplace))+
      (1-sum(x)/N)*entropy(add.laplace(ny-x,laplace))
  })
  H0-H1
}
ls.nofunction <- function() {
  names <- ls(envir=.GlobalEnv)
  mask <- sapply(names,function(name) is.function(get(name)))
  return(names[!mask])
}
nspace <- function(x){
  start <- gregexpr(" ",x)[[1]]
  if(length(start)==1 && start==-1)
    return(0)
  else
    return(length(start))
}
