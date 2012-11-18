wordNumber <- function(txt)
  sapply(gregexpr("\\w+",txt), length)
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
  Y <- sparseMatrix(nrow=X$nrow,ncol=X$ncol,i=X$i,j=X$j,x=X$v)
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
      if(x[i])
        return(i)
    return(n+1)
  })
add.laplace <- function(x,laplace,n=length(x)){
  (x+laplace)/(sum(x)+n*laplace)
}
cv.kfold.random <- function(n,k){
  ord <- order(runif(n))
  res <- split(ord,rep(1:k,length=n))
  lapply(res,sort)
}
cv.kfold.sequential <- function(n,k)
  split(1:n,rep(1:k,length=n))
cv.kfold.stratified <- function(y,k,f){
  require(permute)
  y <- as.factor(y)
  m <- length(levels(y))
  L <- lapply(levels(y),function(i) which(y==i))
  N <- sapply(L,length)
  S <- lapply(N,function(n) f(n,k))
  res <- lapply(1:k,function(i)
                do.call(c,lapply(1:m,function(j) L[[j]][S[[j]][[i]]])))
  lapply(res,function(x) x[shuffle(length(x))])
}
cv.kfold.stratified.random <- function(y,k) cv.kfold.stratified(y,k,cv.kfold.random)
cv.kfold.stratified.sequential <- function(y,k) cv.kfold.stratified(y,k,cv.kfold.sequential)
square.split <- function(n,k){
  a <- (1:k)^2
  a <- a/a[length(a)]
  x <- ceiling(n*a)
  i <- 1
  ## The leading sequence may keep constant.check it
  while(x[i]>=x[i+1]){
    x[i+1] <- x[i]+1
    i <- i+1
  }
  x
}
weight.split <- function(w,n){
  delta <- sum(w)/n
  S <- vector(mode="numeric",length=n)
  i <- 1
  s <- 0
  a <- delta
  j <- 1
  while(i<=length(w)){
    s <- s+w[i]
    if(s>=a){
      S[j] <- i
      a <- a+delta
      j <- j+1
    }
    i <- i+1
  }
  return(S)
}
entropy <- function(P){
  sum(sapply(P,function(x) -x*log(x)))
}
joint.entropy <- function(C,x,laplace=1e-4){
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
  N <- length(y)
  y <- as.factor(y)
  ny <- table(y)
  H0 <- entropy(add.laplace(ny,2*laplace))#ensure H0-H1>=0
  A <- t(sapply(levels(y),function(i) y==i)) * 1
  freq <- A %*% X
  freq <- as.matrix(freq)
  H1 <- apply(freq,2,function(x){
    sum(x)/N*entropy(add.laplace(x,laplace))+
      (1-sum(x)/N)*entropy(add.laplace(ny-x,laplace))
  })
  H0-H1
}
informationGainMultinomial <- function(y,X,laplace=1e-4){
  y <- as.factor(y)
  A <- t(sapply(levels(y),function(i) y==i)) * 1
  freq <- A %*% X
  freq <- as.matrix(freq)
  ny <- rowSums(freq)
  N <- sum(ny)

  H0 <- entropy(add.laplace(ny,2*laplace))#ensure H0>=H1
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
precision <- function(x,y)
  sum(x==y)/length(x)
df.to.list.of.list <- function(x)
  do.call(function(...) mapply(list,...,SIMPLIFY=FALSE),lapply(x,identity))
list.of.list.to.df <- function(x){
  res <- lapply(1:length(x[[1]]),function(i) sapply(x,function(x) x[[i]]))
  names(res) <- names(x[[1]])
  do.call(function(...) data.frame(...,stringsAsFactors=FALSE),res)
}
direct.prod <- function(...){
  df.to.list.of.list(expand.grid(...,stringsAsFactors=FALSE))
}
correct.count <- function(X,y)
  rowSums(X==outer(y,rep(1,ncol(X))))
majority <- function(X,levels=NULL){
  apply(X,1,function(x){
    tab <- table(x)
    as.numeric(names(tab)[which.max(tab)])
  })
}
binarizer <- function(X) 1*(X!=0)
vote <- function(X,levels=NULL,weight=1:ncol(X)){
  if(is.null(levels))
    levels <- as.numeric(levels(as.factor(X)))
  score <- sapply(levels,function(a) (X==a) %*% weight)
  levels[apply(score,1,which.max)]
}
first.occurrence <- function(x,d){
  n <- length(x)
  f <- rep(NA,d+1)
  f[d+1] <- n+1
  for(i in seq(n,1,-1))
    f[x[i]] <- i
  end <- n+1
  for(i in seq(d,1,-1)){
    if(is.na(f[i]))
      f[i] <- end
    else
      end <- f[i]
  }
  f
}
write.libfm <- function(x,y=NULL,file){
  on.exit(sink())
  x <- as(x,"RsparseMatrix")

  if (!is.null(y) & (length(y) != nrow(x)))
    stop(paste("Length of y (=", length(y),
               ") does not match number of rows of x (=",
               nrow(x), ")!", sep=""))
  sink(file)
  is.last.col.all.zero <- !any(x@j==ncol(x)-1)
  nnz <- length(x@x)
  for (i in 1:nrow(x)) {
    if (!is.null(y)) cat (y[i])
    if ((x@p[i] < nnz) && (x@p[i] < x@p[i + 1])) {
      for (j in x@p[i] : (x@p[i + 1] - 1))
        cat(" ",x@j[j+1], ":", x@x[j+1], sep="")
      if (i==1 && is.last.col.all.zero)
        cat(" ",ncol(x), ":", 0, sep="")
    }
    cat("\n")
  }
}
