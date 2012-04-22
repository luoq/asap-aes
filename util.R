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
