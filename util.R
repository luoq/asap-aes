round.range <- function(x,min,max) {
  x<-round(x)
  x[x>max] <- max
  x[x<min] <- min
  return(x)
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

data.file <- function(k)
  paste("data/",as.character(k),".RData",sep="")
save.set <- function()
  save(list=ls.nofunction(),file=data.file(essay_set))
ls.nofunction <- function() {
  names <- ls(envir=.GlobalEnv)
  mask <- sapply(names,function(name) is.function(get(name)))
  return(names[!mask])
}
switch.set <- function(k){
  if(k!=essay_set){
    save.set()
    rm(list=ls.nofunction(),envir=.GlobalEnv)
    load(data.file(k),envir=.GlobalEnv)
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
