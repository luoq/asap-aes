round.range <- function(x,min,max) {
  x<-round(x)
  x[x>max] <- max
  x[x<min] <- min
  return(x)
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
  Y <- as.simple_triplet_matrix(X)
  names(dimnames(Y)) <- c("Docs","Terms")
  class(Y) <- c("DocumentTermMatrix", "simple_triplet_matrix")
  Y
}
