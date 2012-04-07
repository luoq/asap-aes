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
