splitAndSave.set <- function(k,method="random",rho=0.75){
  file <- data.file(k)
  data <- within(Set[[k]],{
    essay_set <- k
    Ndoc <- nrow(Y1)
    if(method=="sequentail")
      index <- 1:ceiling(Ndoc*rho)
    else if(method=="random")
      index <- sort(sample(Ndoc,ceiling(Ndoc*rho)))
    corpus2 <- corpus1[-index]
    corpus1 <- corpus1[index]
    Y2 <- Y1[-index,,drop=FALSE]
    Y1 <- Y1[index,,drop=FALSE]
  })
  with(data,save(list=ls(),file=file))
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
next.set <- function(){
  if(essay_set<8)
    switch.set(essay_set+1)
  else
    cat("No more sets\n")
}
