save.set <- function(k){
  file <- paste(as.character(k),".RData",sep="")
  with(Set[[k]],save(list=ls(),file=file))
}
splitAndSave.set <- function(k,method="random",rho=0.75){
  file <- paste(as.character(k),".RData",sep="")
  with(Set[[k]],{
    n <- nrow(Y1)
    if(method=="sequentail")
      index <- 1:ceiling(n*rho)
    else if(method=="random")
      index <- sort(sample(n,ceiling(n*rho)))
    corpus2 <- corpus1[-index]
    corpus1 <- corpus1[index]
    Y2 <- Y1[-index,,drop=FALSE]
    Y1 <- Y1[index,,drop=FALSE]
    save(corpus1,Y1,corpus2,Y2,index,Yrange,file=file)
  })
}
