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
