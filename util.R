save.set <- function(k){
  file <- paste(as.character(k),".RData",sep="")
  with(Set[[k]],save(list=ls(),file=file))
}
splitAndSave.set <- function(k,method="random",rho=0.75){
  file <- paste(as.character(k),".RData",sep="")
  with(Set[[k]],{
    n <- nrow(train_score)
    if(method=="sequentail")
      index <- 1:ceiling(n*rho)
    else if(method=="random")
      index <- sort(sample(n,ceiling(n*rho)))
    valid_corpus <- train_corpus[-index]
    train_corpus <- train_corpus[index]
    valid_score <- train_score[-index,]
    train_score <- train_score[-index,]
    save(train_corpus,train_score,valid_corpus,valid_score,index,
         file=file)
  })
}
