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
    feature2 <- feature1[-index,,drop=FALSE]
    feature1 <- feature1[index,,drop=FALSE]
    Y2 <- Y1[-index,,drop=FALSE]
    Y1 <- Y1[index,,drop=FALSE]
  })
  with(data,save(list=ls(),file=file))
}
data.file <- function(k)
  paste("data/",as.character(k),".RData",sep="")
save.set <- function()
  save(list=ls.nofunction(),file=data.file(essay_set))
save.all <- function()
  save(list=ls.nofunction(),file="data/all.RData")
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
write.submision <- function(result=Results[[length(Results)]],
                            name=as.character(length(Results))){
  submission <- lapply(1:numberOfEssaySet,function(k){
    if(k!=2)
      id <- with(valid_set,domain1_predictionid[essay_set==k])
    else
      id <- with(valid_set,c(domain1_predictionid[essay_set==2],
                                        domain2_predictionid[essay_set==2]))
    if(k==2)
      pred <- c(result[[k]][[1]],result[[k]][[2]])
    else if(k==7 | k==8){
      pred <- as.matrix(result[[k]]) %*% Set[[k]]$resolve_coef
    }
    else
      pred <- result[[k]][[1]]
    data.frame(prediction_id=id,predicted_score=pred)
  })
  submission <- do.call(rbind,submission)
  submission <- submission[order(submission$prediction_id),]
  write.csv(submission,
            ,file=paste("submission/",name,".csv",sep="")
            ,quote=FALSE,row.names=FALSE)
}
