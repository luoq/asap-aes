train.main2 <- function(feature,M,y,yrange){
  lm.fit <- train.LM_step(feature,y,yrange)
  uniqy <- sort(unique(y))
  refiner <- lapply(1:(length(uniqy)-1),function(i){
    if(uniqy[i+1]!=uniqy[i]+1)
      return(NULL)
    mask <- (y==uniqy[i]) | (y==uniqy[i]+1)
    term_mask <- colSums(M[mask,])!=0
    M <- M[mask,term_mask]
    y <- y[mask]
    classifier <- train.NBM(M,y,yrange)
    return(list(term_mask=term_mask,classifier=classifier))
  })
  return(list(lm.fit=lm.fit,levels=uniqy,refiner=refiner))
}
predict.main2 <- function(model,feature,M,alpha1=0.9,alpha2=0.9){
  pred <- predict(model$lm.fit,feature)
  uniqy <- sort(unique(pred))
  for(i in uniqy){
    j <- which(model$levels==i)
    if(length(j)==0)
      next
    mask <- pred==i
    if(j>1){
      if(!is.null(model$refiner[[j-1]])){
        prob1 <- predict(model$refiner[[j-1]]$classifier,
                              M[mask,model$refiner[[j-1]]$term_mask],prob=TRUE)
        prob1 <- prob1[,1]
        pred[mask][prob1>alpha1] <- i-1
      }
    }
    if(j<length(model$levels)){
      if(!is.null(model$refiner[[j]])){
        prob2 <- predict(model$refiner[[j]]$classifier,
                        M[mask,model$refiner[[j]]$term_mask],prob=TRUE)
        prob2 <- prob2[,2]
        pred[mask][prob2>alpha2] <- i+1
      }
    }
  }
  unname(pred)
}
