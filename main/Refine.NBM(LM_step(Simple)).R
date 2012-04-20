train.main2 <- function(feature,M,y,yrange){
  lm.fit <- train.LM_step(feature,y,yrange)
  uniqy <- sort(unique(y))
  refiner <- lapply(uniqy,function(k){
    mask <- abs(y-k)<=1
    term_mask <- colSums(M[mask,])!=0
    M <- M[mask,term_mask]
    y <- y[mask]
    classifier <- train.NBM(M,y,yrange)
    return(list(term_mask=term_mask,classifier=classifier))
  })
  return(list(lm.fit=lm.fit,levels=uniqy,refiner=refiner))
}
predict.main2 <- function(model,feature,M){
  pred <- predict(model$lm.fit,feature)
  uniqy <- sort(unique(pred))
  for(i in uniqy){
    j <- which(model$levels==i)
    mask <- pred==i
    pred[mask] <- predict(model$refiner[[j]]$classifier,
                          M[mask,model$refiner[[j]]$term_mask])
  }
  unname(pred)
}
