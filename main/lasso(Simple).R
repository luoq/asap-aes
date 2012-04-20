train.main <- function(corpus,feature,Y,Yrange){
  ny <- ncol(Y)
  classifier <- lapply(1:ny,function(k)
                       train.lasso(feature,Y[[k]],Yrange[[k]])
                       )
  return(list(colnames=colnames(Y),classifier=classifier))
}

predict.main <- function(model,corpus,feature){
  K <- length(model$classifier)
  result <- sapply(1:K,function(k)
                   predict(model$classifier[[k]],feature)
  )
  result <- as.data.frame(result)
  colnames(result) <- model$colnames
  result
}
