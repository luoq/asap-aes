train.main <- function(corpus,feature,Y,Yrange){
  M <- get_dtm(corpus)
  terms <- Terms(M)
  M <- as.Matrix(M)
  ny <- ncol(Y)
  
  NBMs <- lapply(1:ny,function(i){
    uniqy <- sort(unique(Y[[i]]))
    K <- length(uniqy)
    N <- nrow(Y)
    alpha <- 1/3
    prob <- NULL
    classifier <- NULL
    for(k in uniqy[1:length(uniqy)-1]){
      y <- 1*(Y[[i]]>k)
      freq <- sum(y)/N
      if( (freq < 1/K*alpha) || (freq > 1-1/K*alpha))#skip class of too few elements
        next
      label <- paste("NBM",as.character(k),sep="")
      classifier[[label]] <- train.NBM(M,y,Yrange[[i]])
      prob[[label]] <- predict(classifier[[label]],M,prob=TRUE)[,1]
    }
    prob <- as.data.frame(prob)
    list(classifier=classifier,prob=prob)
  })
  NBM_classifier <- lapply(NBMs,function(x) x$classifier)
  
  fit=NULL
  for(k in 1:ny){
    X <- cbind(feature,NBMs[[k]]$prob)
    fit[[k]]   <- train.LM_step(X,Y[[k]],Yrange[[k]])
  }

  classifier <- lapply(1:ny,function(k)
                       list(NBM=NBMs[[k]]$classifier,combinator=fit[[k]]))
  return(list(colnames=colnames(Y),terms=terms,classifier=classifier))
}

predict.main <- function(model,corpus,feature){
  K <- length(model$classifier)
  M <- get_dtm(corpus,dictionary=model$terms)
  M <- as.Matrix(M)

  result <- sapply(1:K,function(k){
    with(model$classifier[[k]],{
      NBM_prob <- NULL
      for(name in names(NBM))
        NBM_prob[[name]] <- predict(NBM[[name]],M,prob=TRUE)[,1]
      X <- cbind(feature,NBM_prob)
      predict(combinator,X)
    })
  })
  result <- as.data.frame(result)
  colnames(result) <- model$colnames
  result
}
