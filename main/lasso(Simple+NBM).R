train.main2 <- function(feature,M,y,yrange){
  get_NBM_classifier <- function(M,y){
    uniqy <- sort(unique(y))
    ## alpha <- 1/3
    classifier <- lapply(uniqy[1:(length(uniqy)-1)],function(k){
      y <- 1*(y<=k)
      ## freq <- sum(y)/length(y)
      ## if( (freq < 1/K*alpha) || (freq > 1-1/K*alpha))#skip class of too few elements
      ##   next
      train.NBM(M,y,yrange)
    })
    names(classifier) <- sapply(uniqy[1:(length(uniqy)-1)],function(k) paste("NBM",as.character(k),sep=""))
    classifier
  }
  get_NBM_prob <- function(classifier,M){
    result <- sapply(classifier,function(A) predict(A,M,prob=TRUE)[,2])
    result <- as.data.frame(result)
  }
  n <- length(y)
  trainI <- sort(sample(n,round(n*0.5)))
  classifier <- get_NBM_classifier(M[trainI,],y[trainI])
  NBM <- get_NBM_prob(classifier,M[-trainI,])
  fit <- train.lasso(cbind(feature[-trainI,],NBM),y[-trainI],yrange)
  return(list(NBM_classifier=classifier,lm.fit=fit))
}
predict.main2 <- function(model,feature,M){
  get_NBM_prob <- function(classifier,M){
    result <- sapply(classifier,function(A) predict(A,M,prob=TRUE)[,2])
    result <- as.data.frame(result)
  }
  NBM <- get_NBM_prob(model$NBM_classifier,M)
  pred <- predict(model$lm.fit,cbind(feature,NBM))
  unname(pred)
}
train.main1 <- function(feature,M,Y,Yrange){
  ny <- ncol(Y)
  classifier <- lapply(1:ny,function(i){
    train.main2(feature,M,Y[[i]],Yrange[[i]])
  })
  return(list(colnames=colnames(Y),classifier=classifier))
}
train.main <- function(corpus,feature,Y,Yrange){
  M <- get_dtm(corpus)
  terms <- Terms(M)
  M <- as.Matrix(M)
  result <- train.main1(feature,M,Y,Yrange)
  result$terms <- terms
}
predict.main1 <- function(model,feature,M){
  result <- sapply(model$classifier,function(model){
    predict.main2(model,feature,M)
  })
  result <- as.data.frame(result)
  colnames(result) <- model$colnames
  result
}
predict.main <- function(model,corpus,feature){
  K <- length(model$classifier)
  M <- get_dtm(corpus,dictionary=model$terms)
  M <- as.Matrix(M)
  predict.main1(model,feature,M)
}
