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
  evaluate <- function(feature,NBM,y,yrange){
    K <- 5
    n <- nrow(feature)
    all.folds <- split(1:n,rep(1:K,length=n))
    kappa <- sapply(all.folds,function(omit){
      fit1 <- train.LM_step(feature[-omit,,drop=FALSE],y[-omit],yrange)
      pred1 <- predict(fit1,feature[omit,,drop=FALSE])
      kappa1 <- ScoreQuadraticWeightedKappa(pred1,y[omit],yrange[1],yrange[2])
      fit2 <- train.LM_step(cbind(feature[-omit,,drop=FALSE],NBM[-omit,,drop=FALSE]),y[-omit],yrange)
      pred2 <- predict(fit2,cbind(feature[omit,,drop=FALSE],NBM[omit,,drop=FALSE]))
      kappa2 <- ScoreQuadraticWeightedKappa(pred2,y[omit],yrange[1],yrange[2])
      list(kappa1=kappa1,kappa2=kappa2)
    })
    kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
    USE_NBM <- kappa[2]>kappa[1]
  }
  n <- length(y)
  trainI <- sort(sample(n,round(n*0.5)))
  classifier <- get_NBM_classifier(M[trainI,],y[trainI])
  NBM <- get_NBM_prob(classifier,M[-trainI,])
  USE_NBM <- evaluate(feature[-trainI,],NBM,y[-trainI],yrange)
  if(USE_NBM){
    fit <- train.LM_step(cbind(feature[-trainI,],NBM),y[-trainI],yrange)
    return(list(NBM_classifier=classifier,lm.fit=fit))
  }
  else{
    fit <- train.LM_step(feature,y,yrange)
    return(list(NBM_classifier=NULL,lm.fit=fit))
  }
}
predict.main2 <- function(model,feature,M){
  get_NBM_prob <- function(classifier,M){
    result <- sapply(classifier,function(A) predict(A,M,prob=TRUE)[,2])
    result <- as.data.frame(result)
  }
  if(is.null(model$NBM_classifier)){
    pred <- predict(model$lm.fit,feature)
  }
  else{
    NBM <- get_NBM_prob(model$NBM_classifier,M)
    pred <- predict(model$lm.fit,cbind(feature,NBM))
  }
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
  result
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
