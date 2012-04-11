### get bayes prob
NBMs <- lapply(1:ny,function(i){
  uniqy <- sort(unique(Y1[[i]]))
  K <- length(uniqy)
  N <- nrow(Y1)
  alpha <- 1/3
  prob1 <- NULL
  prob2 <- NULL
  classifier <- NULL
  for(k in uniqy[1:length(uniqy)-1]){
    y1 <- 1*(Y1[[i]]>k)
    freq <- sum(y1)/N
    if( (freq < 1/K*alpha) || (freq > 1-1/K*alpha))#skip class of too few elements
      next
    label <- paste("NBM",as.character(k),sep="")
    classifier[[label]] <- train.NBM(M1,y1,Yrange[[k]])
    prob1[[label]] <- predict(classifier[[label]],M1,prob=TRUE)[,1]
    prob2[[label]] <- predict(classifier[[label]],M2,prob=TRUE)[,1]
  }
  prob1 <- as.data.frame(prob1)
  prob2 <- as.data.frame(prob2)
  list(classifier=classifier,prob1=prob1,prob2=prob2)
})
