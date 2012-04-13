### get bayes prob
NBMs <- lapply(1:ny,function(i){
  uniqy <- sort(unique(Y1[[i]]))
  K <- length(uniqy)
  N <- nrow(Y1)
  alpha <- 1/3
  prob1 <- NULL
  prob2 <- NULL
  classifier <- NULL
  term_mask <- (colSums(M1)>(nrow(M1)*0.005))
  X1 <- M1[,term_mask]
  X2 <- M2[,term_mask]
  for(k in uniqy[1:length(uniqy)-1]){
    y1 <- 1*(Y1[[i]]>k)
    # freq <- sum(y1)/N
    # if( (freq < 1/K*alpha) || (freq > 1-1/K*alpha))#skip class of too few elements
    #   next
    label <- paste("NBM",as.character(k),sep="")
    classifier[[label]] <- train.NBM(X1,y1,Yrange[[k]])
    prob1[[label]] <- predict(classifier[[label]],X1,prob=TRUE)[,1]
    prob2[[label]] <- predict(classifier[[label]],X2,prob=TRUE)[,1]
  }
  prob1 <- as.data.frame(prob1)
  prob2 <- as.data.frame(prob2)
  list(classifier=classifier,prob1=prob1,prob2=prob2)
})
