### get bayes prob
X1 <- M1
X2 <- M2
uniqy <- sort(unique(Y1[[1]]))
NBM_prob1=NULL              
NBM_prob2=NULL              
for(k in uniqy[1:length(uniqy)-1]){
  y1 <- 1*(Y1[[1]]>k)
  y2 <- 1*(Y2[[1]]>k)
  NBM_classifier <- train.NBM(X1,y1,Yrange[[1]])
  NBM_prob1[[paste("NBM",as.character(k),sep="")]] <-
    predict(NBM_classifier,X1,prob=TRUE)[,1]
  NBM_prob2[[paste("NBM",as.character(k),sep="")]] <-
    predict(NBM_classifier,X2,prob=TRUE)[,1]
}
NBM_prob1 <- as.data.frame(NBM_prob1)
NBM_prob2 <- as.data.frame(NBM_prob2)
rm(y1,y2,k,uniqy)
X1 <- cbind(feature1,NBM_prob1)
X2 <- cbind(feature2,NBM_prob2)
