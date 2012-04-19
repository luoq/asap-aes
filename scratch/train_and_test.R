##METHOD controls method
##USE_NBM control whether NBM prob will be used
for(name in c("fit","pred1","pred2","kappa1","kappa2")){
  if(!exists(name))
    assign(name,NULL)
}

for(k in 1:ny){
  XX1 <- X1
  XX2 <- X2
  if(USE_NBM){
    XX1 <- cbind(X1,NBMs[[k]]$prob1)
    XX2 <- cbind(X2,NBMs[[k]]$prob2)
  }
  fit[[METHOD]][[k]]   <- itrain(XX1,Y1[[k]],Yrange[[k]],method=METHOD)
  pred1[[METHOD]][[k]] <- predict(fit[[METHOD]][[k]],XX1)
  pred2[[METHOD]][[k]] <- predict(fit[[METHOD]][[k]],XX2)
  rm(XX1,XX2)
}
pred1[[METHOD]] <- as.data.frame(pred1[[METHOD]])
colnames(pred1[[METHOD]]) <- colnames(Y1)
pred2[[METHOD]] <- as.data.frame(pred2[[METHOD]])
colnames(pred2[[METHOD]]) <- colnames(Y2)

kappa1[[METHOD]] <- sapply(1:ny,function(k)
                           ScoreQuadraticWeightedKappa(pred1[[METHOD]][[k]],Y1[[k]],Yrange[1,k],Yrange[2,k]))
kappa2[[METHOD]] <- sapply(1:ny,function(k)
                           ScoreQuadraticWeightedKappa(pred2[[METHOD]][[k]],Y2[[k]],Yrange[1,k],Yrange[2,k]))
err1[[METHOD]] <- sapply(1:ny,function(k)
                         1-sum(pred1[[METHOD]][[k]]==Y1[[k]])/length(Y1[[k]]))
err2[[METHOD]] <- sapply(1:ny,function(k)
                         1-sum(pred2[[METHOD]][[k]]==Y2[[k]])/length(Y2[[k]]))
adj_err1[[METHOD]] <- sapply(1:ny,function(k)
                             1-sum(abs(pred1[[METHOD]][[k]]-Y1[[k]])<=1)/length(Y1[[k]]))
adj_err2[[METHOD]] <- sapply(1:ny,function(k)
                             1-sum(abs(pred2[[METHOD]][[k]]-Y2[[k]])<=1)/length(Y2[[k]]))

if(essay_set>=7){
  for(name in c("y1r","y2r","pred1r","pred2r","kappa1r","kappa2r")){
    if(!exists(name))
      assign(name,NULL)
  }
  y1r <- as.matrix(Y1) %*% resolve_coef
  y2r <- as.matrix(Y2) %*% resolve_coef
  pred1r[[METHOD]] <- as.matrix(pred1[[METHOD]]) %*% resolve_coef
  pred2r[[METHOD]] <- as.matrix(pred2[[METHOD]]) %*% resolve_coef
  kappa1r[[METHOD]] <- ScoreQuadraticWeightedKappa(pred1r[[METHOD]],y1r,Yrange$Resolved[1],Yrange$Resolved[2])
  kappa2r[[METHOD]] <- ScoreQuadraticWeightedKappa(pred2r[[METHOD]],y2r,Yrange$Resolved[1],Yrange$Resolved[2])

  err1r[[METHOD]] <-1-sum(pred1r[[METHOD]]==y1r)/length(y1r)
  err2r[[METHOD]] <-1-sum(pred2r[[METHOD]]==y2r)/length(y2r)
  adj_err1r[[METHOD]] <- 1-sum(abs(pred1r[[METHOD]]-y1r)<=1)/length(y1r)
  adj_err2r[[METHOD]] <- 1-sum(abs(pred2r[[METHOD]]-y2r)<=1)/length(y2r)
}
