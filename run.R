for(name in c("fit","pred1","pred2","kappa1","kappa2"))
  if(!exists(name))
  assign(name,NULL)

fit[[METHOD]] <- lapply(1:ny,function(k) itrain(X1,Y1[[k]],Yrange[[k]],method=METHOD))
pred1[[METHOD]] <- sapply(1:ny,function(k) predict(fit[[METHOD]][[k]],X1))
pred2[[METHOD]] <- sapply(1:ny,function(k) predict(fit[[METHOD]][[k]],X2))

pred1[[METHOD]] <- as.data.frame(pred1[[METHOD]])
colnames(pred1[[METHOD]]) <- colnames(Y1)
pred2[[METHOD]] <- as.data.frame(pred2[[METHOD]])
colnames(pred2[[METHOD]]) <- colnames(Y2)

kappa1[[METHOD]] <- sapply(1:ny,function(k)
                 ScoreQuadraticWeightedKappa(pred1[[METHOD]][[k]],Y1[[k]],Yrange[1,k],Yrange[2,k]))
kappa2[[METHOD]] <- sapply(1:ny,function(k)
                 ScoreQuadraticWeightedKappa(pred2[[METHOD]][[k]],Y2[[k]],Yrange[1,k],Yrange[2,k]))

if(essay_set>=7){
  for(name in c("y1r","y2r","pred1r","pred2r","kappa1r","kappa2r"))
    if(!exists(name))
      assign(name,NULL)
  y1r[[METHOD]] <- as.matrix(Y1) %*% resolve_coef
  y2r[[METHOD]] <- as.matrix(Y2) %*% resolve_coef
  pred1r[[METHOD]] <- as.matrix(pred1[[METHOD]]) %*% resolve_coef
  pred2r[[METHOD]] <- as.matrix(pred2[[METHOD]]) %*% resolve_coef
  kappa1r[[METHOD]] <- ScoreQuadraticWeightedKappa(pred1r[[METHOD]],y1r[[METHOD]],Yrange$Resolved[1],Yrange$Resolved[2])
  kappa2r[[METHOD]] <- ScoreQuadraticWeightedKappa(pred2r[[METHOD]],y2r[[METHOD]],Yrange$Resolved[1],Yrange$Resolved[2])
}
