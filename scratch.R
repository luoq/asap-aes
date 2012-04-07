#ny <- length(Y1)
#feature1 <- extract.simpleFeatrure(corpus1)
#feature2 <- extract.simpleFeatrure(corpus2)

fit <- lapply(1:ny,function(k) train(feature1,Y1[[k]],Yrange[[k]],method=METHOD))
pred1 <- sapply(1:ny,function(k) predict(fit[[k]],feature1))
pred2 <- sapply(1:ny,function(k) predict(fit[[k]],feature2))

pred1 <- as.data.frame(pred1)
colnames(pred1) <- colnames(Y1)
pred2 <- as.data.frame(pred2)
colnames(pred2) <- colnames(Y2)

kappa1 <- sapply(1:ny,function(k)
                 ScoreQuadraticWeightedKappa(pred1[[k]],Y1[[k]],Yrange[1,k],Yrange[2,k]))
kappa2 <- sapply(1:ny,function(k)
                 ScoreQuadraticWeightedKappa(pred2[[k]],Y2[[k]],Yrange[1,k],Yrange[2,k]))

if(essay_set>=7){
  y1r <- as.matrix(Y1) %*% resolve_coef
  y2r <- as.matrix(Y2) %*% resolve_coef
  pred1r <- as.matrix(pred1) %*% resolve_coef
  pred2r <- as.matrix(pred2) %*% resolve_coef
  kappa1r <- ScoreQuadraticWeightedKappa(pred1r,y1r,Yrange$Resolved[1],Yrange$Resolved[2])
  kappa2r <- ScoreQuadraticWeightedKappa(pred2r,y2r,Yrange$Resolved[1],Yrange$Resolved[2])
}
