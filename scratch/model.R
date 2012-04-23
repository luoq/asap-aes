set.seed(2012)
for(name in c("model","pred","pred","Kappa")){
  if(!exists(name))
    assign(name,NULL)
}
for(k in 1:ny){
  model[[k]] <- train.main2(feature1,M1,Y1[[k]],Yrange[[k]])
  pred[[k]] <- predict.main2(model[[k]],feature2,M2)
  Kappa[[k]] <- ScoreQuadraticWeightedKappa(pred[[k]],Y2[[k]])
}
if(essay_set>=7){
  for(name in c("pred_r","Kappa_r")){
    if(!exists(name))
      assign(name,NULL)
  }
  pred <- do.call(cbind,pred)
  pred_r <- as.matrix(pred) %*% resolve_coef
  Kappa_r <- ScoreQuadraticWeightedKappa(y2r,pred_r,Yrange$Resolved[1],Yrange$Resolved[2])
}
