train <- function(...,method)
  get(paste("train.",method,sep=""))(...)
train.LM <- function(X,y,yrange){
  fit <- lm(y~.,data=cbind(y=y,X))
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("LM",class(result))
  result
}
predict.LM <- function(LM,X){
  round.range(predict(LM$fit,X),LM$yrange[1],LM$yrange[2])
}
