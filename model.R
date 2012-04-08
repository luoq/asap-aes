train <- function(...,method)
  get(paste("train.",method,sep=""))(...)
train.LM <- function(X,y,yrange){
  fit <- lm(y~.,data=cbind(y=y,X))
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("LM",class(result))
  result
}
predict.LM <- function(model,X){
  round.range(predict(model$fit,X),model$yrange[1],model$yrange[2])
}
train.LM_step <- function(X,y,yrange){
  fit <- lm(y~.,data=cbind(y=y,X))
  require(MASS)
  fit <- stepAIC(fit,trace=0)
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("LM_step",class(result))
  result
}
predict.LM_step <- predict.LM
train.lasso <- function(X,y,yrange){
  cv.fit <- cv.lars(as.matrix(X),y,K=5,mode="step")
  fit <- lars(as.matrix(X),y)
  index <- select.step(cv.fit$cv,cv.fit$cv.error)
  
  result <- list(fit=fit,yrange=yrange,index=index)
  class(result) <- c("lasso",class(result))
  result
}
predict.lasso <- function(model,X){
  pred <- predict(model$fit,X,s=model$index)$fit
  round.range(pred,model$yrange[1],model$yrange[2])
}
