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
train.LM_indicator_step <- function(X,y,yrange){
  cls <- sort(unique(y))
  fit <- lapply(1:length(cls),function(C){
    fit <- lm(y~.,data=cbind(y=I(y==C),X))
    require(MASS)
    fit <- stepAIC(fit,trace=0)
  })
  result <- list(cls=cls,fit=fit)
  
  class(result) <- c("LM_indicator_step",class(result))
  result
}
predict.LM_indicator_step <- function(model,X){
  result <- sapply(model$fit,function(fit){
    predict(fit,X)})
  result <- apply(result,1,which.max)
  result <- model$cls[result]
}
train.LDA <- function(X,y,yrange){
  result <- lda(X,y)
  
  result <- list(model=result)
  class(result) <- c("LDA",class(result))
  result
}
predict.LDA <- function(model,X){
  result <- predict(model$model,X)$class
  as.numeric(levels(result)[result])
}
train.lasso <- function(X,y,yrange){
  fit <- lars(as.matrix(X),y)
  cv.fit <- cv.lars(as.matrix(X),y,K=5,plot.it=FALSE)
  s <- select.step(cv.fit$cv,cv.fit$cv.error)
  s <- cv.fit$index[s]
  
  result <- list(fit=fit,yrange=yrange,s=s)
  class(result) <- c("lasso",class(result))
  result
}
predict.lasso <- function(model,X){
  pred <- predict(model$fit,X,s=model$s,mode="fraction")$fit
  round.range(pred,model$yrange[1],model$yrange[2])
}
train.SVM <- function(X,y,yrange){
  y.factor <- as.factor(y)
  require(e1071)
  model <- svm(X,y.factor,kernel="linear",cost=100)
  L <- levels(y.factor)
  
  result <- list(model=model,levels=L)
  class(result) <- c("SVM",class(result))
  result
}
predict.SVM <- function(model,X){
  require(e1071)
  result <- predict(model$model,X)
  as.numeric(model$levels[result])
}
