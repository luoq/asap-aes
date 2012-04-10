train <- function(...,method)
  get(paste("train.",method,sep=""))(...)
train.LM <- function(X,y,yrange){
  fit <- lm(y~.,data=cbind(y=y,as.data.frame(X)))
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("LM",class(result))
  result
}
predict.LM <- function(model,X){
  result <- predict(model$fit,as.data.frame(X))
  res <<- unname(result)
  round.range(result,model$yrange[1],model$yrange[2])
}
train.LM_step <- function(X,y,yrange){
  fit <- lm(y~.,data=cbind(y=y,as.data.frame(X)))
  require(MASS)
  fit <- stepAIC(fit,trace=0)
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("LM_step",class(result))
  result
}
predict.LM_step <- predict.LM
train.LM2_step <- function(X,y,yrange){
  fit <- lm(y~.^2,data=cbind(y=y,X))
  require(MASS)
  fit <- stepAIC(fit,trace=0)
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("LM_step",class(result))
  result
}
predict.LM2_step <- predict.LM
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
  factor2numeric(result)
}
train.SLDA <- function(X,y,yrange){
  result <- slda(y~.,cbind(y=as.factor(y),as.data.frame(X)))
  
  result <- list(model=result)
  class(result) <- c("SLDA",class(result))
  result
}
predict.SLDA <- function(model,X){
  result <- predict(model$model,as.data.frame(X))$class
  factor2numeric(result)
}
train.RWeka <- function(fun){
  function(X,y,yrange){
    require(RWeka)
    model <- get(fun)(y~.,data=cbind(y=as.factor(y),X))
    result <- list(model=model)
    class(result) <- c(paste(fun,"_model",sep=""),class(result))
    result
  }
}
predict.RWeka <- function(model,X){
  require(RWeka)
  factor2numeric(predict(model$model,X))
}
train.J48 <- train.RWeka("J48")
predict.J48_model <- predict.RWeka
train.LMT <- train.RWeka("LMT")
predict.LMT_model <- predict.RWeka
train.M5P <- train.RWeka("M5P")
predict.M5P_model <- predict.RWeka
train.DecisionStump <- train.RWeka("DecisionStump")
predict.DecisionStump_model <- predict.RWeka
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
  require(e1071)
  model <- svm(X,as.factor(y),kernel="linear",cost=100)
  
  result <- list(model=model)
  class(result) <- c("SVM",class(result))
  result
}
predict.SVM <- function(model,X){
  require(e1071)
  factor2numeric(predict(model$model,X))
}
train.randomForest <- function(X,y,yrange){
  require(randomForest)
  model <- randomForest(X,as.factor(y))

  result <- list(model=model)
  class(result) <- c("randomForest_model",class(result))
  result
}
predict.randomForest_model <- function(model,X){
  require(randomForest)
  factor2numeric(predict(model$model,X))
}
