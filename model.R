itrain <- function(...,method)
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
train.RLM <- function(X,y,yrange){
  require(MASS)
  fit <- rlm(y~.,data=cbind(y=y,as.data.frame(X)))
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("RLM",class(result))
  result
}
predict.RLM <- predict.LM
train.LQS <- function(X,y,yrange){
  require(MASS)
  fit <- lqs(y~.,data=cbind(y=y,as.data.frame(X)))
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("RLM",class(result))
  result
}
predict.LQS <- predict.LM
train.lmRob <- function(X,y,yrange){
  require(robust)
  fit <- lmRob(y~.,data=cbind(y=y,as.data.frame(X)))
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("lmRob_model",class(result))
  result
}
predict.lmRob_model <- predict.LM
train.LM2_step <- function(X,y,yrange){
  fit <- lm(y~.^2,data=cbind(y=y,X))
  require(MASS)
  fit <- stepAIC(fit,trace=0)
  result <- list(fit=fit,yrange=yrange)
  class(result) <- c("LM_step",class(result))
  result
}
predict.LM2_step <- predict.LM
train.LM_step_I <- function(X,y,yrange){
  require(MASS)
  cls <- sort(unique(y))
  fit <- lapply(cls,function(C){
    fit <- lm(y~.,data=cbind(y=I(y==C),X))
    fit <- stepAIC(fit,trace=0)
  })
  result <- list(cls=cls,fit=fit)
  class(result) <- c("LM_step_I",class(result))
  result
}
predict.LM_step_I <- function(model,X){
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
train.QDA <- function(X,y,yrange){
  result <- qda(X,y)
  result <- list(model=result)
  class(result) <- c("QDA",class(result))
  result
}
predict.QDA <- predict.LDA
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
    model <- get(fun)(y~.,data=cbind(y=as.factor(y),as.data.frame(X)))
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
cv.lasso <- function(X,y,yrange){
  K <- 10
  n <- nrow(X)
  X <- as.matrix(X)
  all.folds <- split(1:n,rep(1:K,length=n))
  index <- seq(0,1,length=100)
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- lars(X[-omit,,drop=FALSE],y[-omit])
    pred <- predict(fit,X[omit,,drop=FALSE],mode="fraction",s=index)$fit
    pred <- round.range(pred,yrange[1],yrange[2])
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit],yrange[1],yrange[2]))
  })
  kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  index[which.max(kappa)]
}
train.lasso <- function(X,y,yrange){
  require(lars)
  fit <- lars(as.matrix(X),y)
  s <- cv.lasso(as.matrix(X),y,yrange)

  result <- list(fit=fit,yrange=yrange,s=s)
  class(result) <- c("lasso",class(result))
  result
}
predict.lasso <- function(model,X){
  require(lars)
  pred <- predict(model$fit,X,s=model$s,mode="fraction")$fit
  round.range(pred,model$yrange[1],model$yrange[2])
}
train.SVM <- function(X,y,yrange){
  require(e1071)
  model <- svm(X,as.factor(y))

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
train.NBB <- function(X,y,range){
  require(Matrix)
  y <- as.factor(y)
  prior <- table(y)
  X <- (X>0)*1
  A <- t(sapply(levels(y),function(i) y==i)) * 1
  freq <- A %*% X
  alpha <- 0.01
  freq <- Diagonal(x=1/(prior)) %*%  (freq)
  freq <- (1-2*alpha)*freq+alpha
  freq <- as.matrix(freq)
  prior <- prior/sum(prior)
  result <- list(prior=prior,freq=freq,levels=levels(y))
  class(result) <- c("NBB",class(result))
  result
}
predict.NBB <- function(model,X){
  require(Matrix)
  X <- (X>0)*1
  n <- nrow(X)
  logp1 <- t(log(model$freq))
  logp0 <- t(log(1-model$freq))
  L <- X %*% (logp1-logp0)# + rep(1,n) %o% colSums(logp0)
  logprior <- log(model$prior)
  L <- L+(rep(1,n) %o% logprior)
  L <- as.matrix(L)

  result <- apply(L,1,which.max)
  result <- as.numeric(model$levels[result])
}
select.NBM.feature <- function(X,y,yrange){
  ## require(CORElearn)
  K <- 10
  n <- nrow(X)
  all.folds <- kfold(n,K)

  w <- informationGainMultinomial(y,X)
  ## w <- attrEval(y~.,data=cbind(y=y,as.data.frame(as.matrix(X))),estimator="InfGain")
  ord <- order(w,decreasing=TRUE)
  ns <- round(seq(1,length(w),length=100))
  
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    X2 <- X[omit,,drop=FALSE]
    y2 <- y[omit]
    X <- X[-omit,,drop=FALSE]
    y <- y[-omit]
    
    y <- as.factor(y)
    prior <- table(y)
    prior <- prior/sum(prior)
    A <- t(sapply(levels(y),function(i) y==i)) * 1
    laplace <- 1e-4
    freq <- A %*% X
    freq <- as.matrix(freq)
    
    r <- sapply(ns,function(k){
      subset <- ord[1:k]
      freq <- freq[,subset,drop=FALSE]
      
      freq <- Diagonal(x=1/(rowSums(freq)+laplace*ncol(X))) %*% (freq+laplace)
      freq <- as.matrix(freq)
      model <- list(prior=prior,subset=subset,freq=freq,levels=levels(y))
      class(model) <- c("NBM",class(model))

      pred <- predict(model,X2)
      #ScoreQuadraticWeightedR(pred,y2,yrange[1],yrange[2])
      sum(pred==y2)/length(y2)
    })
  })
  kappa <- apply(kappa,1,mean)
  ord[1:ns[which.max(kappa)]]
}
train.NBM <- function(X,y,yrange){
  require(Matrix)
  subset <- select.NBM.feature(X,y,yrange)
  X <- X[,subset,drop=FALSE]
    
  y <- as.factor(y)
  prior <- table(y)
  A <- t(sapply(levels(y),function(i) y==i)) * 1
  laplace <- 1e-4
  freq <- A %*% X
  freq <- as.matrix(freq)
  freq <- Diagonal(x=1/(rowSums(freq)+laplace*ncol(X))) %*% (freq+laplace)
  freq <- as.matrix(freq)
  prior <- prior/sum(prior)
  result <- list(prior=prior,subset=subset,freq=freq,levels=levels(y))
  class(result) <- c("NBM",class(result))
  result
}
predict.NBM <- function(model,X,prob=FALSE){
  require(Matrix)
  X <- X[,model$subset,drop=FALSE]
  n <- nrow(X)
  logp1 <- t(log(model$freq))
  L <- X %*% logp1
  logprior <- log(model$prior)
  L <- L+(rep(1,n) %o% logprior)
  if(prob){
    ## If an element is too large or too small,exp gives bad result
    L <- t(apply(L,1,function(x) x-mean(x)))
    L <- exp(L)
    L <- Diagonal(x=1/rowSums(L)) %*% L
    unname(as.matrix(L))
  }
  else{
    result <- apply(L,1,which.max)
    result <- as.numeric(model$levels[result])
  }
}
cv.lasso_glmnet <- function(X,y,yrange){
  K <- 10
  n <- nrow(X)
  X <- as.matrix(X)
  all.folds <- split(1:n,rep(1:K,length=n))
  index <- seq(0,1,length=100)
  kappa <- sapply(1:K,function(k){
    omit <- all.folds[[k]]
    fit <- glmnet(X[-omit,,drop=FALSE],y[-omit])
    pred <- predict(fit,X[omit,,drop=FALSE],s=index)
    pred <- round.range(pred,yrange[1],yrange[2])
    kappa <- apply(pred,2,function(pred)
                   ScoreQuadraticWeightedKappa(pred,y[omit],yrange[1],yrange[2]))
  })
  kappa <- apply(kappa,1,MeanQuadraticWeightedKappa)
  index[which.max(kappa)]
}
train.glmnet <- function(X,y,yrange){
  require(glmnet)
  X <- as.matrix(X)
  s <- cv.lasso_glmnet(X,y,yrange)
  fit <- glmnet(X,y)

  result <- list(fit=fit,yrange=yrange,s=s)
  class(result) <- c("glmnet_model",class(result))
  result
}
predict.glmnet_model <- function(model,X){
  require(glmnet)
  pred <- predict(model$fit,as.matrix(X),s=model$s)
  round.range(pred,model$yrange[1],model$yrange[2])
}
train.LM_step_S <- function(X,y,yrange){
  cls <- sort(unique(y))
  fit <- lapply(cls[1:length(cls)-1],function(C){
    fit <- lm(y~.,data=cbind(y=I(y<=C),X))
    fit <- stepAIC(fit,trace=0)
  })
  result <- list(cls=cls,fit=fit)
  class(result) <- c("LM_step_S",class(result))
  result
}
predict.LM_step_S <- function(model,X){
  result <- sapply(model$fit,function(fit){
    predict(fit,X)})
  result <- apply(result,1,function (x) which(x>0.5)[1])
  result[is.na(result)] <- length(model$cls)
  result <- model$cls[result]
}
train.LOGIT_S <- function(X,y,yrange){
  cls <- sort(unique(y))
  fit <- lapply(cls[1:length(cls)-1],function(C){
    fit <- glm(y~.,data=cbind(y=I(y<=C),X),family=binomial)
  })
  result <- list(cls=cls,fit=fit)
  class(result) <- c("LOGIT_S",class(result))
  result
}
predict.LOGIT_S <- predict.LM_step_S
train.LOGIT_I <- function(X,y,yrange){
  cls <- sort(unique(y))
  fit <- lapply(cls,function(C){
    fit <- glm(y~.,data=cbind(y=I(y==C),X),family=binomial)
  })
  result <- list(cls=cls,fit=fit)
  class(result) <- c("LOGIT_I",class(result))
  result
}
predict.LOGIT_I <- predict.LM_step_I
predict.KNN <- function(X1,y1,X2){
  Sim <- normalize(X2) %*% t(normalize(X1))
  Ord <- order.rows(Sim)
  Y <- matrix(y1[Ord],nrow=nrow(X2))
  browser()
}
