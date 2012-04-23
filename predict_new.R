new_set <- read.csv("newdata/input.csv.gz",stringsAsFactors=FALSE)
new_set$essay_set <- factor(new_set$essay_set)
numberOfEssaySet <- 8

New=vector(mode="list",length=numberOfEssaySet)
require(tm)
for( k in 1:numberOfEssaySet){
  New[[k]]$corpus <- Corpus(VectorSource(with(new_set,essay[essay_set==k])))
  New[[k]]$feature <- extract.simpleFeatrure(New[[k]]$corpus)
}
New[[7]]$resolve_coef <- rep(1,4)
New[[8]]$resolve_coef <- c(1,1,1,2)

load('model/model.RData')

source('model/lasso(Simple+NBM).R')
Result <- lapply(1:numberOfEssaySet,function(k)
                 with(New[[k]],predict.main(Model[[k]],corpus,feature)))

write.newsubmision <- function(result,path){
  submission <- lapply(1:numberOfEssaySet,function(k){
    if(k!=2)
      id <- with(new_set,domain1_predictionid[essay_set==k])
    else
      id <- with(new_set,c(domain1_predictionid[essay_set==2],
                                        domain2_predictionid[essay_set==2]))
    if(k==2)
      pred <- c(result[[k]][[1]],result[[k]][[2]])
    else if(k==7 | k==8){
      pred <- as.matrix(result[[k]]) %*% New[[k]]$resolve_coef
    }
    else
      pred <- result[[k]][[1]]
    data.frame(prediction_id=id,predicted_score=pred)
  })
  submission <- do.call(rbind,submission)
  submission <- submission[order(submission$prediction_id),]
  write.csv(submission,file=path,quote=FALSE,row.names=FALSE)
}

write.newsubmision(Result,"newdata/output.csv")
