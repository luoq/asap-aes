### Read data file.Get train_corpus,valid_corpus,train_score
train_set <- read.csv("../data/training_set_rel3.csv.gz",stringsAsFactors=FALSE)
train_set$essay_set <- factor(train_set$essay_set)
valid_set <- read.csv("../data/valid_set.csv.gz",stringsAsFactors=FALSE)
valid_set$essay_set <- factor(valid_set$essay_set)
numberOfEssaySet <- 8

require(tm)
Set=vector(mode="list",length=numberOfEssaySet)
for( k in 1:numberOfEssaySet){
  Set[[k]]$train_corpus <- Corpus(VectorSource(with(train_set,essay[essay_set==k])))
  Set[[k]]$valid_corpus <- Corpus(VectorSource(with(valid_set,essay[essay_set==k])))
}
for(k in c(1,3,4,5,6)){
  Set[[k]]$train_score <- data.frame(rubric=with(train_set,domain1_score[essay_set==k]))
}

Set[[2]]$train_score <- with(train_set,data.frame(rubric1=domain1_score[essay_set==2],rubric2=domain2_score[essay_set==2]))
### sum of 4 traits are final score
Set[[7]]$train_score <- with(train_set[train_set$essay_set==7,-3],
                         data.frame(Ideas=rater1_trait1+rater2_trait1,
                                    Organization=rater1_trait2+rater2_trait2,
                                    Style=rater1_trait3+rater2_trait3,
                                    Conventions=rater1_trait4+rater2_trait4))
### IdeasAndContent+Organization+SentenceFluency+2*Conventions is final score
Set[[8]]$train_score <- with(train_set[train_set$essay_set==8,-3],
                         local({
                           result <- matrix(0,nrow=length(rater1_trait1),ncol=4)
                           mask <- is.na(rater3_trait1)
                           result[mask,] <- cbind(rater1_trait1[mask]+rater2_trait1[mask],rater1_trait2[mask]+rater2_trait2[mask],
                                                  rater1_trait5[mask]+rater2_trait5[mask],rater1_trait6[mask]+rater2_trait6[mask])
                           result[!mask,] <- 
                             2*cbind(rater3_trait1[!mask],rater3_trait2[!mask],rater3_trait5[!mask],rater3_trait6[!mask])
                           result <- as.data.frame(result)
                           names(result) <- c("IdeasAndContent","Organization","SentenceFluency","Conventions")
                           result
                         }))

rm(train_set,valid_set,k)
