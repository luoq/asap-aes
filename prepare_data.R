train_set <- read.csv("../data/training_set_rel3.csv.gz",stringsAsFactors=FALSE)
train_set$essay_set <- factor(train_set$essay_set)
valid_set <- read.csv("../data/valid_set.csv.gz",stringsAsFactors=FALSE)
valid_set$essay_set <- factor(valid_set$essay_set)
numberOfEssaySet <- 8

require(tm)
Set=vector(mode="list",length=numberOfEssaySet)
for( k in 1:numberOfEssaySet){
  Set[[k]]$corpus1 <- Corpus(VectorSource(with(train_set,essay[essay_set==k])))
  Set[[k]]$corpus2 <- Corpus(VectorSource(with(valid_set,essay[essay_set==k])))
}
for(k in c(1,3,4,5,6)){
  Set[[k]]$Y1 <- data.frame(rubric=with(train_set,domain1_score[essay_set==k]))
}
rm(k)

Set[[2]]$Y1 <- with(train_set,data.frame(rubric1=domain1_score[essay_set==2],rubric2=domain2_score[essay_set==2]))
### sum of 4 traits are final score
Set[[7]]$Y1 <- with(train_set[train_set$essay_set==7,-3],
                         data.frame(Ideas=rater1_trait1+rater2_trait1,
                                    Organization=rater1_trait2+rater2_trait2,
                                    Style=rater1_trait3+rater2_trait3,
                                    Conventions=rater1_trait4+rater2_trait4))
Set[[7]]$resolve_coef <- rep(1,4)
### IdeasAndContent+Organization+SentenceFluency+2*Conventions is final score
Set[[8]]$Y1 <- with(train_set[train_set$essay_set==8,-3],
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
Set[[8]]$resolve_coef <- c(1,1,1,2)

Set[[1]]$Yrange <- data.frame(rubric=c(2,12),row.names=c("min","max"))
Set[[2]]$Yrange <- data.frame(rubric1=c(1,6),rubric2=c(1,4),row.names=c("min","max"))
Set[[3]]$Yrange <- data.frame(rubric=c(0,3),row.names=c("min","max"))
Set[[4]]$Yrange <- data.frame(rubric=c(0,3),row.names=c("min","max"))
Set[[5]]$Yrange <- data.frame(rubric=c(0,4),row.names=c("min","max"))
Set[[6]]$Yrange <- data.frame(rubric=c(0,4),row.names=c("min","max"))
Set[[7]]$Yrange <- as.data.frame(matrix(c(0,6),nrow=2,ncol=4),rownames<-c("min","max"))
colnames(Set[[7]]$Yrange) <- colnames(Set[[7]]$Y1)
Set[[7]]$Yrange$Resolved <- c(0,24) #Description says 0-30
Set[[8]]$Yrange <- as.data.frame(matrix(c(2,12),nrow=2,ncol=4),rownames<-c("min","max"))
colnames(Set[[8]]$Yrange) <- colnames(Set[[8]]$Y1)
Set[[8]]$Yrange$Resolved <- c(10,60) #Description says 0-60

source('data.R')
lapply(1:numberOfEssaySet,splitAndSave.set)
save.image(file="data/orig.Rdata")
