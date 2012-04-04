### Read data file.Get train_corpus,valid_corpus,train_score
train_set <- read.csv("../data/training_set_rel3.csv.gz",stringsAsFactors=FALSE)
train_set$essay_set <- factor(train_set$essay_set)
valid_set <- read.csv("../data/valid_set.csv.gz",stringsAsFactors=FALSE)
valid_set$essay_set <- factor(valid_set$essay_set)
numberOfEssaySet <- 8

require(tm)
train_corpus <- lapply(1:numberOfEssaySet,function(k)
                       Corpus(VectorSource(with(train_set,essay[essay_set==k]))))
valid_corpus <- lapply(1:numberOfEssaySet,function(k)
                       Corpus(VectorSource(with(valid_set,essay[essay_set==k]))))
train_score <- NULL
train_score[[1]] <- with(train_set,domain1_score[essay_set==1])
train_score[[2]] <- with(train_set,cbind(domain1_score[essay_set==2],domain2_score[essay_set==2]))
colnames(train_score[[2]]) <- c("domain1","domain2")
train_score[[3]] <- with(train_set,domain1_score[essay_set==3])
train_score[[4]] <- with(train_set,domain1_score[essay_set==4])
train_score[[5]] <- with(train_set,domain1_score[essay_set==5])
train_score[[6]] <- with(train_set,domain1_score[essay_set==6])
train_score[[7]] <- with(train_set[train_set$essay_set==7,-3],cbind(rater1_trait1+rater2_trait1,
                                     rater1_trait2+rater2_trait2,rater1_trait3+rater2_trait3,rater1_trait4+rater2_trait4))
colnames(train_score[[7]]) <- c("Ideas","Organization","Style","Conventions")
train_score[[8]] <- with(train_set[train_set$essay_set==8,-3],
                         local({
                           result <- matrix(0,nrow=length(rater1_trait1),ncol=4)
                           mask <- is.na(rater3_trait1)
                           result[mask,] <- cbind(rater1_trait1[mask]+rater2_trait1[mask],rater1_trait2[mask]+rater2_trait2[mask],
                                                  rater1_trait5[mask]+rater2_trait5[mask],rater1_trait6[mask]+rater2_trait6[mask])
                           result[!mask,] <- 
                             2*cbind(rater3_trait1[!mask],rater3_trait2[!mask],rater3_trait5[!mask],rater3_trait6[!mask])
                           result
                         }))
colnames(train_score[[8]]) <- c("IdeasAndContent","Organization","SentenceFluency","Conventions")
