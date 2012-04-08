wordNumber <- function(txt)
  sapply(gregexpr("\\W+",txt), length) + 1
characterNumber <- function(txt)
  nchar(gsub("\\W+","",txt))
misspelledNumber <- function(txt){
  require(Aspell)
  n <- 0
  spellDoc(txt,function (...) n <<- n+1)
  n
}
misspelledWords <- function(txt){
  require(Aspell)
  words <- NULL
  spellDoc(txt,function (word,...) words <<- c(words,word))
  words
}
sentenceNumber <- function(txt){
  require('openNLP')
  length(sentDetect(txt))
}
extract.simpleFeatrure <- function(corpus){
  preprocess_corpus <- function(corpus){
    corpus <- tm_map(corpus,function(txt) gsub("[!(),\\./:;?]"," ",txt))
    corpus <- tm_map(corpus,removePunctuation)
  }
  result <- NULL
  result$Nword <- wordNumber(corpus)
  result$NwordRoot4 <- result$Nword^(1/4)
  result$Nchar <- characterNumber(corpus)
  result$NcharAvg <- with(result,Nchar/Nword)
  result$Nmisspell <- sapply(corpus,misspelledNumber)
  result$Nsent <- sapply(corpus,sentenceNumber)
  result$SentLen <-  with(result,Nword/Nsent)

  dtm <- DocumentTermMatrix(preprocess_corpus(corpus))
  result$DiffWord <- apply(dtm>0,1,sum)
  terms <- Terms(dtm)
  for(i in 5:10){
    mask <- (nchar(terms)>=i)
    result[[paste("W",as.character(i),sep="")]] <-
      apply(dtm[,mask],1,sum)
  }
  
  as.data.frame(result)
}
get_dtm <- function(corpus,dictionary=NULL){
  preprocess_corpus <- function(corpus){
    ## :punct: = [!"#$%&'()*+,\-./:;<=>?@[\\\]^_`{|}~]
    ## these punctuation may be between two words
    corpus <- tm_map(corpus,function(txt) gsub("[!(),\\./:;?]"," ",txt))
    ## NOTE:termFreq tokenize before removePunctuation.
    corpus <- tm_map(corpus,removePunctuation)
    corpus <- tm_map(corpus,removeNumbers)
  }
  corpus <- preprocess_corpus(corpus)
  if(is.null(dictionary))
    ctrl <- list(stemming=TRUE,
                 bounds=list(global=c(4,Inf)))# about half the terms only belong to one document
  else
    ctrl <- list(stemming=TRUE,dictionary=dictionary)
    
  dtm <- DocumentTermMatrix(corpus,control=ctrl)
}
