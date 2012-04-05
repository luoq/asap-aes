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
  result <- NULL
  result$Nword <- wordNumber(corpus)
  result$NwordRoot4 <- result$Nword^(1/4)
  result$Nchar <- characterNumber(corpus)
  result$NcharAvg <- with(result,Nchar/Nword)
  result$Nmisspell <- sapply(corpus,misspelledNumber)
  result$Nsent <- sapply(corpus,sentenceNumber)
  result$SentLen <-  with(result,Nword/Nsent)
  as.data.frame(result)
}
