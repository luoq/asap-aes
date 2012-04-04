wordNumber <- function(txt)
  sapply(gregexpr("\\W+",txt), length) + 1
characterNumber <- function(txt)
  nchar(gsub("\\W+","",txt))
