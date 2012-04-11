### get term matrix
M1 <- get_dtm(corpus1)
terms <- Terms(M1)
M2 <- get_dtm(corpus2,dictionary=terms)
M1 <- as.Matrix(M1)
M2 <- as.Matrix(M2)
# space <- build_lsa(apply_weight(M1))
# U1 <- space$u
# U2 <- fold_in_lsa(apply_weight(M2),space)
