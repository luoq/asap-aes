space <- build_lsa(M1[y==max(y),])
U1 <- fold_in_lsa(M1,space)
U2 <- fold_in_lsa(M2,space)
