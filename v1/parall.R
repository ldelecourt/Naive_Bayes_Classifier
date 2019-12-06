proba_1_obs <- function(ligne_df, nbayes) {
  table_conditionnelle <- nbayes$table_proba_cond
  n_mod_predire <- dim(table_conditionnelle[[1]])[2]
  prior <- nbayes$prior
  variable_explicative <- colnames(table_conditionnelle[[1]])
  
  
  # On calcul les prediction sur les données discrétisée
  #proba1 <- seq(1, n_mod_predire)
  proba2 <- seq(1, n_mod_predire)
  for (j in 1:n_mod_predire) {
    P <- prior[j]
    P2 <- log(prior[j])
    for (i in names(table_conditionnelle)) {
      #P <- P * table_conditionnelle[[i]][as.character(ligne_df[i]), j]
      P2 <- P2 + log(table_conditionnelle[[i]][as.character(ligne_df[i]), j])
    }
    #proba1[j] <- P
    proba2[j] <- P2
  }
  #return(proba1)
  return(proba2)
  #return(list(proba1=proba1, proba2=proba2))
}