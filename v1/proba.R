sproba <- function(data_test, n_mod_predire, table_conditionnelle, variable_explicative, prior) {
  # On calcul les prediction sur les données discrétisée
    numerateur <- c()
    for (j in 1:n_mod_predire) {
      P <- 1
      #P2 <- 0
      for (i in names(table_conditionnelle)) {
        P <- P * table_conditionnelle[[i]][data_test[,i], j]
        #P2 <- P2 + table_conditionnelle[[i]][as.character(data_test[k,i]), j] 
      }
      numerateur[j] <- P * prior[j]
      #numerateur[j] <- log(P2) + log(prior[j])
    }
    
    
    evidence <- sum(numerateur)
    
    # posterior:
    prediction <- numerateur / evidence  # no evidence if log()
    prediction <- rbind(prediction)
    
    colnames(prediction) <- unique(variable_explicative)
    # class:
    class <- unique(variable_explicative)[which.max(prediction)]
    prediction <- cbind.data.frame(prediction, class)
    return(prediction)
}




