library(discretization)
source("functions.R")


#### FONCTION PREDICTION POUR UN OBJET NBAYES ####
predict <- function(object_NBAYES, data_test, type="both") {       # ajouter une option type('class' ou 'posterior')
  if (class(object_NBAYES) != "NBAYES") {
    stop("The object you gave is not a NBAYES object")
  }
  
  table_conditionnelle <- object_NBAYES$table_proba_cond
  n_mod_predire <- dim(table_conditionnelle[[1]])[2]
  prior <- object_NBAYES$prior
  nvar <- ncol(data_test)
  variable_explicative <- colnames(table_conditionnelle[[1]])
  pred <- c()
  
  condition1 <- sum(sapply(data_test, class) == "numeric")
  condition2 <-sum(sapply(data_test, class) == "integer")
  
  if ((condition1 > 0) | (condition2 > 0)) {
    # discretization des colonnes ayant ete discretiser dans le fit()
    # la condition permet de ne pas discrétiser la variable a predire qui n'existe pas dans les donnees test
    # si celle-ci a te discretisee
    condition <- object_NBAYES$condition
    var_a_predire <- object_NBAYES$var_a_predire
    
    if (sum(names(condition) == var_a_predire) == 1) {
      condition <- condition[-which(names(condition) == var_a_predire)]
      
      for (j in 1:length(condition)) {
        data_test[names(condition[j])] <- discretisation(data_test[names(condition[j])] , object_NBAYES$cuts[[condition[j]]])
      } 
    } else {
      for (j in 1:length(condition)) {
        data_test[names(condition[j])] <- discretisation(data_test[names(condition[j])] , object_NBAYES$cuts[[condition[j]]])
      }
    }
  
    # On calcul les prediction sur les données discrétisée
    for (k in 1:nrow(data_test)) {
      numerateur <- c()
      for (j in 1:n_mod_predire) {
        P <- 1
        #P2 <- 0
        for (i in names(table_conditionnelle)) {
          P <- P * table_conditionnelle[[i]][data_test[k,i], j]
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
      pred <- rbind(pred, prediction)
    }
    
    pred <- as.data.frame(pred)
    rownames(pred) <- 1:nrow(pred)
    
    # Formatage resultat
    if (type == "class") {
      pred <- pred["class"]
    }
    else if (type == "posterior"){
      pred <- pred[-(ncol(pred))]
    }
    else {
      return(list(prediction=pred)) 
    }
    
    return(list(prediction=pred)) 
  }
    
    ###### ELSE ###### 
   else {

    for (k in 1:nrow(data_test)) {
      numerateur <- c()
      for (j in 1:n_mod_predire) {
        P <- 1
        #P2 <- 0
        for (i in 1:nvar) {
          P <- P * table_conditionnelle[[i]][as.character(data_test[k,i]), j] 
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
      pred <- rbind(pred, prediction)
    }
    
    pred <- as.data.frame(pred)
    rownames(pred) <- 1:nrow(pred)
    
    # Formatage resultat
    if (type == "class") {
      pred <- pred["class"]
    }
    else if (type == "posterior"){
      pred <- pred[-(ncol(pred))]
    }
    else {
      return(list(prediction=pred)) 
    }
    
    return(list(prediction=pred)) 
   }
}

