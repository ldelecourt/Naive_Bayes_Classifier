library(discretization)
library(foreach)
library(parallel) 
library(doParallel)

source("functions.R")
source("parall.R")

#### FONCTION PREDICTION POUR UN OBJET NBAYES ####
predict.NBAYES <- function(object_NBAYES, data_test, type="both") {       # ajouter une option type('class' ou 'posterior')
  if (class(object_NBAYES) != "NBAYES") {
    stop("The object you gave is not a NBAYES object")
  }
  
  table_conditionnelle <- object_NBAYES$table_proba_cond
  n_mod_predire <- dim(table_conditionnelle[[1]])[2]
  prior <- object_NBAYES$prior
  nvar <- ncol(data_test)
  variable_explicative <- colnames(table_conditionnelle[[1]])
  
  
  condition1 <- sum(sapply(data_test, class) == "numeric")
  condition2 <-sum(sapply(data_test, class) == "integer")
  
  if ((condition1 > 0) | (condition2 > 0)) {
    # discretization des colonnes ayant ete discretiser dans le fit()
    # la condition permet de ne pas discrétiser la variable a predire qui n'existe pas dans les donnees test
    # si celle-ci a te discretisee
    condition <- object_NBAYES$condition
    var_a_predire <- object_NBAYES$var_a_predire
    
    # Initialisation de la parallelisation
    nb_cores <- detectCores()
    cl <- makeCluster(nb_cores)
    registerDoParallel(cl)
    
    if (sum(names(condition) == var_a_predire) == 1) {
      condition <- condition[-which(names(condition) == var_a_predire)]
      
      
      #for (j in 1:length(condition)) {
      #  data_test[names(condition[j])] <- discretisation(data_test[names(condition[j])] , object_NBAYES$cuts[[condition[j]]])
      #} 
      
      data_test <- foreach(i=1:length(condition), .combine=cbind, .export=c("discretisation")) %dopar%
        discretisation(data_test[names(condition[i])] , object_NBAYES$cuts[[condition[i]]])
      stopCluster(cl)
      
    } else {
      
      #for (j in 1:length(condition)) {
      #  data_test[names(condition[j])] <- discretisation(data_test[names(condition[j])] , object_NBAYES$cuts[[condition[j]]])
      #}
      
      data_test <- foreach(i=1:length(condition), .combine=cbind, .export=c("discretisation")) %dopar%
        discretisation(data_test[names(condition[i])] , object_NBAYES$cuts[[condition[i]]])
      stopCluster(cl)
    }
    
    
    # On calcul les prediction sur les données discrétisée
    ###### PARALLELISATION ######
    cl <- makeCluster(nb_cores)
    registerDoParallel(cl)
    pred <- foreach(i=1:nrow(test), .combine=rbind, .export=c("proba_1_obs")) %dopar% proba_1_obs(data_test[i,], object_NBAYES)
    stopCluster(cl)
    
    evidence_par <- seq(1, nrow(pred))
    for (i in 1:nrow(pred)) {
      evidence_par[i] <- abs(sum(pred[i,]))
    }
    
    # Normalisation de la prediction
    pred <- (pred/evidence_par) + 1
    
    # On harmonise le nom des lignes
    pred <- as.data.frame(pred)
    rownames(pred) <- 1:nrow(pred)
    colnames(pred) <- unique(variable_explicative)
    
    # creation de la colonne "class"
    classe <- seq(1, nrow(pred))
    #for (i in 1:nrow(pred)) {
    #  classe[i] <- colnames(pred)[which.max(pred[i,])]
    #}
    
    cl <- makeCluster(nb_cores)
    registerDoParallel(cl)
    classe <- foreach(i=1:nrow(pred), .combine=c, .export=c("proba_1_obs")) %dopar%
      colnames(pred)[which.max(pred[i,])]
    stopCluster(cl)
    
    # Concatene les deux
    pred <- cbind.data.frame(pred, classe)
    colnames(pred)[ncol(pred)] <- "class"
    # We're almost done!
    print("Prediction is done!")
    
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
  
  ###### ELSE ###### Pas besoin de discretiser
  else {
    # On calcul les prediction sur les données discrétisée
    ###### PARALLELISATION ######
    nb_cores <- detectCores()
    cl <- makeCluster(nb_cores)
    registerDoParallel(cl)
    pred <- foreach(i=1:nrow(test), .combine=rbind, .export=c("proba_1_obs")) %dopar% proba_1_obs(data_test[i,], object_NBAYES)
    stopCluster(cl)
    
    evidence_par <- seq(1, nrow(pred))
    for (i in 1:nrow(pred)) {
      evidence_par[i] <- abs(sum(pred[i,]))
    }
    
    # Normalisation de la prediction
    pred <- (pred/evidence_par) + 1
    
    # On harmonise le nom des lignes
    pred <- as.data.frame(pred)
    rownames(pred) <- 1:nrow(pred)
    colnames(pred) <- unique(variable_explicative)
    
    # creation de la colonne "class"
    classe <- seq(1, nrow(pred))
    
    cl <- makeCluster(nb_cores)
    registerDoParallel(cl)
    classe <- foreach(i=1:nrow(pred), .combine=c, .export=c("proba_1_obs")) %dopar%
      colnames(pred)[which.max(pred[i,])]
    stopCluster(cl)
    
    # Concatene les deux
    pred <- cbind.data.frame(pred, classe)
    colnames(pred)[ncol(pred)] <- "class"
    # We're almost done!
    print("Prediction is done!")
    
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

