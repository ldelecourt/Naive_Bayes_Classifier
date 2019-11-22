library(readxl)


#  On charge les donnees
data_ALL <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Data/heart_2.xls", sheet='dataset')

# On prend que les variables qualitatives pour ces donn?es (Census)
#data <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Data/Census.xlsx", sheet='adult')
#data <- data[c(-1, -9)]



# Creation du modele (fit())

fit <- function(formula, data, laplace=1, ...) {
  # Nom de l'objet en sortie
  NBAYES <- list()
  
  # On extrait le dataframe de l'argument 'formula'
  formula <- as.formula(formula)
  df <- model.frame(formula = formula, data=data)
  
  # On met toutes les colonnes en Factor
  df <- as.data.frame(lapply(df, factor))
  
  warning("Laplace smoothing at 1 is used by default !!!")
  for (i in 2:ncol(df)) {
    # Table de frequence variable explicative
    table_freq <- table(df[ ,i], df[ ,1])
      
    # Lissage de Laplace
    table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut
    
    # Probabilite conditionnelle pour toutes les modalités de chaque variable
    proba_explicative <- prop.table(table_freq, 2)
    NBAYES$table_proba_cond[[colnames(df[i])]] <- prop.table(table_freq, 2)
  }
  
  # Probabilites A Priori
  proba_Y <- prop.table(table(df[,1]) + 1)
  NBAYES$prior <- proba_Y
  
  # On nomme l'objet NBAYES
  class(NBAYES) <- "NBAYES"
  return(NBAYES)
}


predict <- function(object_NBAYES, data_test, type="both") {       # ajouter une option type('class' ou 'posterior')
  if (class(object_NBAYES) != "NBAYES") {
    stop("The object you gave is not a NBAYES object")
  }
  
  table_conditionnelle <- object_NBAYES$table_proba_cond
  n_mod_predire <- length(rownames(table_conditionnelle[[1]]))
  prior <- object_NBAYES$prior
  nvar <- ncol(data_test)
  variable_explicative <- colnames(table_conditionnelle[[1]])
  pred <- c()
  
  for (k in 1:nrow(data_test)) {
    numerateur <- c()
    for (j in 1:n_mod_predire) {
      P <- 1
      for (i in 1:nvar) {
        P <- P * table_conditionnelle[[i]][as.character(data_test[k,i]), j] 
      }
      numerateur[j] <- P * prior[j]
    }
    
    evidence <- sum(numerateur)
    
    # posterior:
    prediction <- numerateur / evidence
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


###### TEST ######
data_train <- data_ALL
data_test <- data_ALL[sample(nrow(data_ALL), 5), 1:2]

modele <- fit(disease ~ . , data_train)
res <- predict(modele, data_test, type="posterior")
print(res)



