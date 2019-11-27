library(discretization)

# Creation du modele (fit())

fit <- function(formula, data, laplace=1, ...) {
  # Nom de l'objet en sortie
  NBAYES <- list()
  
  # On extrait le dataframe de l'argument 'formula'
  formula <- as.formula(formula)
  df <- model.frame(formula = formula, data=data)
  
  # On met toutes les colonnes en Factor
  #df <- as.data.frame(lapply(df, factor))
  
  if (all(sapply(data_ALL, class) == "character") == TRUE) {
    # Table frequence variable a predire
    table_Y <- table(df[,1])
    
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
    proba_Y <- prop.table(table_Y + 1)
    NBAYES$prior <- proba_Y
    
    # Donnees a passer dans le predict()
    #NBAYES$discretiz <- TRUE
    #NBAYES$condition <- 
    #NBAYES$n_col_train <- 
    #NBAYES$cuts <- 
    
    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)
    
  } else {
    # discretization des colonnes
    # Do not work if the first column is a class factor
    condition <- class(df[, 1]) == "factor"
    if (condition) {
      #print("La 1ere colonne est de type factor")
      df_disc <- mdlp(df[-1])
      df_disc$Disc.data <- cbind(df[, 1], df_disc$Disc.data)
    } else {
      #print("La 1ere colonne n'est pas de type factor")
      df_disc <- mdlp(data)
    }
    
    #print(df_disc)
    
    # On met toutes les colonnes en Factor
    #df <- as.data.frame(lapply(df, factor))
    
    # Table frequence variable a predire
    table_Y <- table(df_disc$Disc.data[,1])
    
    warning("Laplace smoothing at 1 is used by default !!!")
    for (i in 2:ncol(df_disc$Disc.data)) {
      # Table de frequence variable explicative
      table_freq <- table(df_disc$Disc.data[ ,i], df_disc$Disc.data[ ,1])
      
      # Lissage de Laplace
      table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut
      
      # Probabilite conditionnelle pour toutes les modalités de chaque variable
      proba_explicative <- prop.table(table_freq, 2)
      NBAYES$table_proba_cond[[colnames(df_disc$Disc.data[i])]] <- prop.table(table_freq, 2)
    }
    
    # Probabilites A Priori
    proba_Y <- prop.table(table_Y + 1)
    NBAYES$prior <- proba_Y
    
    # Donnees a passer dans le predict()
    NBAYES$discretiz     <- TRUE
    # Colonnes ayant subit discretisation
    NBAYES$condition     <- which(sapply(df_disc$Disc.data, class) == "integer")
    # Nbre de colonne du jeu de donnee d'entrainement
    NBAYES$n_col_train   <- ncol(df)
    NBAYES$cuts          <- df_disc$cutp
    NBAYES$var_a_predire <- formula[[2]]
      
    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)
  }
}