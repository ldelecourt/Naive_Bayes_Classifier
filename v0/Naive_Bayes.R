library(readxl)

#  On charge les donn?es
data <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/heart.xls", sheet='dataset')

# On prend que les variables qualitatives pour ces donn?es (Census)
#data <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Census.xlsx", sheet='adult')
#data <- data[c(-1, -9)]



# Cr?ation du mod?le (fit())

fit <- function(formula, data, laplace=1, ...) {
  # Nom de l'objet en sortie
  NBAYES <- list()
  
  # On extrait le dataframe de l'argument 'formula'
  formula <- as.formula(formula)
  df <- model.frame(formula = formula, data=data)
  
  # On met toutes les colonnes en Factor
  df <- as.data.frame(lapply(df, factor))
  
  # Probabilit?s des modalit?s de chaque variable explicative en fonction de la variable expliqu?e
  list_proba_explicative <- c()
  #NBAYES$table_proba_cond <- c()
  
  for (i in 2:ncol(df)) {
    # Table de fr?quence
    table_freq <- table(df[ ,i], df[ ,1])
    # Si une modalit? ? une fr?quence == 0
    if (sum(table_freq == 0 ) > 0) { 
      warning("Laplace smoothing had to be used for variable: '", colnames(df[i]) ,"' !!!\n")
      # Lissage de Laplace
      table_freq[which(table_freq == 0)] <- 1   # On ajoute 1 ? la fr?quence == 0
    }
    proba_explicative <- prop.table(table_freq, 2)
    
    NBAYES$table_proba_cond[[colnames(df[i])]] <- prop.table(table_freq, 2)
    
    #NBAYES$table_proba_cond[i] <- as.matrix(proba_explicative, rownames=rownames(data))
    list_proba_explicative <- rbind(list_proba_explicative, proba_explicative)
    # Modalit?s des variables explicatives en lignes
    # Somme des proba des modalit?s d'une m?me variable = 1
    # P(no/negative) + P(yes/negative) = 1
  }
  
  # Probabilit?s des modalit?s de la variable expliqu?e
  proba_Y <- prop.table(table(df[1]))
  NBAYES$table_explicative <- proba_Y
  
  # Matrice avec toutes les probabilit?s conditionnelles + probabilit?s variables expliqu?e
  list_proba_all <- rbind(list_proba_explicative, proba_Y)
    
  # On impl?mente les instances de la classe NBAYES
  NBAYES$list_proba_all <- list_proba_all
  class(NBAYES) <- "NBAYES"
  return(NBAYES)
}


predict <- function(object_NBAYES, data) {       # ajouter une option type('class' ou 'posterior')
  if (class(object_NBAYES) != "NBAYES") {
    stop("The object you gave is not a NBAYES object")
  }
  # Pr?diction gr?ce au mod?le (predict())
  proba_finale <- apply(object_NBAYES$list_proba_all, 2, prod)
  # On normalise pour avoir une probabilit? 0 < ? < 1
  proba_finale <- proba_finale / sum(proba_finale)
  
  return(list(table_cond=object_NBAYES$table_proba_cond, table_explicative=object_NBAYES$table_explicative, proba_finale=proba_finale)) 
  #proba_explicative=object_NBAYES$list_proba_all,
}


modele <- fit(exang ~ . , data)
res <- predict(modele, data)
print(res)