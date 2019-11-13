#library(readxl)

#  On charge les données
data <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/heart.xls", sheet='dataset')

# On prend que les variables qualitatives pour ces données (Census)
#data <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Census.xlsx", sheet='adult')
#data <- data[c(-1, -9)]



# Création du modèle (fit())

fit <- function(formula, data, laplace=1, ...) {
  # Nom de l'objet en sortie
  NBAYES <- list()
  
  # On extrait le dataframe de l'argument 'formula'
  formula <- as.formula(formula)
  df <- model.frame(formula = formula, data=data)
  
  # On met toutes les colonnes en Factor
  df <- as.data.frame(lapply(df, factor))
  
  # Probabilités des modalités de chaque variable explicative en fonction de la variable expliquée
  list_proba_explicative <- c()
  NBAYES$table_proba_cond <- c()
  for (i in 2:ncol(df)) {
    # Table de fréquence
    table_freq <- table(df[ ,i], df[ ,1])
    # Si une modalité à une fréquence == 0
    if (sum(table_freq == 0 ) > 0) { 
      warning("Laplace smoothing had to be used for variable: '", colnames(df[i]) ,"' !!!\n")
      # Lissage de Laplace
      table_freq[which(table_freq == 0)] <- 1   # On ajoute 1 à la fréquence == 0
    }
    proba_explicative <- prop.table(table_freq, 2)
    #NBAYES$table_proba_cond[i] <- as.matrix(proba_explicative, rownames=rownames(data))
    list_proba_explicative <- rbind(list_proba_explicative, proba_explicative)
    # Modalités des variables explicatives en lignes
    # Somme des proba des modalités d'une même variable = 1
    # P(no/negative) + P(yes/negative) = 1
  }
  
  # Probabilités des modalités de la variable expliquée
  proba_Y <- prop.table(table(df[1]))
  
  # Matrice avec toutes les probabilités conditionnelles + probabilités variables expliquée
  list_proba_all <- rbind(list_proba_explicative, proba_Y)
  
  # On implémente les instances de la classe NBAYES
  NBAYES$list_proba_all <- list_proba_all
  class(NBAYES) <- "NBAYES"
  return(NBAYES)
}


predict <- function(object_NBAYES, data) {       # ajouter une option type('class' ou 'posterior')
  if (class(object_NBAYES) != "NBAYES") {
    stop("The object you gave is not a NBAYES object")
  }
  # Prédiction grâce au modèle (predict())
  proba_finale <- apply(object_NBAYES$list_proba_all, 2, prod)
  # On normalise pour avoir une probabilité 0 < ? < 1
  proba_finale <- proba_finale / sum(proba_finale)
  
  return(list(proba_explicative=object_NBAYES$list_proba_all, proba_finale=proba_finale))
}


modele <- fit(exang ~ . , data)
res <- predict(modele, data)
print(res)
