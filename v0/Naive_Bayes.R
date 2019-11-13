# On charge les données
#library(readxl)
#data <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/heart.xls", sheet='dataset')
#data <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Census.xlsx", sheet='adult')

# On prend que les variables qualitatives pour ce test
#data <- data[c(-1, -9)]

# Création du modèle (fit())

# On definit la variable expliquée (à prédire)
Nom_Variable_A_Predire <- "sex"

# Numero de colonne de la variable expliquée
for (i in 1:ncol(data)) {
  if (colnames(data[i]) == Nom_Variable_A_Predire) {
    Num_Colonne_A_Predire = i
  }
}

# On met toutes les colonnes en Factor
data <- as.data.frame(lapply(data, factor))

# Probabilités des modalités de chaque variable explicative en fonction de la variable expliquée
list_proba_explicative <- c()
for (i in 1:ncol(data)) {
  if (i != Num_Colonne_A_Predire) {
    # Table de fréquence
    table_freq <- table(data[ ,i], data[ ,Num_Colonne_A_Predire])
    # Si une modalité à une fréquence == 0
    if (sum(table_freq == 0 ) > 0) { 
      # Lissage de Laplace
      table_freq[which(table_freq == 0)] <- 1   # On ajoute 1 à la fréquence == 0
    }
    proba_explicative <- prop.table(table_freq, 2)
    list_proba_explicative <- rbind(list_proba_explicative, proba_explicative)
    # Modalités des variables explicatives en lignes
    # Somme des proba des modalités d'une même variable = 1
    # P(no/negative) + P(yes/negative) = 1
  }
}

# Probabilités des modalités de la variable expliquée
proba_Y <- prop.table(table(data[Num_Colonne_A_Predire]))

# Matrice avec toutes les probabilités conditionnelles + probabilités variables expliquée
list_proba_all <- rbind(list_proba_explicative, proba_Y)



# Prédiction grâce au modèle (predict())
proba_finale <- apply(list_proba_all, 2, prod)
# On normalise pour avoir une probabilité 0 < ? < 1
proba_finale <- proba_finale / sum(proba_finale)


print(list_proba_all)
print(proba_finale)
print(head(sort(proba_finale, decreasing=TRUE), 5))