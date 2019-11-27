# ZONE DE TEST POUR CLASSIFIEUR BAYESIEN

###### CALL LIBRARIES AND FUNCTIONS ######
library(readxl)

source("fit.R")
source("predict.R")
#source("functions.R")


###### DATA ######
data(mtcars)
data(iris)

#  On charge les donnees
data_ALL <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Data/heart_2.xls", sheet='dataset')
#data_ALL <- iris
#data_ALL <- mtcars

data_train <- data_ALL
data_test <- data_ALL[-1]
#data_test <- data_ALL[sample(nrow(data_ALL), 5), 2:]


###### TEST ######

print(system.time({
  modele <- fit(disease ~ . , data_train)
  print(modele$condition)
  print(modele$cuts)
  res <- predict(modele, data_test)
  #print(res)
}))


# Commentaires:
# --> fit() fonctionne bien avec tous types de variables (full quali, full quanti)
# --> fit() pour donner mixte mdlp() considere les colonnes a discretiser en premier 
# --> predict() fonctionne pour données full quali
# --> predict() fonctionne pour données full quanti
# --> predict() fonction pour mixte, si une colonne a discretiser est donné en formula sinon bug
# --> predict() pb données mixte lorsque l'on donne le meme nombre de variable en train et test
 
  
