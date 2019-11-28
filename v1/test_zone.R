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
#data_ALL <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Data/heart_2.xls", sheet='dataset')
#data_ALL <- iris
data_ALL <- mtcars

data_train <- data_ALL
data_test <- data_ALL[-4]
#data_test <- data_ALL[sample(nrow(data_ALL), 10), 1:4]


###### TEST ######

print(system.time({
  modele <- fit(hp ~ . , data_train)
  res <- predict(modele, data_test)
  print(res)
}))


# Commentaires:
# --> fit() fonctionne bien avec tous types de variables (full quali, full quanti, mixte)
# --> predict() fonctionne pour données full quali, full quanti
# --> predict() pour Sepal.Width

# --> predict() pas pour Species, pb mdlp() place des varibles
