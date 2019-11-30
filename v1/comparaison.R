# Libraries
library(e1071)
library(readxl)
library(dplyr)
library(DataExplorer)

# Sources
source("fit.R")
source("predict.R")


##################################################### IRIS SPECIES ##################################################### 

####### DATA ####### 
set.seed(1)
data(iris)
df <- iris

# Train
train <- sample_n(df, 100, replace=FALSE)

# Test
test <- sample_n(df, 50, replace=FALSE)
Y <- "Species"
test <- select(test, -Y)


####### NBAYES ####### 
print(system.time({
  modele <- fit(Species ~ . , train)
  Mon_Bail <- predict(modele, test, "posterior")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naive_bayes(Species ~ . , train, laplace=1)
  package <- predict(modele, test, "prob")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - package
moy <- lapply(diff, mean)





##################################################### IRIS Sepal.Width ##################################################### 

####### DATA ####### 
df <- read.csv("/Users/d/Cours/SISE_M2/programmation_R/projet/Data/FIFA19.csv", 
                 header=TRUE, sep=",")

df <- select(df, -Photo, -Flag, -Club.Logo, -Value, -Wage, -Release.Clause)
#df$Value = as.numeric(gsub("[\\€]", "", df$Value))
#df$Wage = as.numeric(gsub("\\€", "", df$Wage))
#df$Release.Clause = as.numeric(gsub("\\€", "", df$Release.Clause))
df <- na.omit(df)
introduce(df)

# Train
train <- sample_n(df, 13000, replace=FALSE)

# Test
test <- sample_n(df, 8000, replace=FALSE)
Y <- "Nationality"
test <- select(test, -Y)


####### NBAYES ####### 
print(system.time({
  modele <- fit(Nationality ~ . , train)
  Mon_Bail <- predict(modele, test, "posterior")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naive_bayes(Nationality ~ . , train, laplace=1)
  package <- predict(modele, test, "prob")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - package
moy <- lapply(diff, mean)
print(moy)



