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
  modele <- naiveBayes(Species ~ . , train, laplace=1)
  package <- predict(modele, test, "raw")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - package
moy <- lapply(diff, mean)





##################################################### FIFA19 Nationality ################################################ 

####### DATA ####### 
df <- read.csv("/Users/d/Cours/SISE_M2/programmation_R/projet/Data/FIFA19.csv", 
                 header=TRUE, sep=",")

df <- select(df, -Photo, -Flag, -Club.Logo, -Value, -Wage, -Release.Clause)
#df$Value = as.numeric(gsub("[\\€]", "", df$Value))
#df$Wage = as.numeric(gsub("\\€", "", df$Wage))
#df$Release.Clause = as.numeric(gsub("\\€", "", df$Release.Clause))
df <- na.omit(df)
introduce(df)

df <- select(df, Name, Age, Nationality, Overall, Potential, Club ,Special, Preferred.Foot, International.Reputation,
             LS, ST, RS, LW, Weak.Foot)

# Train
train <- sample_n(df, 500, replace=FALSE)

# Test
test <- sample_n(df, 200, replace=FALSE)
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
  package <- predict(modele, test, "class")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - package
moy <- lapply(diff, mean)
print(moy)


##################################################### foot_results city ################################################## 

####### DATA ####### 
df <- read.csv("/Users/d/Cours/SISE_M2/Data_Sets/foot_results.csv", 
               header=TRUE, sep=",")
df <- df[-1]
introduce(df)

# Train
train <- df[1:3000, ]

# Test
test <- df[10000:10300, ]
Y <- "city"
test <- select(test, -Y)


####### NBAYES ####### 
print(system.time({
  modele <- fit(city ~ . , train)
  Mon_Bail <- predict(modele, test, "posterior")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naive_bayes(city ~ . , train, laplace=1)
  package <- predict(modele, test, "prob")
}))



####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - package
moy <- lapply(diff, mean)




##################################################### RABBIT Animal ##################################################### 

####### DATA ####### 
data(Rabbit)
df <- Rabbit
introduce(df)

# Train
train <- sample_n(df, 40, replace=FALSE)

# Test
test <- sample_n(df, 20, replace=FALSE)
Y <- "Animal"
test <- select(test, -Y)


####### NBAYES ####### 
print(system.time({
  modele <- fit(Animal ~ . , train)
  Mon_Bail <- predict(modele, test, "posterior")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naive_bayes(Animal ~ . , train, laplace=1)
  package <- predict(modele, test, "prob")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - as.data.frame(package)
moy <- lapply(diff, mean)




##################################################### Aids2 T.categ ##################################################### 

####### DATA ####### 
data(Aids2)
df <- Aids2
introduce(df)

# Train
train <- sample_n(df, 90, replace=FALSE)

# Test
test <- sample_n(df, 40, replace=FALSE)
Y <- "T.categ"
test <- select(test, -Y)


####### NBAYES ####### 
print(system.time({
  modele <- fit(T.categ ~ . , train)
  Mon_Bail <- predict(modele, test, "posterior")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naive_bayes(T.categ ~ . , train, laplace=1)
  package <- predict(modele, test, "prob")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - as.data.frame(package)
moy <- lapply(diff, mean)




##################################################### Fish Species ##################################################### 

####### DATA ####### 
df <- read.csv("/Users/d/Cours/SISE_M2/Data_Sets/Fish.csv", 
               header=TRUE, sep=",")
introduce(df)
df <- sample_n(df, 142, replace=FALSE)

# Train
train <- df[1:90, ]

# Test
test <- df[91:142, ]
test <- test[-1]


####### NBAYES ####### 
print(system.time({
  modele <- fit(Species ~ . , train)
  Mon_Bail <- predict(modele, test, "class")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naiveBayes(Species ~ . , train, laplace=1)
  package <- predict(modele, test, "class")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - as.data.frame(package)
moy <- lapply(diff, mean)


t2 <- table(package, df[91:142, 1])

