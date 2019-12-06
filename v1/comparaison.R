# Libraries
library(e1071)
library(dplyr)
library(DataExplorer)
library(readxl)

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
Y <- test[5]
test <- test[-5]


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
diff <- Mon_Bail$prediction - package
moy <- lapply(diff, mean)
# Matrice de confusion
t1 <- table(Mon_Bail$prediction[,1], Y[,1])
t2 <- table(package, Y[[1]])




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



##################################################### Heart num sex ##################################################### 

####### DATA ####### 
df <- read.csv("/Users/d/Cours/SISE_M2/Data_Sets/heart_num.csv", header=TRUE, sep=",")
introduce(df)

# Train
train <- df[1:200, ]
# Test
test <- df[201:303, ]
Y <- test[1]
test <- test[-1]

age_disc <- mdlp(cbind(df[1:200, 1], df[1:200, 1]))$Disc.data
age_disc[-2]
train[1] <- age_disc[,1]

####### NBAYES ####### 
print(system.time({
  modele <- fit(age ~ . , train)
  Mon_Bail <- predict(modele, test, "class")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naiveBayes(age ~ . , train, laplace=1)
  package <- predict(modele, test, "class")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - as.data.frame(package)
moy <- lapply(diff, mean)
# Matrice de confusion
t1 <- table(Mon_Bail$prediction[,1], Y[,1])
t2 <- table(package, Y[[1]])





##################################################### plalnts ##################################################### 

####### DATA ####### 
df <- read.csv("/Users/d/Cours/SISE_M2/Data_Sets/adult.data", header=FALSE, sep=",")
introduce(df)

# Mix df
df_mix <- sample_n(df, 10000, replace=FALSE)
df_mix <- select(df_mix, -V1, -V3, -V5, -V11, -V12, -V13)
# Train
train <- df_mix[1:7000, ]
# Test
test <- df_mix[7001:10000, ]
Y <- test[5]
test <- test[-5]


####### NBAYES ####### 
print(system.time({
  modele <- fit(V8 ~ . , train)
  Mon_Bail <- predict(modele, test, "class")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naiveBayes(V8 ~ . , train, laplace=1)
  package <- predict(modele, test, "class")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - as.data.frame(package)
moy <- lapply(diff, mean)
# Matrice de confusion
t1 <- table(Mon_Bail$prediction[,1], Y[,1])
t2 <- table(package, Y[[1]])


##################################################### body size ################################################

####### DATA ####### 
df <- read_excel("/Users/d/Cours/SISE_M2/Data_Sets/body_dataset.xlsx", sheet=1)
introduce(df)

# Mix df
df_mix <- sample_n(df, 7000, replace=FALSE)
# Discretisation de poids
poids <- mdlp(cbind(df_mix$poids, df_mix$poids))$Disc.data
colnames(poids) <- c("poids1", "poids2")
poids<- as.data.frame(poids)
df_mix_poids_disc <- df_mix[-13]
df_mix_poids_disc <- cbind(df_mix_poids_disc, poids$poids1)
colnames(df_mix_poids_disc) <- c("epaule", "mollet", "nombril", "hanche", "poignet", "cuisse", "taille", "biceps", "avantbras",
                                 "genou", "poitrine", "cheville", "poidss")

# Train
train <- df_mix_poids_disc[1:2000, ]
# Test
test <- df_mix_poids_disc[2001:3000, ]
Y <- test[13]
test <- test[-13]


####### NBAYES ####### 
print(system.time({
  modele <- fit(poidss ~ . , train)
  Mon_Bail <- predict(modele, test, "class")
}))

library(profvis)
profvis({
  modele <- fit(poidss ~ . , train)
  Mon_Bail <- predict(modele, test, "class")
})


####### PACKAGE ####### 
print(system.time({
  modele <- naiveBayes(poidss ~ . , train, laplace=1)
  package <- predict(modele, test, "class")
}))


####### COMPARAISON ####### 
diff <- Mon_Bail$prediction - as.data.frame(package)
moy <- lapply(diff, mean)
# Matrice de confusion
t1 <- table(Mon_Bail$prediction[,1], Y[,1])
t2 <- table(package, Y[[1]])



##################################################### heart ##################################################### 

####### DATA ####### 
df <- read_excel("/Users/d/Cours/SISE_M2/programmation_R/projet/Data/heart_2.xls", sheet='dataset')

# Train
train <- sample_n(df, 190, replace=FALSE)

# Test
test <- sample_n(df, 90, replace=FALSE)
Y <- test[3]
test <- test[-3]

####### NBAYES ####### 
print(system.time({
  modele <- fit(disease ~ . , train)
  Mon_Bail <- predict(modele, test, "class")
}))


####### PACKAGE ####### 
print(system.time({
  modele <- naiveBayes(disease ~ . , train, laplace=1)
  package <- predict(modele, test, "class")
}))


# Matrice de confusion
t1 <- table(Mon_Bail$prediction[,1], Y[[1]])
t2 <- table(package, Y[[1]])


