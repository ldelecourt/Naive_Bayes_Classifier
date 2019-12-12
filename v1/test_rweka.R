library(RWeka)
library(stringr)
source("functions.R")
source("fit.R")

# Data
data(iris)
df <- iris

# Calcul du nombre de colonne a discretiser
condition1 <- sum(sapply(df, class) == "numeric")
condition2 <- sum(sapply(df, class) == "integer")
n_quanti <- condition1 + condition2

# Retrouver les cuts
# data <- c()
# cutp <- c()
# for (j in 1:n_quanti) {
#   for (i in 2:length(unique(disc[,j]))) {
#     cutp[[i-1]] <- sapply(str_extract_all(unique(disc[,j])[i], "-?[0-9.]+"), function(x) max(as.numeric(x)))
#   }
# }
# data$cutp


# discretisation
disc <- Discretize(Species ~ ., data=df)

# Fonction pour récupérer les cuts de Discretize
get_cuts <- function(colonne) {
  cutp <- c()
  print(levels(unique(colonne)))
  for (i in 2:length(unique(colonne))) {
    cutp[[i-1]] <- sapply(str_extract_all(levels(unique(colonne))[i], "-?[0-9.]+"), function(x) max(as.numeric(x)))
  }
  return(cutp)
}

# On récupère les cuts
cuts <- lapply(disc[-ncol(disc)], get_cuts)


modele <- fit(Species ~ ., df)

