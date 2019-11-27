# File containing several functions created for Naive Bayes Classifier

# Discretisation à partir d'une discretisation déja faites sur un autre jeu de donnees
discretisation <- function(column, cuts) {
  nombre_cut_total <- length(cuts)
  
  for (i in 1:nrow(column)) {
    
    for (k in 1:nombre_cut_total) {
      if (column[i,1] < cuts[1]) {
        column[i,1] <- 1
        break
      
      } else if (column[i,1] > cuts[nombre_cut_total]) {
        column[i,1] <- nombre_cut_total + 1
        break
      
      } else { for (j in 1:nombre_cut_total-1) {
        if ((cuts[j] < column[i,1]) && (column[i,1] < cuts[j+1])) {
          column[i,1] <- j+1
          break
        }
      }
        break
      }
    }
    #print(column[i,1])
  }
  return(column)
}

