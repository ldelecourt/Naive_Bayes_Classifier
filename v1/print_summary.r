#Surcharge de la méthode print
print.NBAYES<-function(NBAYES){
  for (i in 1:length(NBAYES$table_proba_cond)){
    cat("Probabilités conditionnelles de la variable",names(NBAYES$table_proba_cond[i])," :","\n")
    print(NBAYES$table_proba_cond[[i]])
    cat("\n")
  }
  cat("Probabilités à priori :","\n")
  print(NBAYES$prior)
}

#Surcharge de la méthode summary qui retourne un objet pour accéder aux différents éléments
summary.NBAYES<-function(NBAYES){
  print(NBAYES)
  return(NBAYES)
}
