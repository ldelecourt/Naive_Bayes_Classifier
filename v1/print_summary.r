#Surcharge de la m�thode print
print.NBAYES<-function(NBAYES){
  for (i in 1:length(NBAYES$table_proba_cond)){
    cat("Probabilit�s conditionnelles de la variable",names(NBAYES$table_proba_cond[i])," :","\n")
    print(NBAYES$table_proba_cond[[i]])
    cat("\n")
  }
  cat("Probabilit�s � priori :","\n")
  print(NBAYES$prior)
}

#Surcharge de la m�thode summary qui retourne un objet pour acc�der aux diff�rents �l�ments
summary.NBAYES<-function(NBAYES){
  print(NBAYES)
  return(NBAYES)
}
