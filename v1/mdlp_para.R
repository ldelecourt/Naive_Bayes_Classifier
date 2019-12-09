library(discretization)

discret <- function(colonne, Y) {
  df <- mdlp(cbind(colonne, Y))
  df_disc <- df$Disc.data[-2]
  cuts <- df$cutp
  return(list(df=df_disc, cuts=cuts))
}