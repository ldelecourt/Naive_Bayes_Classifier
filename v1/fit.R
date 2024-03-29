library(discretization)
library(RWeka)
source("mdlp_para.R")
source("cuts_from_Discretize.R")

# Creation du modele (fit())

fit <- function(formula, data, laplace=1, parallel=FALSE, mdlp=FALSE, discretize=TRUE) {
  # Nom de l'objet en sortie
  NBAYES <- list()

  # On extrait le dataframe de l'argument 'formula'
  # en supprimant les lignes comportant des NA
  formula <- as.formula(formula)
  df <- na.omit(model.frame(formula = formula, data=data))

  # Nombre de colonne df
  n_var <- ncol(df)
  # variable a predire
  Y <- df[1]
  # On supprime Y de df
  df <- df[-1]

  # on discretize la variable a predire si besoin
  if (class(Y[,1]) == "numeric" || class(Y[,1]) == "integer") {
    Y <- mdlp(cbind(Y, Y))$Disc.data[-2]
  }


  # Variable quanti
  condition1 <- sapply(df, class) == "numeric"
  condition2 <- sapply(df, class) == "integer"
  if ((sum(condition1) > 0 | sum(condition2)) > 0) {
    quanti <- df[c(which(condition1))]
    quanti <- cbind(quanti, df[c(which(condition2))])
  }

  # Variable quali
  condition1 <- sapply(df, class) == "character"
  condition2 <- sapply(df, class) == "factor"
  condition3 <- sapply(df, class) == "logic"

  if (exists('quanti') && is.data.frame(get('quanti'))) {
    quali <- df[c(which(condition1))]
    quali <- cbind(quali, df[c(which(condition2))])
    quali <- cbind(quali, df[c(which(condition3))])
  } else {
    quali <- df
  }

  # On met toutes les colonnes en Factor
  #df <- as.data.frame(lapply(df, factor))

  if (length(quali) == n_var-1) {
    # On recree le df
    quali <- cbind(Y, quali)
    # Table frequence variable a predire
    table_Y <- table(quali[ ,1])

    warning("Laplace smoothing at 1 is used by default !!!")
    for (i in 2:ncol(quali)) {
      # Table de frequence variable explicative
      table_freq <- table(quali[ ,i], quali[ ,1])

      # Lissage de Laplace
      table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut

      # Probabilite conditionnelle pour toutes les modalités de chaque variable
      proba_explicative <- prop.table(table_freq, 2)
      NBAYES$table_proba_cond[[colnames(quali[i])]] <- prop.table(table_freq, 2)
    }

    # Probabilites A Priori
    proba_Y <- prop.table(table_Y + 1)
    NBAYES$prior <- proba_Y

    # We're almost done
    print("Fitting is done!")

    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)


    #### ELSE ####
  } else {
    if (parallel==FALSE & mdlp==TRUE) {
      # Discretisaion des variables numeric
      quanti <- cbind(quanti, Y)
      df_disc <- mdlp(quanti)

      # On concatene les quanti et quali pour retrouver le DF originel discretise
      df_disc$Disc.data <- cbind(df_disc$Disc.data, quali)

      # Colonnes ayant subit discretisation
      NBAYES$condition <- which(sapply(df_disc$Disc.data, class) == "integer")

    } else if (parallel==TRUE & mdlp==TRUE) {
      nb_cores <- detectCores()
      cl <- makeCluster(nb_cores)
      registerDoParallel(cl)
      quanti_discret <- foreach(i=1:ncol(quanti), .combine="c", .export=c("discret", "mdlp")) %dopar%
        discret(quanti[,i], Y)
      stopCluster(cl)

      # Pour récupérer les cuts
      df_discret <- df[1]
      df_discret <- df_discret[-1]
      cuts <- seq(2, length(quanti_discret), by=2)
      for (i in cuts) {
        cuts[i/2] <- quanti_discret[[i]]
        df_discret <- cbind(df_discret, quanti_discret[[i-1]])
      }
      colnames(df_discret) <- colnames(quanti)

      # On crée une liste contenant le dataframe discrétisé et les cuts
      df_disc <- c()
      df_disc[["Disc.data"]] <- df_discret
      df_disc[["cutp"]] <- cuts

      # On concatene les quanti et quali
      df_disc$Disc.data <- cbind(df_disc$Disc.data, quali)
      df_disc$Disc.data <- cbind(df_disc$Disc.data, Y)

      # Colonnes ayant subit discretisation
      NBAYES$condition <- which(sapply(df_disc$Disc.data, class) == "integer")

    } else if (parallel==FALSE & discretize==TRUE) {
      # Noms des colonnes subissant la discretisation
      NBAYES$condition <- rbind(c(1:ncol(quanti)))
      colnames(NBAYES$condition) <- colnames(quanti)

      # On discretise
      quanti <- Discretize(as.formula(paste(names(Y), "~ .")), cbind(quanti, Y))
      # On récupère les cuts
      cuts <- lapply(quanti[-ncol(quanti)], get_cuts)

      # On crée une liste contenant le dataframe discrétisé et les cuts
      df_disc <- c()
      df_disc[["Disc.data"]] <- quanti
      df_disc[["cutp"]] <- cuts

      # On concatene les quanti et quali
      df_disc$Disc.data <- cbind(df_disc$Disc.data, quali)
    }


    # Table frequence variable a predire
    table_Y <- table(Y)

    warning("Laplace smoothing at 1 is used by default !!!")
    for (i in which(names(df_disc$Disc.data) != names(Y))) {
      # Table de frequence variable explicative
      table_freq <- table(df_disc$Disc.data[ ,i], Y[,1])

      # Lissage de Laplace
      table_freq <-  table_freq + laplace   # On ajoute laplace=1 par défaut

      # Probabilite conditionnelle pour toutes les modalités de chaque variable
      proba_explicative <- prop.table(table_freq, 2)
      NBAYES$table_proba_cond[[colnames(df_disc$Disc.data[i])]] <- prop.table(table_freq, 2)
    }

    # Probabilites A Priori
    proba_Y <- prop.table(table_Y + 1)
    NBAYES$prior <- proba_Y

    # Donnees a passer dans le predict()
    NBAYES$discretiz     <- TRUE
    # Nbre de colonne du jeu de donnee d'entrainement
    NBAYES$n_col_train   <- ncol(df)
    NBAYES$cuts          <- df_disc$cutp
    NBAYES$var_a_predire <- formula[[2]]

    # We're almost done
    print("Fitting is done!")

    # On nomme l'objet NBAYES
    class(NBAYES) <- "NBAYES"
    return(NBAYES)
  }
}


