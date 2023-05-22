library(tidyverse)
library(plotly)
library(dplyr)
library(lubridate)
database <- read.csv("C:/Users/Emilie/documents/ISEN 2022/stat_acc_V3.csv", header = TRUE, sep = ";")

#Suppression des lignes ne contenant aucunes valeurs
Accidents_no_NA <- na.omit(database)
#summary(Accidents_no_NA)

#On remarque la présence de valeurs maximales absurdes concernant la longitude et la latitude
#Exemple 1: Ligne 3683 -> longitude > 90
#Exemple 2: Ligne 3684 -> latitude > 90

#On remarque aussi que certains accidents possèdent un nombre de place NULL :
#Exemple 3: Ligne 52127 -> place = NULL

#Supression des lignes contenant des valeurs absurdes en suivant la condition suivante :
Condition <- Accidents_no_NA$longitude < -90 | Accidents_no_NA$longitude > 90 | Accidents_no_NA$latitude < -90 | Accidents_no_NA$latitude > 90 | Accidents_no_NA$place == 'NULL'
database <- subset(Accidents_no_NA, !Condition)
#summary(database)

fichier_text <- function(contenu,chemin_du_fichier){
  # Vérifier si le fichier existe
  if (file.exists(chemin_du_fichier)) {
    # Si le fichier existe, ajouter le contenu à la fin
    writeLines(contenu, chemin_du_fichier)
  } else {
    # Si le fichier n'existe pas, le créer et y écrire le contenu
    writeLines(contenu, chemin_du_fichier)
  }
}




valeur_num_type <- function(database, col_name) {
  
  table_types <- table(database[[col_name]])
  print(table_types)
  
  types_uniques <- unique(database[[col_name]])
  chiffres_associes <- numeric(length(types_uniques))
  
  for (i in 1:length(types_uniques)) {
    chiffres_associes[i] <- i
  }
  
  correspondance <- data.frame(Type = types_uniques, Chiffre = chiffres_associes)
  
  # Ajout dans un fichier texte des types et de leurs chiffres associés
  correspondance_text <- paste(correspondance$Chiffre, correspondance$Type)
  # création d'un fichier text contenant les types et leur chiffre  associé :
  #-------CHANGER LE CHEMIN D'ACCES-----------------
  write(correspondance_text, file = "C:/Users/Emilie/documents/ISEN 2022/type.txt", append = TRUE, sep = "\n")
  
  for (i in 1:length(database[[col_name]])) {
    database[[col_name]][i] <- chiffres_associes[match(database[[col_name]][i], types_uniques)]
  }
  variables_numeriques <- c(col_name)
  database[variables_numeriques] <- lapply(database[variables_numeriques], as.numeric)
  
  return(database)
}

E1 <- valeur_num_type(database, "descr_cat_veh")
E1 <- valeur_num_type(E1, "descr_agglo")
E1 <- valeur_num_type(E1, "descr_athmo")
E1 <- valeur_num_type(E1, "descr_lum")
E1 <- valeur_num_type(E1, "descr_etat_surf")
E1 <- valeur_num_type(E1, "description_intersection")
E1 <- valeur_num_type(E1, "descr_dispo_secu")
E1 <- valeur_num_type(E1, "descr_grav")
E1 <- valeur_num_type(E1, "descr_motif_traj")
E1 <- valeur_num_type(E1, "descr_type_col")

#convertir en num
suppressWarnings({

variables_numeriques <- c("age", "place", "an_nais", "id_code_insee", "id_usa")
E1[variables_numeriques] <- lapply(E1[variables_numeriques], as.numeric)
})

# Convertir les variables de date en format date
variables_dates <- c("date")
E1[variables_dates] <- lapply(E1[variables_dates], as.Date)

library(lubridate)

construire_series_chronologiques <- function(data) {
  
  # Agréger par mois
  accidents_par_mois <- data %>%
    mutate(Mois = floor_date(date, "month")) %>%
    group_by(Mois) %>%
    summarise(Nombre_accidents = n())
  
  # Agréger par semaine
  accidents_par_semaine <- data %>%
    mutate(Semaine = floor_date(date, "week")) %>%
    group_by(Semaine) %>%
    summarise(Nombre_accidents = n())
  
  # Régression linéaire pour les séries mensuelles
  regression_mois <- lm(Nombre_accidents ~ as.Date(Mois), data = accidents_par_mois)
  
  # Régression linéaire pour les séries hebdomadaires
  regression_semaine <- lm(Nombre_accidents ~ as.Date(Semaine), data = accidents_par_semaine)
  
  # Calcul des erreurs de prédiction
  erreur_mois <- sum(regression_mois$residuals^2)
  erreur_semaine <- sum(regression_semaine$residuals^2)
  
  # Détermination du niveau d'agrégation offrant la meilleure prédiction
  niveau_agregation <- ifelse(erreur_mois < erreur_semaine, "le meilleur niveau pour la prédiction est : mois", "le meilleur niveau pour la prédiction est :semaine")
  
  # Résultats
  resultats <- list(accidents_par_mois = accidents_par_mois,
                    accidents_par_semaine = accidents_par_semaine,
                    regression_mois = regression_mois,
                    regression_semaine = regression_semaine,
                    erreur_mois = erreur_mois,
                    erreur_semaine = erreur_semaine,
                    niveau_agregation = niveau_agregation)
  
  return(resultats)
}

re <- construire_series_chronologiques(E1)
print(re)
