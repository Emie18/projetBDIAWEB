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



valeur_num_type <- function(database, col_name) {
  table_types <- table(database[[col_name]])
  print(table_types)
  
  types_uniques <- unique(database[[col_name]])
  chiffres_associes <- numeric(length(types_uniques))
  
  for (i in 1:length(types_uniques)) {
    chiffres_associes[i] <- i
  }
  correspondance <- data.frame(Type = types_uniques, Chiffre = chiffres_associes)
  
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






