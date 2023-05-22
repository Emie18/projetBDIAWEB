library(tidyverse)
library(plotly)
database <- read.csv("C:/Users/Emilie/documents/ISEN 2022/stat_acc_V3.csv", header = TRUE, sep = ";")
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
  
  write.csv(database, "C:/Users/Emilie/documents/ISEN 2022/stat_acc_V3_test1.csv", row.names = TRUE)
  
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

print(unique(E1$descr_cat_veh))


