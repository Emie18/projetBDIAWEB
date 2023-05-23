library(tidyverse)
library(plotly)
library(dplyr)
library(lubridate)
source("C:/Users/Emilie/Documents/ISEN 2022/projet_bigdata/bigdata/fonctions_Emi.r")

#ouverture des fichier csv
database <- read.csv("C:/Users/Emilie/documents/ISEN 2022/stat_acc_V3.csv", header = TRUE, sep = ";")
tot_habitants <- read.csv("C:/Users/Emilie/Documents/ISEN 2022/Regions.csv", header = TRUE, sep = ";")
regions <- read.csv("C:/Users/Emilie/Documents/ISEN 2022/communes-departement-region.csv", header = TRUE, sep = ",")
#-------CHANGER LE CHEMIN D'ACCES-----------------#
fichier_type <- "C:/Users/Emilie/documents/ISEN 2022/type.txt"

#Nettoyage des données
E1 <- Nettoyage_des_donnees(database)

#suppression du fichier s'il existe
if (file.exists(fichier_type)) {
  file.remove(fichier_type)
}

#convertion des types en chiffre
E1 <- valeur_num_type(E1, "descr_cat_veh",fichier_type)
E1 <- valeur_num_type(E1, "descr_agglo",fichier_type)
E1 <- valeur_num_type(E1, "descr_athmo",fichier_type)
E1 <- valeur_num_type(E1, "descr_lum",fichier_type)
E1 <- valeur_num_type(E1, "descr_etat_surf",fichier_type)
E1 <- valeur_num_type(E1, "description_intersection",fichier_type)
E1 <- valeur_num_type(E1, "descr_dispo_secu",fichier_type)
E1 <- valeur_num_type(E1, "descr_grav",fichier_type)
E1 <- valeur_num_type(E1, "descr_motif_traj",fichier_type)
E1 <- valeur_num_type(E1, "descr_type_col",fichier_type)

#appel de la fonction pour contruire la chronologie
re <- construire_series_chronologiques(E1)
print(re)

#ajouter les regions à E1
E1<-ajout_region(E1,tot_habitants,regions)

#afficher la map des accidents en France
map_accident(E1)





