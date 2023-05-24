library(tidyverse)
library(plotly)
library(dplyr)
library(lubridate)
source("C:/ISEN/CIR-3/BigData/projetBDIAWEB/bigdata/local/commande2G.R")

#ouverture des fichier csv
database <- read.csv("C:/ISEN/CIR-3/BigData/projetBDIAWEB/bigdata/stat_acc_V3.csv", header = TRUE, sep = ";")
tot_habitants <- read.csv("C:/ISEN/CIR-3/BigData/projetBDIAWEB/bigdata/Regions.csv", header = TRUE, sep = ";")
regions <- read.csv("C:/ISEN/CIR-3/BigData/projetBDIAWEB/bigdata/communes-departement-region.csv", header = TRUE, sep = ",")
#-------CHANGER LE CHEMIN D'ACCES-----------------#
fichier_type <- "C:/ISEN/CIR-3/BigData/projetBDIAWEB/bigdata/type.txt"

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

#Jeu de données
E2 <- JDD_accidents_regions(E1,tot_habitants,regions)

#Histogramme sur le nombre d'accidents par tranches d'âges
hist_accident(E1)

#Histogramme sur le nombre d'accidents par mois
hist_mensuel(E1)

