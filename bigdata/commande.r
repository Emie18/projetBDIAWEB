library(tidyverse)
library(plotly)
library(dplyr)
library(lubridate)
source("C:/Users/Emilie/Documents/ISEN 2022/projet_bigdata/bigdata/fonctions_Emi.r")
source("C:/Users/Emilie/Documents/ISEN 2022/projet_bigdata/bigdata/fonction_regression.r")


#ouverture des fichier csv
database <- read.csv("C:/Users/Emilie/documents/ISEN 2022/stat_acc_V3.csv", header = TRUE, sep = ";")
tot_habitants <- read.csv("C:/Users/Emilie/Documents/ISEN 2022/Regions.csv", header = TRUE, sep = ";")
regions <- read.csv("C:/Users/Emilie/Documents/ISEN 2022/communes-departement-region.csv", header = TRUE, sep = ";")
tot_habitants_departement <-read.csv("C:/Users/Emilie/Documents/ISEN 2022/ptot_departement.csv", header = TRUE, sep = ",")
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


E2_dep<- ajout_departement(E1,regions)
#ajouter les regions à E1
E3_reg<-ajout_region(E1,tot_habitants,regions)



# Calculer le nombre total d'accidents par région
accidents_par_region <- E3_reg %>%
  group_by(nom_region) %>%
  summarise(Quantite_accidents = n())

# Fusionner avec les données de population par région
accidents_par_region <- merge(accidents_par_region, tot_habitants, by.x = "nom_region", by.y = "REG")

# Calculer le taux d'accidents pour 100 000 habitants
accidents_par_region$Taux_accidents <- (accidents_par_region$Quantite_accidents / accidents_par_region$PTOT) * 100000

# Afficher les résultats
accidents_par_region


# Calculer le nombre total d'accidents par région
accidents_par_departement <- E2_dep %>%
  group_by(nom_departement) %>%
  summarise(Quantite_accidents = n())

# Fusionner avec les données de population par région
accidents_par_departement <- merge(accidents_par_departement, tot_habitants_departement, by = "nom_departement")

# Calculer le taux d'accidents pour 100 000 habitants
accidents_par_departement$Taux_accidents <- (accidents_par_departement$Quantite_accidents / accidents_par_region$PTOT) * 100000

# Afficher les résultats
accidents_par_departement

# Calculer le nombre total d'accidents graves par région
accidents_graves_par_region <- E3_reg %>%
  filter(descr_grav == 2) %>%
  group_by(nom_region) %>%
  summarise(Quantite_accidents_graves = n())

# Calculer le nombre total d'accidents graves par département
accidents_graves_par_departement <- E2_dep %>%
  filter(descr_grav == 2) %>%
  group_by(nom_departement) %>%
  summarise(Quantite_accidents_graves = n())

#afficher la map des accidents grave en France par région
map_region_grave(E3_reg,accidents_graves_par_region)

#afficher la map des accidents grave en France par département
map_departement_grave(E2_dep,accidents_graves_par_departement)

#afficher la map  du nombre des accidents en France par département
map_department(E2_dep,accidents_par_departement)

#afficher la map du nombre des accidents en France par région
map_region(E3_reg,accidents_par_region)


#ajouter les regions à E1
E_100k<-ajout_region2(E1,tot_habitants,regions)

#Jeu de données pour ACP
E_100k <- JDD_accidents_regions(E_100k)

comparer_regessions(re)




