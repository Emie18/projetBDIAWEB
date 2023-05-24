library(tidyverse)
library(plotly)
library(dplyr)
library(lubridate)
source("C:/Users/Emilie/Documents/ISEN 2022/projet_bigdata/bigdata/fonctions_Emi.r")
source("C:/Users/Emilie/Documents/ISEN 2022/projet_bigdata/bigdata/fonction_regression.r")


#ouverture des fichier csv
database <- read.csv("C:/Users/Emilie/documents/ISEN 2022/stat_acc_V3.csv", header = TRUE, sep = ";",encoding = "UTF-8")
tot_habitants <- read.csv("C:/Users/Emilie/Documents/ISEN 2022/Regions.csv", header = TRUE, sep = ";",encoding = "UTF-8")
regions <- read.csv("C:/Users/Emilie/Documents/ISEN 2022/communes-departement-region.csv", header = TRUE, sep = ";",encoding = "UTF-8")
tot_habitants_departement <-read.csv("C:/Users/Emilie/Documents/ISEN 2022/ptot_departement.csv", header = TRUE, sep = ",",encoding = "UTF-8")
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
regression_semaine<- re$regression_semaine
regression_mois <- re$regression_mois
accidents_par_mois_cumulee <- re$accidents_par_mois_cumulee
accidents_par_semaine_cumulee <- re$accidents_par_semaine_cumulee
niveau_agregation<- re$niveau_agregation
plot(accidents_par_mois_cumulee,main="Régression linaire d'accidents par mois")
abline(coef(regression_mois))

plot(accidents_par_semaine_cumulee,main="Régression linaire d'accidents par semaine")
abline(coef(regression_semaine))

R2_m <-cor(accidents_par_mois_cumulee,c(1:12))^2
R2_m
x_m <-cbind(rep(1,12),accidents_par_mois_cumulee)
R2_m_ajust <- R2_m-(1-R2_m)/(length(x_m)-1-1)
R2_m_ajust

R2_s <-cor(accidents_par_mois_cumulee,c(1:53))^2
R2_s
x_s <-cbind(rep(1,53),accidents_par_semaine_cumulee)
R2_s_ajust <- R2_s-(1-R2_s)/(length(x_s)-1-1)
R2_s_ajust


E2_dep<- ajout_departement(E1,regions)
#ajouter les regions à E1
E3_reg<-ajout_region(E1,tot_habitants,regions)

resultats <- calculer_accidents(E3_reg, E2_dep, tot_habitants, tot_habitants_departement)

# Récupération des résultats dans des variables individuelles
accidents_par_region <- resultats$accidents_par_region
accidents_par_departement <- resultats$accidents_par_departement
accidents_graves_par_region <- resultats$accidents_graves_par_region
accidents_graves_par_departement <- resultats$accidents_graves_par_departement

accidents_par_departement <- rename(accidents_par_departement,REG=nom_departement)
accidents_graves_par_departement <- rename(accidents_graves_par_departement,REG=nom_departement)

#afficher les cartes des accidents par régions et par départements
carte_r(E3_reg,accidents_par_region,"code_region","Taux d'accidents par région pour 100k/habitants en 2009")
carte_r(E3_reg,accidents_graves_par_region,"code_region","Taux d'accidents grave par région pour 100k/habitants en 2009")
carte_d(E2_dep,accidents_par_departement,"code_departement","Taux d'accidents par département pour 100k/habitants en 2009")
carte_d(E2_dep,accidents_graves_par_departement,"code_departement","Taux d'accidents grave par département pour 100k/habitants en 2009")


#ajouter les regions à E1
E_100k<-ajout_region2(E1,tot_habitants,regions)

#Jeu de données pour ACP
E_100k <- JDD_accidents_regions(E_100k)

comparer_regessions(re)






