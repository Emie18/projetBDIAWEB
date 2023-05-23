library(tidyverse)
library(plotly)
library(dplyr)
library(lubridate)
#####LIRE CSV####
database <- read.csv("C:/Users/33784/Desktop/stat_acc_V3.csv", header = TRUE, sep = ";")

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


#####FONCTION####
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

creer_graphique_barres_v1 <- function(E1, variable_x, variable_y, x_label, y_label, titre) {
  if (variable_x == "ville") {
    plot_ly(E1 %>% count(!!sym(variable_x)), x = ~get(variable_x), y = ~n, type = "bar") %>%
      layout(xaxis = list(title = x_label), yaxis = list(title = y_label), title = titre)
  } else {
    plot_ly(E1 %>% count(!!sym(variable_x)), x = ~get(variable_x), y = ~n, type = "bar", color = ~as.factor(get(variable_x))) %>%
      layout(xaxis = list(title = x_label), yaxis = list(title = y_label), title = titre)
  }
}

creer_graphique_barres_v2 <- function(E1, variable_x, variable_y, x_label, y_label, titre) {
  plot_ly(E1, x = ~get(variable_x), y = ~get(variable_y), type = "bar", color = ~get(variable_x)) %>%
    layout(xaxis = list(title = x_label), yaxis = list(title = y_label), title = titre)
}


construire_series_chronologiques <- function(data) {
  
  # Agréger par mois
  accidents_par_mois <- data %>%
    mutate(Mois = floor_date(date, "month")) %>%
    group_by(Mois) %>%
    summarise(Nombre_accidents = n())
  
  # Agréger par semaine
  accidents_par_semaine <- data %>%
    mutate(Semaine = floor_date(date, "week", week_start = getOption("lubridate.week.start", 7))) %>%
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

# Convertir la colonne de date/heure en format POSIXct
E1$date <- as.POSIXct(E1$date, format = "%Y-%m-%d %H:%M:%S")

# Extraire l'heure à partir de la colonne de date/heure et créer une nouvelle colonne "heure"
E1$heure <- format(E1$date, "%H:%M:%S")

plages_horaires <- cut(as.numeric(format(E1$date, "%H")), 
                       breaks = c(0, 6, 12, 18, 24), 
                       labels = c("0-6h", "6-12h", "12-18h", "18-24h"),
                       include.lowest = TRUE)

# Ajouter la colonne des plages horaires au dataframe
E1$plages_horaires <- plages_horaires

# Convertir la colonne de date en format DATE
variables_dates <- c("date")
E1[variables_dates] <- lapply(E1[variables_dates], as.Date)

re <- construire_series_chronologiques(E1)
accidents_par_semaine <- re$accidents_par_semaine
accidents_par_mois <- re$accidents_par_mois
# Créer les graphiques
graphique_semaine <- creer_graphique_barres_v2(accidents_par_semaine, "Semaine", "Nombre_accidents", "Semaine", "Nombre d'accidents", "Nombre d'accidents par semaine")
graphique_mois <- creer_graphique_barres_v2(accidents_par_mois, "Mois", "Nombre_accidents", "Mois", "Nombre d'accidents", "Nombre d'accidents par mois")

print(re)
#####AF####
# Calculer le nombre d'accidents par jour de la semaine
accidents_par_jour_semaine <- E1 %>%
  mutate(Jour_semaine = wday(date, label = TRUE)) %>%
  count(Jour_semaine) %>%
  arrange(match(Jour_semaine, c( "lun", "mar", "mer", "jeu", "ven", "sam","dim")))

# Créer le graphique à barres avec Plotly
plot <- plot_ly(accidents_par_jour_semaine, x = ~Jour_semaine, y = ~n, type = "bar", marker = list(color = "steelblue"))

# Personnaliser l'axe x et y
plot <- plot %>% layout(xaxis = list(title = "Jour de la semaine"), yaxis = list(title = "Nombre d'accidents"))

#graphique condition atmo
creer_graphique_barres_v1(E1, "descr_athmo", "n", "Conditions atmosphériques", "Nombre d'accidents", "Nombre d'accidents en fonction des conditions atmosphériques")

#graphique surface
creer_graphique_barres_v1(E1, "descr_etat_surf", "n", "Description de la surface", "Nombre d'accidents", "Nombre d'accidents en fonction de la description de la surface")

#graphque avec les villes
creer_graphique_barres_v1(E1, "ville", "n", "Ville", "Nombre d'accidents", "Nombre d'accidents par ville")

#graphique avec les tranches horaires
creer_graphique_barres_v1(E1, "plages_horaires", "n", "Tranches horaires", "Nombre d'accidents", "Nombre d'accidents par tranches horaires")

# graphique avec les jours
print(plot)
#graphique avvec les semaines
print(graphique_semaine)
#graphique avec les mois
print(graphique_mois)



######ANALYSE#######
# Chargement des packages nécessaires
library(vcd)
library(lmtest)
library(zoo)
# Tableaux croisés et tests d'indépendance du chi2
table_croisee_1 <- table(E1$descr_athmo, E1$descr_cat_veh)
table_croisee_2 <- table(E1$descr_athmo, E1$descr_etat_surf)

# Tableaux croisés et tests d'indépendance du chi2 avec simulation
test_chi2_1 <- chisq.test(table_croisee_1, simulate.p.value = TRUE)
test_chi2_2 <- chisq.test(table_croisee_2, simulate.p.value = TRUE)

# Affichage des résultats
print("Tableau croisé : descr_athmo vs. descr_cat_veh")
print(table_croisee_1)
print("Test d'indépendance du chi2 : descr_athmo vs. descr_cat_veh")
print(test_chi2_1)
print("Tableau croisé : descr_athmo vs. descr_etat_surf")
print(table_croisee_2)
print("Test d'indépendance du chi2 : descr_athmo vs. descr_etat_surf")
print(test_chi2_2)

# Représentation graphique avec mosaic plots
mosaicplot(table_croisee_1, shade = TRUE)
mosaicplot(table_croisee_2, shade = TRUE)

# Régressions linéaires du nombre d'accidents par mois et par semaine
regression_mois <- lm(Nombre_accidents ~ as.Date(Mois), data = accidents_par_mois)
regression_semaine <- lm(Nombre_accidents ~ as.Date(Semaine), data = accidents_par_semaine)

# Analyse des performances de la régression
performance_mois <- summary(regression_mois)
performance_semaine <- summary(regression_semaine)

# Affichage des performances
print("Régression linéaire - Mois")
print(performance_mois)
print("Régression linéaire - Semaine")
print(performance_semaine)

# Erreurs types associées aux estimateurs
erreur_type_mois <- sqrt(diag(vcov(regression_mois)))
erreur_type_semaine <- sqrt(diag(vcov(regression_semaine)))

# Intervalles de confiance à 95% pour les estimateurs
intervalle_confiance_mois <- confint(regression_mois, level = 0.95)
intervalle_confiance_semaine <- confint(regression_semaine, level = 0.95)

# Calcul des R2 et R2 ajusté pour les deux modèles
R2_mois <- performance_mois$r.squared
R2_ajuste_mois <- performance_mois$adj.r.squared
R2_semaine <- performance_semaine$r.squared
R2_ajuste_semaine <- performance_semaine$adj.r.squared

# Affichage des résultats
print("Régression linéaire - Mois")
print(paste("R2 :", R2_mois))
print(paste("R2 ajusté :", R2_ajuste_mois))
print("Régression linéaire - Semaine")
print(paste("R2 :", R2_semaine))
print(paste("R2 ajusté :", R2_ajuste_semaine))



