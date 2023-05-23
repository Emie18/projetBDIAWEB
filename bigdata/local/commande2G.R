#-----------------------#
#---fichier Fonctions---#
#-----------------------#

#fonction pour nettoyer les données, enlever les lignes, avec des valeurs manquante ou absurde
Nettoyage_des_donnees <- function(database){
  
  #Suppression des lignes ne contenant aucunes valeurs
  Accidents_no_NA <- na.omit(database)
  #summary(Accidents_no_NA)
  
  #On remarque la présence de valeurs maximales absurdes concernant la longitude et la latitude
  #Exemple 1: Ligne 3683 -> longitude > 90
  #Exemple 2: Ligne 3684 -> latitude > 90
  
  #On remarque aussi que certains accidents possèdent un nombre de place NULL :
  #Exemple 3: Ligne 52127 -> place = NULL
  
  #Supression des lignes contenant des valeurs absurdes en suivant la condition suivante :
  Condition <- Accidents_no_NA$longitude < -5.2667 | Accidents_no_NA$longitude > 9.6625 | Accidents_no_NA$latitude < 41.3333 | Accidents_no_NA$latitude > 51.1242 | Accidents_no_NA$place == 'NULL'
  database <- subset(Accidents_no_NA, !Condition)
  #summary(database)
  #convertir en num
  suppressWarnings({
    
    variables_numeriques <- c("age", "place", "an_nais", "code_INSEE", "id_usa")
    database[variables_numeriques] <- lapply(database[variables_numeriques], as.numeric)
  })
  
  # Convertir les variables de date en format date
  variables_dates <- c("date")
  database[variables_dates] <- lapply(database[variables_dates], as.Date)
  
  #modifier l'age:
  database$age <- database$age - 14
  return(database)
  
}

#fonction pour convertir les types en chiffre
valeur_num_type <- function(database, col_name,fichier_type) {
  
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
  
  write(correspondance_text, file = fichier_type, append = TRUE, sep = "\n")
  
  for (i in 1:length(database[[col_name]])) {
    database[[col_name]][i] <- chiffres_associes[match(database[[col_name]][i], types_uniques)]
  }
  variables_numeriques <- c(col_name)
  database[variables_numeriques] <- lapply(database[variables_numeriques], as.numeric)
  
  return(database)
}

#fonction pour construire une base de donnée
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

#fonction pour ajouter les région à la base de données
ajout_region <- function(E1,tot_habitants,regions){
  
  #Relier les fichiers CSV en utilisant la colonne commune "code_insee"
  
  reg <- regions[,c("code_INSEE", "code_region")]
  
  # Identifier les lignes avec des valeurs en double dans la colonne clé
  duplicates <- duplicated(regions$code_INSEE)
  
  # Sélectionner uniquement les lignes uniques
  regions <- subset(regions, !duplicates)
  
  # Vérifier les duplications dans la colonne clé "id_code_insee"
  if (any(duplicated(regions$code_INSEE))) {
    stop("La colonne 'code_INSEE' dans le fichier 'regions' contient des valeurs en double.")
  }
  
  E1 <- merge(E1, reg, by ="code_INSEE", all.x = TRUE)
  
  pop <- tot_habitants[, c("code_region", "PTOT", "REG")]
  E1 <- merge(E1, pop, by="code_region", all.x = TRUE)
  
  return(E1)
}

#fonction pour ajouter les région à la base de données*
JDD_accidents_regions <- function(E1, tot_habitants, regions) {
  grav <- E1[,c("REG", "code_region", "PTOT", "descr_grav")]
  data_final <- grav %>%
      group_by(REG, code_region) %>%
      summarise(nombre_accidents = n(), PTOT = first(PTOT)) %>%
      mutate(accidents_par_100k = (nombre_accidents / PTOT) * 100000)
    return(data_final)
  }


#fonction pour affichier une carte avec les accidents en France en 2009
map_accident <- function(E1){
  
  # Définir les labels personnalisés pour la légende
  labels_legende <- c("Tué", "Blessé hospitalisé", "Blessé léger","Indemne")
  
  # Convertir la variable descr_grav en facteur avec les labels personnalisés
  E1$descr_grav <- factor(E1$descr_grav, levels = c("2", "3", "4","1"), labels = labels_legende)
  
  palette <- c("Tué" = "black", "Blessé hospitalisé" = "red", "Blessé léger" = "orange","Indemne" = "blue" )
  E1$couleur <- palette[as.character(E1$descr_grav)]
  
  accidents_par_region <- E1 %>%
    group_by(descr_agglo) %>%
    summarise(Quantite_accidents = n())
  
  # Création de la carte
  Sys.setenv("MAPBOX_TOKEN"="pk.eyJ1IjoiZW1pZTE4IiwiYSI6ImNsaDdxdXB2dDAxZmYzZW1tM3hhbWR3b24ifQ.zjp20nsMooS-xVfxn982pA")
  
  fig <- plot_ly(E1, type = "scattermapbox", mode = "markers",
                 lat = ~latitude, lon = ~longitude,
                 color = ~descr_grav, colors = palette) %>%
    layout(mapbox = list(accesstoken = Sys.getenv('MAPBOX_TOKEN'),
                         center = list(lon = 4, lat = 46),
                         zoom = 4.5,
                         style = 'mapbox://styles/mapbox/light-v10'),
           title = list(text = "Accidents en France en 2009", x = 0.5))
  
  show(fig)
  
}