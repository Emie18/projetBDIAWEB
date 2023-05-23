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
  Condition <- Accidents_no_NA$longitude < -5.2 | Accidents_no_NA$longitude > 9.66| Accidents_no_NA$latitude < -41.3 | Accidents_no_NA$latitude > 51.1242 | Accidents_no_NA$place == 'NULL'
  database <- subset(Accidents_no_NA, !Condition)
  #summary(database)
  #convertir en num
  suppressWarnings({
    
    variables_numeriques <- c("age", "place", "an_nais", "id_usa")
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

ajout_region <- function(E1, tot_habitants, regions) {
  # Sélectionner les colonnes nécessaires pour la fusion
  regions <- regions[, c("id_code_insee", "nom_region")]
  
  # Identifier les lignes avec des valeurs en double dans la colonne clé
  duplicates <- duplicated(regions$id_code_insee)
  
  # Sélectionner uniquement les lignes uniques
  regions <- subset(regions, !duplicates)
  
  # Vérifier les duplications dans la colonne clé "id_code_insee"
  if (any(duplicated(regions$id_code_insee))) {
    stop("La colonne 'id_code_insee' dans le fichier 'regions' contient des valeurs en double.")
  }
  
  # Fusionner les données en utilisant la colonne "id_code_insee" comme clé de fusion
  E2 <- merge(E1, regions, by = "id_code_insee", all.x = TRUE)
  
  return(E2)
  
  
}

ajout_departement <- function(E1,regions) {
  # Sélectionner les colonnes nécessaires pour la fusion
  departement <- regions[, c("id_code_insee", "nom_departement")]
  
  # Identifier les lignes avec des valeurs en double dans la colonne clé
  duplicates <- duplicated(departement$id_code_insee)
  
  # Sélectionner uniquement les lignes uniques
  departement <- subset(departement, !duplicates)
  
  # Vérifier les duplications dans la colonne clé "id_code_insee"
  if (any(duplicated(departement$id_code_insee))) {
    stop("La colonne 'id_code_insee' dans le fichier 'regions' contient des valeurs en double.")
  }
  
  # Fusionner les données en utilisant la colonne "id_code_insee" comme clé de fusion
  E2 <- merge(E1, departement, by = "id_code_insee", all.x = TRUE)
  
  return(E2)
}

#fonction pour affichier une carte avec les accidents en France en 2009
map_accident <- function(E1){
  
  # Définir les labels personnalisés pour la légende
  labels_legende <- c("Tué", "Blessé hospitalisé", "Blessé léger","Indemne")
  
  # Convertir la variable descr_grav en facteur avec les labels personnalisés
  E1$descr_grav <- factor(E1$descr_grav, levels = c("2", "3", "4","1"), labels = labels_legende)
  
  palette <- c("Tué" = "black", "Blessé hospitalisé" = "red", "Blessé léger" = "orange","Indemne" = "blue" )
  E1$couleur <- palette[as.character(E1$descr_grav)]
  

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


map_region <- function(E2,accidents_par_region) {

    # Créer une palette de couleurs en fonction du nombre d'accidents
  palette <- colorRampPalette(c("#AED9E0","#7BC4E2","#1F8FC2",
                                "#175C85",
                                "#A0DED6",
                                "#3BB4B0",
                                "#00A79D",
                                "#008C69",
                                "#FFF6A5",
                                "#FFD542",
                                "#FFA642",
                                "#FF7F00",
                                "#FF6B6B",
                                "#FF4040",
                                "#B83232"))(length(unique(accidents_par_region$Quantite_accidents)))
  
  # Associer une couleur à chaque région en fonction du nombre d'accidents
  region_colors <- setNames(palette, unique(accidents_par_region$Quantite_accidents))
  
  # Ajouter la couleur correspondante à chaque région dans le data frame accidents_par_region
  accidents_par_region$couleur <- region_colors[as.character(accidents_par_region$nom_region)]
  
  # Fusionner les données de accidents_par_region avec le data frame E2
  E2 <- left_join(E2, accidents_par_region, by = "nom_region")
  
  # Création de la carte
  Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoiZW1pZTE4IiwiYSI6ImNsaDdxdXB2dDAxZmYzZW1tM3hhbWR3b24ifQ.zjp20nsMooS-xVfxn982pA")
  
  fig <- plot_ly(E2, type = "scattermapbox", mode = "markers",
                 lat = ~latitude, lon = ~longitude,
                 color = ~Quantite_accidents, colors = palette,
                 text = ~paste("Région:", nom_region, "<br>Quantité d'accidents:", Quantite_accidents)) %>%
    layout(mapbox = list(
      accesstoken = Sys.getenv('MAPBOX_TOKEN'),
      center = list(lon = 2.454071, lat = 46.603354),
      zoom = 4.5,
      style = 'mapbox://styles/mapbox/light-v10'),
      title = list(text = "Accidents par région", x = 0.5))
      
  show(fig)
}
map_department <- function(E2,accidents_par_region) {
  
  # Créer une palette de couleurs en fonction du nombre d'accidents
  palette <- colorRampPalette(c("#AED9E0","#7BC4E2","#1F8FC2",
                                "#175C85",
                                "#A0DED6",
                                "#3BB4B0",
                                "#00A79D",
                                "#008C69",
                                "#FFF6A5",
                                "#FFD542",
                                "#FFA642",
                                "#FF7F00",
                                "#FF6B6B",
                                "#FF4040",
                                "#B83232"))(length(unique(accidents_par_departement$Quantite_accidents)))
  
  
  departement_colors <- setNames(palette, unique(accidents_par_departement$Quantite_accidents))

  accidents_par_departement$couleur <- departement_colors[as.character(accidents_par_departement$nom_departement)]
  
  E2 <- left_join(E2, accidents_par_departement, by = "nom_departement")
  
  # Création de la carte
  Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoiZW1pZTE4IiwiYSI6ImNsaDdxdXB2dDAxZmYzZW1tM3hhbWR3b24ifQ.zjp20nsMooS-xVfxn982pA")
  
  fig <- plot_ly(E2, type = "scattermapbox", mode = "markers",
                 lat = ~latitude, lon = ~longitude,
                 color = ~Quantite_accidents, colors = palette,size = 2,
                 text = ~paste("Région:", nom_departement, "<br>Quantité d'accidents:", Quantite_accidents)) %>%
    layout(mapbox = list(
      accesstoken = Sys.getenv('MAPBOX_TOKEN'),
      center = list(lon = 2.454071, lat = 46.603354),
      zoom = 4.5,
      style = 'mapbox://styles/mapbox/light-v10'),
      title = list(text = "Accidents par département", x = 0.5))
  
  show(fig)
}

map_region_grave <- function(E2,accidents_par_region) {
  
  # Créer une palette de couleurs en fonction du nombre d'accidents
  palette <- colorRampPalette(c("#AED9E0","#7BC4E2","#1F8FC2",
                                "#175C85",
                                "#A0DED6",
                                "#3BB4B0",
                                "#00A79D",
                                "#008C69",
                                "#FFF6A5",
                                "#FFD542",
                                "#FFA642",
                                "#FF7F00",
                                "#FF6B6B",
                                "#FF4040",
                                "#B83232"))(length(unique(accidents_par_region$Quantite_accidents_graves)))
  
  # Associer une couleur à chaque région en fonction du nombre d'accidents
  region_colors <- setNames(palette, unique(accidents_par_region$Quantite_accidents_graves))
  
  # Ajouter la couleur correspondante à chaque région dans le data frame accidents_par_region
  accidents_par_region$couleur <- region_colors[as.character(accidents_par_region$nom_region)]
  
  # Fusionner les données de accidents_par_region avec le data frame E2
  E2 <- left_join(E2, accidents_par_region, by = "nom_region")
  
  # Création de la carte
  Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoiZW1pZTE4IiwiYSI6ImNsaDdxdXB2dDAxZmYzZW1tM3hhbWR3b24ifQ.zjp20nsMooS-xVfxn982pA")
  
  fig <- plot_ly(E2, type = "scattermapbox", mode = "markers",
                 lat = ~latitude, lon = ~longitude,
                 color = ~Quantite_accidents_graves, colors = palette,
                 text = ~paste("Région:", nom_region, "<br>Quantité d'accidents:", Quantite_accidents_graves)) %>%
    layout(mapbox = list(
      accesstoken = Sys.getenv('MAPBOX_TOKEN'),
      center = list(lon = 2.554071, lat = 46.603354),
      zoom = 4,
      style = 'mapbox://styles/mapbox/light-v10'),
      title = list(text = "Accidents grave par région", x = 0.5))
  
  show(fig)
}

#fonction pour afficher la cartes des accidents graves
map_departement_grave <- function(E2,accidents_par_region) {
  
  # Créer une palette de couleurs en fonction du nombre d'accidents
  palette <- colorRampPalette(c("#AED9E0","#7BC4E2","#1F8FC2",
                                "#175C85",
                                "#A0DED6",
                                "#3BB4B0",
                                "#00A79D",
                                "#008C69",
                                "#FFF6A5",
                                "#FFD542",
                                "#FFA642",
                                "#FF7F00",
                                "#FF6B6B",
                                "#FF4040",
                                "#B83232"))(length(unique(accidents_par_region$Quantite_accidents_graves)))
  
  # Associer une couleur à chaque région en fonction du nombre d'accidents
  region_colors <- setNames(palette, unique(accidents_par_region$Quantite_accidents_graves))
  
  # Ajouter la couleur correspondante à chaque région dans le data frame accidents_par_region
  accidents_par_region$couleur <- region_colors[as.character(accidents_par_region$nom_departement)]
  
  # Fusionner les données de accidents_par_region avec le data frame E2
  E2 <- left_join(E2, accidents_par_region, by = "nom_departement")
  
  # Création de la carte
  Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoiZW1pZTE4IiwiYSI6ImNsaDdxdXB2dDAxZmYzZW1tM3hhbWR3b24ifQ.zjp20nsMooS-xVfxn982pA")
  
  fig <- plot_ly(E2, type = "scattermapbox", mode = "markers",
                 lat = ~latitude, lon = ~longitude,
                 color = ~Quantite_accidents_graves, colors = palette,
                 text = ~paste("département:", nom_departement, "<br>Quantité d'accidents:", Quantite_accidents_graves)) %>%
    layout(mapbox = list(
      accesstoken = Sys.getenv('MAPBOX_TOKEN'),
      center = list(lon = 2.554071, lat = 46.603354),
      zoom = 4,
      style = 'mapbox://styles/mapbox/light-v10'),
      title = list(text = "Accidents graves par département", x = 0.5))
  
  show(fig)
}

#----------------GAUTHIER----------------------------------------------------------------------------------------#

#fonction pour ajouter les région à la base de données
ajout_region2 <- function(E1,tot_habitants,regions){
  
  #Relier les fichiers CSV en utilisant la colonne commune "code_insee"
  reg <- regions[,c("id_code_insee", "code_region")]
  
  # Identifier les lignes avec des valeurs en double dans la colonne clé
  duplicates <- duplicated(regions$id_code_insee)
  
  # Sélectionner uniquement les lignes uniques
  regions <- subset(regions, !duplicates)
  
  # Vérifier les duplications dans la colonne clé "id_code_insee"
  if (any(duplicated(regions$id_code_insee))) {
    stop("La colonne 'id_code_insee' dans le fichier 'regions' contient des valeurs en double.")
  }
  
  E1 <- merge(E1, reg, by ="id_code_insee", all.x = TRUE)
  
  pop <- tot_habitants[, c("code_region", "PTOT", "REG")]
  E1 <- merge(E1, pop, by="code_region", all.x = TRUE)
  
  return(E1)
}

#fonction pour ajouter les région à la base de données*
JDD_accidents_regions <- function(E_100K) {
  grav <- E_100K[,c("REG", "PTOT", "descr_grav")]
  data_final <- grav %>%
    group_by(descr_grav, REG) %>%
    summarise(nombre_accidents = n(), PTOT = first(PTOT)) %>%
    mutate(accidents_par_100k = (nombre_accidents / PTOT) * 100000)
  return(data_final)
}


