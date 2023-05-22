library(ggplot2)


convertir_formats <- function(chemin_fichier) {
  # Lire le fichier CSV
  donnees <- read.csv(chemin_fichier, header = TRUE, sep=";")
  
  # Convertir les variables numériques en format numérique
  variables_numeriques <- c("age", "place", "an_nais", "id_code_insee", "id_usa")
  donnees[variables_numeriques] <- lapply(donnees[variables_numeriques], as.numeric)
  
  # Convertir les variables de date en format date
  variables_dates <- c("date")
  donnees[variables_dates] <- lapply(donnees[variables_dates], as.Date)
  
  # Retourner les données avec les formats mis à jour
  return(donnees)
}

# Utilisation de la fonction
chemin_fichier <- "C:/Users/33784/Desktop/stat_acc_V3.csv"
donnees_converties <- convertir_formats(chemin_fichier)


ggplot(donnees_converties, aes(x =descr_athmo, y = Num_Acc)) +
  geom_bar(stat = "identity") +
  labs(x = "descr_athmo", y = "Num_Acc") +
  theme_bw()


ggplot(donnees_converties, aes(x =descr_etat_surf, y = Num_Acc)) +
  geom_bar(stat = "identity") +
  labs(x = "descr_etat_surf", y = "Num_Acc") +
  theme_bw()


