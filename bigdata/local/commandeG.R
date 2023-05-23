##1 Suppression des lignes non-valides

#Importation et visualisation du fichier csv
Accidents <- read.csv("/ISEN/CIR-3/BigData/Projet/projetBDIAWEB/bigdata/stat_acc_V3.csv", sep=";")
View(Accidents)
summary(Accidents)

#Suppression des lignes ne contenant aucunes valeurs
Accidents_no_NA <- na.omit(Accidents)
View(Accidents_no_NA)
summary(Accidents_no_NA)

#On remarque la présence de valeurs maximales absurdes concernant la longitude et la latitude
#Exemple 1: Ligne 3683 -> longitude > 90
#Exemple 2: Ligne 3684 -> latitude > 90

#On remarque aussi que certains accidents possèdent un nombre de place NULL :
#Exemple 3: Ligne 52127 -> place = NULL

#Supression des lignes contenant des valeurs absurdes en suivant la condition suivante :
Condition <- Accidents_no_NA$longitude < -90 | Accidents_no_NA$longitude > 90 | Accidents_no_NA$latitude < -90 | Accidents_no_NA$latitude > 90 | Accidents_no_NA$place == 'NULL'
Accidents_filtre <- subset(Accidents_no_NA, !Condition)
summary(Accidents_filtre)
View(Accidents_filtre)


##5 Construction du jeu de données

#Relier les fichiers CSV en utilisant la colonne commune "code_insee"

reg <- regions[,c("code_commune_INSEE", "code_region")]
merge_data <- merge(database, reg, by ='code_commune_INSEE', all.x = TRUE)

pop <- tot_habitants[, c("code_region", "PTOT", "REG")]
database <- merge(merge_data, pop, by='code_region', all.x = TRUE)

# Calcul du nombre total d'accidents par gravité et région
nombre_accidents <- aggregate(database$descr_grav, by = list(region = database$REG), FUN = length)

# Renommer la colonne "x" pour refléter le nombre d'accidents
colnames(nombre_accidents) <- c("REG", "nombre_accidents")