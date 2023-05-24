# projetBDIAWEB
# Partie R
Ce fichier README fournit une documentation pour le code R fourni dans le script "stat_acc.R". Le script effectue diverses analyses et visualisations de données à partir d'un ensemble de données sur les accidents de la route.

# Partie BigData

Ce fichier README fournit une documentation pour le code R fourni dans le script "stat_acc.R". Le script effectue diverses analyses et visualisations de données à partir d'un ensemble de données sur les accidents de la route.

## Prérequis

Avant d'exécuter le script, assurez-vous d'installer les packages R suivants :

- tidyverse
- plotly
- dplyr
- lubridate
- vcd
- lmtest
- zoo

Vous pouvez les installer en utilisant la fonction `install.packages()` dans R.

## Données

Le script utilise un fichier CSV contenant les données des accidents de la route. Assurez-vous de spécifier le bon chemin d'accès au fichier CSV dans la ligne suivante du code :

```
database <- read.csv("chemin_vers_le_fichier.csv", header = TRUE, sep = ";", encoding = "UTF-8")
```

Assurez-vous également que le fichier CSV est correctement formaté avec les colonnes appropriées et le bon encodage.

## Fonctionnalités

Le script effectue les tâches suivantes :

1. Nettoyage des données : Suppression des lignes contenant des valeurs manquantes et des valeurs aberrantes dans les colonnes de longitude, latitude et nombre de places.
2. Conversion des variables catégorielles en variables numériques : Le script utilise la fonction `valeur_num_type()` pour convertir certaines colonnes catégorielles en variables numériques.
3. Visualisations graphiques : Le script utilise la fonction `creer_graphique_barres_v1()` et `creer_graphique_barres_v2()` pour créer des graphiques à barres interactifs en utilisant Plotly.
4. Construction de séries chronologiques : Le script utilise la fonction `construire_series_chronologiques()` pour agréger les données par mois et par semaine, effectuer des régressions linéaires et déterminer le niveau d'agrégation offrant la meilleure prédiction.
5. Analyse des données : Le script effectue des analyses croisées et des tests d'indépendance du chi2 sur différentes variables.

## Utilisation

Pour exécuter le script, veuillez suivre les étapes suivantes :

1. Assurez-vous d'avoir installé les packages requis mentionnés dans la section "Prérequis".
2. Spécifiez le chemin d'accès correct au fichier CSV contenant les données dans la ligne appropriée du code.
3. Exécutez le script dans un environnement R.

Assurez-vous d'avoir les autorisations nécessaires pour lire et écrire des fichiers dans les répertoires spécifiés.

## Résultats

Le script génère plusieurs visualisations graphiques interactives utilisant Plotly, telles que des graphiques à barres pour les accidents par semaine, par mois, par jour de la semaine, en fonction des conditions atmosphériques, de la gravité, de la surface, des tranches horaires, etc.

Les résultats des analyses croisées et des tests d'indépendance du chi2 sont également affichés dans la console.

## Auteur

Ce script R a été rédigé par  et est fourni dans le cadre d'un projet ou d'une analyse de données.
