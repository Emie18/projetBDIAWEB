#-----------------------#
#---fichier Fonctions---#
#-----------------------#

#fonction pour comparer la regression de 
comparer_regessions <- function(resultats) {
  # Extraire les résultats des régressions
  regression_mois <- resultats$regression_mois
  regression_semaine <- resultats$regression_semaine
  
  # Analyser les performances de la régression
  performance_mois <- summary(regression_mois)$r.squared
  performance_semaine <- summary(regression_semaine)$r.squared
  
  # Analyser les erreurs types associées aux estimateurs
  erreur_type_mois <- summary(regression_mois)$coefficients[, "Std. Error"]
  erreur_type_semaine <- summary(regression_semaine)$coefficients[, "Std. Error"]
  
  # Calculer les intervalles de confiance à 95% pour ces estimateurs
  intervalle_confiance_mois <- confint(regression_mois)
  intervalle_confiance_semaine <- confint(regression_semaine)
  
  # Calculer les R2 et R2 ajusté pour les deux modèles
  r2_mois <- summary(regression_mois)$r.squared
  r2_ajuste_mois <- summary(regression_mois)$adj.r.squared
  r2_semaine <- summary(regression_semaine)$r.squared
  r2_ajuste_semaine <- summary(regression_semaine)$adj.r.squared
  
  # Afficher les résultats
  cat("Performances de la régression (R2) :\n")
  cat("Régression mois :", r2_mois, "\n")
  cat("Régression semaine :", r2_semaine, "\n\n")
  
  cat("Performances de la régression ajustée (R2 ajusté) :\n")
  cat("Régression mois :", r2_ajuste_mois, "\n")
  cat("Régression semaine :", r2_ajuste_semaine, "\n\n")
  
  cat("Erreurs types associées aux estimateurs :\n")
  cat("Régression mois :", erreur_type_mois, "\n")
  cat("Régression semaine :", erreur_type_semaine, "\n\n")
  
  cat("Intervalles de confiance à 95% pour les estimateurs :\n")
  cat("Régression mois :\n")
  print(intervalle_confiance_mois)
  cat("\nRégression semaine :\n")
  print(intervalle_confiance_semaine)
  
}