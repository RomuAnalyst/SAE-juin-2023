# Définir une fonction pour générer des données aléatoires et effectuer des analyses
mon_programme <- function() {
  # Générer des données aléatoires
  set.seed(123)  # Pour la reproductibilité
  n <- 100  # Nombre d'observations
  donnees <- data.frame(
    x = rnorm(n, mean = 50, sd = 10),  # Variable aléatoire x
    y = rnorm(n, mean = 30, sd = 5)     # Variable aléatoire y
  )
  
  # Effectuer une analyse simple
  modele <- lm(y ~ x, data = donnees)  # Modèle de régression linéaire
  summary_modele <- summary(modele)     # Résumé du modèle
  
  # Afficher les résultats
  print("Résumé du modèle de régression linéaire :")
  print(summary_modele)
  
  # Visualiser les données et le modèle
  plot(donnees$x, donnees$y, main = "Données aléatoires", xlab = "x", ylab = "y")
  abline(modele, col = "red")  # Ajouter la ligne de régression
}

# Exécuter le programme
mon_programme()
