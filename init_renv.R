# Script d'initialisation de renv
# À exécuter une seule fois lors de la première utilisation du projet

# Installer renv si nécessaire
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialiser renv
renv::init(bare = TRUE)

# Installer shiny
install.packages("shiny")

# Créer un snapshot
renv::snapshot()

cat("renv a été initialisé avec succès!\n")
cat("Vous pouvez maintenant utiliser votre projet Shiny.\n")
