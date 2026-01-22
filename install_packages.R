# Script pour installer les packages requis pour l'application Shiny BFS

# Liste des packages requis
required_packages <- c(
  "shiny",
  "BFS",
  "dplyr",
  "DT",
  "shinycssloaders",
  "tidyr"
)

# Packages optionnels mais recommandés
optional_packages <- c(
  "shinyjs"
)

# Fonction pour installer un package s'il n'est pas déjà installé
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste0("Installation de ", pkg, "...\n"))
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org")
        cat(paste0("✓ ", pkg, " installé avec succès\n"))
      }, error = function(e) {
        cat(paste0("✗ Erreur lors de l'installation de ", pkg, ": ", e$message, "\n"))
      })
    } else {
      cat(paste0("✓ ", pkg, " est déjà installé\n"))
    }
  }
}

cat("=== Installation des packages requis ===\n")
install_if_missing(required_packages)

cat("\n=== Installation des packages optionnels ===\n")
install_if_missing(optional_packages)

cat("\n=== Vérification finale ===\n")
all_installed <- TRUE
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("✗ ", pkg, " n'est pas installé\n"))
    all_installed <- FALSE
  }
}

if (all_installed) {
  cat("\n✓ Tous les packages requis sont installés!\n")
  cat("Vous pouvez maintenant lancer l'application avec: shiny::runApp('app.R')\n")
} else {
  cat("\n✗ Certains packages n'ont pas pu être installés. Veuillez les installer manuellement.\n")
}
