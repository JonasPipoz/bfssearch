# Utilitaires Golem (optionnel)
# Ce fichier peut être utilisé si vous souhaitez utiliser golem plus tard

# Pour l'instant, on n'utilise pas golem mais on garde la structure compatible

# Fonction pour obtenir le nom du package depuis DESCRIPTION
get_golem_name <- function() {
  desc <- read.dcf("DESCRIPTION")
  desc[1, "Package"]
}

# Fonction pour obtenir la version depuis DESCRIPTION
get_golem_version <- function() {
  desc <- read.dcf("DESCRIPTION")
  desc[1, "Version"]
}
