#' @title Configuration de l'application
#' @description Configuration globale de l'application BFS Search
#' @noRd

# Configuration de l'application
app_config <- list(
  # Langue par défaut
  default_language = "fr",
  
  # Délai entre requêtes (en secondes)
  request_delay = 2,
  
  # Limite par défaut de résultats
  default_limit = 50,
  
  # URL du Swiss Stats Explorer
  sse_url = "https://stats.swiss/?lc=fr"
)

# Nom du package (pour compatibilité Golem)
golem_name <- "bfssearch"
golem_version <- "0.0.0.9000"
