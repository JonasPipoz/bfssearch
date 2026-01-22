#' Recherche dans le catalogue opendata.swiss
#' 
#' @param keyword Mot-clé de recherche
#' @param language Langue pour les résultats (par défaut "fr")
#' @param limit Nombre maximum de résultats (par défaut 50)
#' 
#' @return Data frame avec les résultats de recherche (id, title, organization, description, etc.)
#' 
#' @noRd
search_opendata_swiss <- function(keyword, language = "fr", limit = 50) {
  # Vérifier que httr et jsonlite sont disponibles
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Les packages 'httr' et 'jsonlite' sont requis pour la recherche opendata.swiss")
  }
  
  # 1. Define the API endpoint
  url <- "https://opendata.swiss/api/3/action/package_search"
  
  # 2. Build query parameters
  query_params <- list(
    q = keyword,
    rows = limit
  )
  
  # 3. Request the data (add User-Agent to avoid 403 errors)
  response <- httr::GET(
    url, 
    query = query_params,
    httr::add_headers("User-Agent" = "R-httr")
  )
  
  # Check if request was successful
  status <- httr::status_code(response)
  if (status != 200) {
    error_msg <- paste0(
      "Failed to connect to opendata.swiss API. ",
      "Status code: ", status, ". ",
      "Please check your internet connection and try again."
    )
    stop(error_msg)
  }
  
  # 4. Parse the JSON content
  tryCatch({
    content <- httr::content(response, encoding = "UTF-8")
    
    # Check if request was successful
    if (!is.list(content) || !"success" %in% names(content) || !content$success) {
      error_msg <- if ("error" %in% names(content)) {
        paste0("API error: ", content$error)
      } else {
        "Unknown error from opendata.swiss API"
      }
      stop(error_msg)
    }
    
    # Extract results
    if (!"result" %in% names(content) || !"results" %in% names(content$result)) {
      return(data.frame(
        id = character(0),
        title = character(0),
        organization = character(0),
        description = character(0),
        num_resources = integer(0),
        stringsAsFactors = FALSE
      ))
    }
    
    results <- content$result$results
    
    if (length(results) == 0) {
      return(data.frame(
        id = character(0),
        title = character(0),
        organization = character(0),
        description = character(0),
        num_resources = integer(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # 5. Extract dataset information
    catalog <- lapply(results, function(ds) {
      # Extract title (multilingual, prefer specified language)
      # Priorité: title > display_name > name
      title_val <- ""
      
      # 1. Essayer d'abord avec "title" (plus fiable)
      if ("title" %in% names(ds)) {
        if (is.list(ds$title)) {
          # Chercher dans l'ordre: langue demandée > en > de > première disponible non vide
          if (language %in% names(ds$title) && nchar(trimws(ds$title[[language]] %||% "")) > 0) {
            title_val <- trimws(ds$title[[language]])
          } else if ("en" %in% names(ds$title) && nchar(trimws(ds$title$en %||% "")) > 0) {
            title_val <- trimws(ds$title$en)
          } else if ("de" %in% names(ds$title) && nchar(trimws(ds$title$de %||% "")) > 0) {
            title_val <- trimws(ds$title$de)
          } else if (length(ds$title) > 0) {
            # Prendre la première valeur non vide
            for (val in ds$title) {
              if (nchar(trimws(val %||% "")) > 0) {
                title_val <- trimws(val)
                break
              }
            }
          }
        } else {
          title_val <- trimws(as.character(ds$title))
        }
      }
      
      # 2. Si title est vide, essayer display_name
      if (title_val == "" && "display_name" %in% names(ds)) {
        if (is.list(ds$display_name)) {
          if (language %in% names(ds$display_name) && nchar(trimws(ds$display_name[[language]] %||% "")) > 0) {
            title_val <- trimws(ds$display_name[[language]])
          } else if ("en" %in% names(ds$display_name) && nchar(trimws(ds$display_name$en %||% "")) > 0) {
            title_val <- trimws(ds$display_name$en)
          } else if (length(ds$display_name) > 0) {
            for (val in ds$display_name) {
              if (nchar(trimws(val %||% "")) > 0) {
                title_val <- trimws(val)
                break
              }
            }
          }
        } else {
          title_val <- trimws(as.character(ds$display_name))
        }
      }
      
      # 3. Si toujours vide, utiliser "name" comme dernier recours
      if (title_val == "" && "name" %in% names(ds)) {
        title_val <- trimws(as.character(ds$name))
      }
      
      # Extract description (multilingual, prefer specified language)
      description_val <- ""
      if ("description" %in% names(ds) && is.list(ds$description)) {
        if (language %in% names(ds$description)) {
          description_val <- ds$description[[language]]
        } else if ("en" %in% names(ds$description)) {
          description_val <- ds$description$en
        } else if (length(ds$description) > 0) {
          description_val <- ds$description[[1]]
        }
      } else if ("description" %in% names(ds)) {
        description_val <- as.character(ds$description)
      }
      
      # Extract organization
      org_val <- ""
      if ("organization" %in% names(ds) && is.list(ds$organization)) {
        # L'organisation peut être un objet avec title/name, ou une liste multilingue
        if ("title" %in% names(ds$organization)) {
          # Si title est une liste multilingue
          if (is.list(ds$organization$title)) {
            org_val <- ds$organization$title[[language]] %||% 
                       ds$organization$title$en %||% 
                       ds$organization$title[[1]] %||% ""
          } else {
            org_val <- as.character(ds$organization$title)
          }
        } else if ("name" %in% names(ds$organization)) {
          org_val <- as.character(ds$organization$name)
        } else if (length(ds$organization) > 0) {
          # Si c'est directement une liste avec des valeurs
          org_val <- ds$organization[[1]] %||% ""
        }
      }
      
      # Extract number of resources
      num_resources <- 0
      if ("num_resources" %in% names(ds)) {
        num_resources <- as.integer(ds$num_resources)
      } else if ("resources" %in% names(ds) && is.list(ds$resources)) {
        num_resources <- length(ds$resources)
      }
      
      # Extract author
      author_val <- ""
      if ("author" %in% names(ds)) {
        author_val <- trimws(as.character(ds$author %||% ""))
      }
      
      # Extract publisher
      publisher_val <- ""
      if ("publisher" %in% names(ds)) {
        if (is.list(ds$publisher)) {
          # Publisher peut être un objet avec name/url ou une liste multilingue
          if ("name" %in% names(ds$publisher)) {
            if (is.list(ds$publisher$name)) {
              # Si name est multilingue
              if (language %in% names(ds$publisher$name) && nchar(trimws(ds$publisher$name[[language]] %||% "")) > 0) {
                publisher_val <- trimws(ds$publisher$name[[language]])
              } else if ("en" %in% names(ds$publisher$name) && nchar(trimws(ds$publisher$name$en %||% "")) > 0) {
                publisher_val <- trimws(ds$publisher$name$en)
              } else if (length(ds$publisher$name) > 0) {
                for (val in ds$publisher$name) {
                  if (nchar(trimws(val %||% "")) > 0) {
                    publisher_val <- trimws(val)
                    break
                  }
                }
              }
            } else {
              publisher_val <- trimws(as.character(ds$publisher$name))
            }
          } else if (length(ds$publisher) > 0) {
            # Si publisher est directement une liste multilingue
            if (language %in% names(ds$publisher) && nchar(trimws(ds$publisher[[language]] %||% "")) > 0) {
              publisher_val <- trimws(ds$publisher[[language]])
            } else if ("en" %in% names(ds$publisher) && nchar(trimws(ds$publisher$en %||% "")) > 0) {
              publisher_val <- trimws(ds$publisher$en)
            } else if (length(ds$publisher) > 0) {
              for (val in ds$publisher) {
                if (nchar(trimws(val %||% "")) > 0) {
                  publisher_val <- trimws(val)
                  break
                }
              }
            }
          }
        } else {
          publisher_val <- trimws(as.character(ds$publisher))
        }
      }
      
      data.frame(
        id = ds$id %||% "",
        title = title_val,
        organization = org_val,
        author = author_val,
        publisher = publisher_val,
        description = if (nchar(description_val) > 200) {
          paste0(substr(description_val, 1, 200), "...")
        } else {
          description_val
        },
        num_resources = num_resources,
        stringsAsFactors = FALSE
      )
    })
    
    catalog_df <- do.call(rbind, catalog)
    
    if (nrow(catalog_df) == 0) {
      return(data.frame(
        id = character(0),
        title = character(0),
        organization = character(0),
        description = character(0),
        num_resources = integer(0),
        stringsAsFactors = FALSE
      ))
    }
    
    return(catalog_df)
    
  }, error = function(e) {
    error_msg <- paste0(
      "Erreur lors du parsing de la réponse: ", e$message, ". ",
      "Veuillez réessayer plus tard."
    )
    stop(error_msg)
  })
}

# Helper function pour %||%
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Obtenir les détails d'un dataset opendata.swiss
#' 
#' @param package_id Identifiant du dataset
#' 
#' @return Liste avec les détails du dataset et ses ressources
#' 
#' @noRd
get_opendata_swiss_package <- function(package_id) {
  # Vérifier que httr et jsonlite sont disponibles
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Les packages 'httr' et 'jsonlite' sont requis")
  }
  
  # 1. Define the API endpoint
  url <- "https://opendata.swiss/api/3/action/package_show"
  
  # 2. Request the data (add User-Agent to avoid 403 errors)
  response <- httr::GET(
    url, 
    query = list(id = package_id),
    httr::add_headers("User-Agent" = "R-httr")
  )
  
  # Check if request was successful
  status <- httr::status_code(response)
  if (status != 200) {
    error_msg <- paste0(
      "Failed to connect to opendata.swiss API. ",
      "Status code: ", status
    )
    stop(error_msg)
  }
  
  # 3. Parse the JSON content
  content <- httr::content(response, encoding = "UTF-8")
  
  if (!is.list(content) || !"success" %in% names(content) || !content$success) {
    error_msg <- if ("error" %in% names(content)) {
      paste0("API error: ", content$error)
    } else {
      "Unknown error from opendata.swiss API"
    }
    stop(error_msg)
  }
  
  return(content$result)
}
