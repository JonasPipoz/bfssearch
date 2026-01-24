#' Recherche dans le catalogue Swiss Stats Explorer
#' 
#' @param keyword Mot-clé de recherche
#' @param language Langue pour les résultats (par défaut "fr")
#' 
#' @return Data frame avec les résultats de recherche (id, agencyID, version, name)
#' 
#' @noRd
search_swiss_stats <- function(keyword, language = "fr") {
  # Vérifier que httr et jsonlite sont disponibles
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Les packages 'httr' et 'jsonlite' sont requis pour la recherche SSE")
  }
  
  # 1. Define the Dataflow endpoint for the catalog (using v1, v2 is not supported)
  url <- "https://disseminate.stats.swiss/rest/v1/dataflow/all/all/latest"
  
  # 2. Request the data with Accept-Language header for multilingual support
  # SDMX REST API supports Accept-Language header to specify preferred language
  # Request multiple languages (fr, de, it, en) to get all name variants for search
  # Format: "fr,de,it,en" or "fr;q=1.0,de;q=0.9,it;q=0.8,en;q=0.7"
  response <- httr::GET(
    url,
    httr::add_headers(
      "Accept-Language" = "fr,de,it,en",  # Request multiple languages for comprehensive search
      "Accept" = "application/json"  # Request JSON format
    )
  )
  
  # Check if request was successful
  status <- httr::status_code(response)
  if (status != 200) {
    error_msg <- paste0(
      "Failed to connect to the Swiss Stat API. ",
      "Status code: ", status, ". ",
      "Please check your internet connection and try again."
    )
    stop(error_msg)
  }
  
  # 3. Parse the JSON content
  tryCatch({
    # httr::content() automatically parses JSON when possible
    content <- httr::content(response, encoding = "UTF-8")
    
    # The API returns a structure with "resources" (empty) and "references" (contains dataflows)
    if (!is.list(content) || !"references" %in% names(content)) {
      return(data.frame(
        id = character(0),
        agencyID = character(0),
        version = character(0),
        name = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    references <- content$references
    
    if (length(references) == 0) {
      return(data.frame(
        id = character(0),
        agencyID = character(0),
        version = character(0),
        name = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # 4. Extract dataflows from references
    # Each reference is a named list where the name is a URN and the value contains id, name, etc.
    catalog <- lapply(references, function(ref) {
      # Extract id
      id <- ref$id %||% ""
      
      # Extract all names for search (multilingual)
      # SDMX JSON structure can have names in different formats:
      # 1. Object with language keys: { "fr": "...", "de": "...", "it": "...", "en": "..." }
      # 2. List of objects: [{ "lang": "fr", "value": "..." }, ...]
      # 3. Simple string (single language)
      all_names <- character(0)
      name <- ""
      
      if (!is.null(ref$name)) {
        if (is.list(ref$name) && !is.data.frame(ref$name)) {
          # Name is multilingual - collect all names for search
          # Check if it's a list of objects with lang/value structure
          if (length(ref$name) > 0 && is.list(ref$name[[1]]) && "lang" %in% names(ref$name[[1]])) {
            # Format: [{ "lang": "fr", "value": "..." }, ...]
            for (name_obj in ref$name) {
              if (is.list(name_obj) && "value" %in% names(name_obj)) {
                lang_val <- trimws(as.character(name_obj$value %||% ""))
                if (nchar(lang_val) > 0) {
                  all_names <- c(all_names, lang_val)
                  # Extract for display if language matches
                  if (name_obj$lang == language && name == "") {
                    name <- lang_val
                  }
                }
              }
            }
          } else {
            # Format: { "fr": "...", "de": "...", "it": "...", "en": "..." }
            all_names <- unlist(lapply(ref$name, function(x) {
              val <- trimws(as.character(x %||% ""))
              if (nchar(val) > 0) val else NULL
            }))
            all_names <- all_names[!sapply(all_names, is.null)]
            names(all_names) <- NULL  # Remove names attribute
          }
          
          # Extract name in requested language with fallback for display
          if (name == "") {
            # Try requested language first
            if (is.list(ref$name) && length(ref$name) > 0 && is.list(ref$name[[1]]) && "lang" %in% names(ref$name[[1]])) {
              # Format: [{ "lang": "fr", "value": "..." }, ...]
              for (name_obj in ref$name) {
                if (is.list(name_obj) && name_obj$lang == language && "value" %in% names(name_obj)) {
                  name <- trimws(as.character(name_obj$value %||% ""))
                  if (nchar(name) > 0) break
                }
              }
            } else {
              # Format: { "fr": "...", "de": "...", ... }
              if (language %in% names(ref$name) && nchar(trimws(ref$name[[language]] %||% "")) > 0) {
                name <- trimws(ref$name[[language]])
              }
            }
          }
          
          # Fallback to other languages if requested language not found
          if (name == "") {
            fallback_order <- c("en", "de", "fr", "it")
            for (fallback_lang in fallback_order) {
              if (is.list(ref$name) && length(ref$name) > 0 && is.list(ref$name[[1]]) && "lang" %in% names(ref$name[[1]])) {
                # Format: [{ "lang": "fr", "value": "..." }, ...]
                for (name_obj in ref$name) {
                  if (is.list(name_obj) && name_obj$lang == fallback_lang && "value" %in% names(name_obj)) {
                    name <- trimws(as.character(name_obj$value %||% ""))
                    if (nchar(name) > 0) break
                  }
                }
              } else {
                # Format: { "fr": "...", "de": "...", ... }
                if (fallback_lang %in% names(ref$name) && nchar(trimws(ref$name[[fallback_lang]] %||% "")) > 0) {
                  name <- trimws(ref$name[[fallback_lang]])
                  break
                }
              }
              if (nchar(name) > 0) break
            }
          }
          
          # Final fallback: take first available name
          if (name == "" && length(all_names) > 0) {
            name <- all_names[1]
          }
        } else {
          # Name is a simple string
          name <- trimws(as.character(ref$name))
          all_names <- name
        }
      }
      
      # Extract agencyID and version from the URN (if available in the structure)
      # The URN format is: urn:sdmx:org.sdmx.infomodel.datastructure.Dataflow=AGENCY:ID(VERSION)
      agencyID <- ref$agencyID %||% ""
      version <- ref$version %||% ""
      
      data.frame(
        id = id,
        agencyID = agencyID,
        version = version,
        name = name,
        all_names = paste(all_names, collapse = " "),  # All names concatenated for search
        stringsAsFactors = FALSE
      )
    })
    
    catalog <- do.call(rbind, catalog)
    if (nrow(catalog) == 0) {
      return(data.frame(
        id = character(0),
        agencyID = character(0),
        version = character(0),
        name = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # 5. Filter by keyword (case-insensitive)
    # Search in all names (multilingual) and in id
    if (!is.null(keyword) && keyword != "") {
      # Search in all_names (contains all language versions) and in id
      matches <- grepl(keyword, catalog$all_names, ignore.case = TRUE) | 
                 grepl(keyword, catalog$id, ignore.case = TRUE)
      results <- catalog[matches, , drop = FALSE]
      # Remove the all_names column before returning
      results$all_names <- NULL
    } else {
      results <- catalog
      # Remove the all_names column before returning
      results$all_names <- NULL
    }
    
    if (nrow(results) == 0) {
      return(data.frame(
        id = character(0),
        agencyID = character(0),
        version = character(0),
        name = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    return(results)
    
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
