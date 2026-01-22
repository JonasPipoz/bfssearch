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
  
  # 2. Request the data (API returns JSON by default with httr::GET)
  response <- httr::GET(url)
  
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
      # Extract id and name
      id <- ref$id %||% ""
      name <- ref$name %||% ""
      
      # Extract agencyID and version from the URN (if available in the structure)
      # The URN format is: urn:sdmx:org.sdmx.infomodel.datastructure.Dataflow=AGENCY:ID(VERSION)
      agencyID <- ref$agencyID %||% ""
      version <- ref$version %||% ""
      
      data.frame(
        id = id,
        agencyID = agencyID,
        version = version,
        name = name,
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
    if (!is.null(keyword) && keyword != "") {
      matches <- grepl(keyword, catalog$name, ignore.case = TRUE) | 
                 grepl(keyword, catalog$id, ignore.case = TRUE)
      results <- catalog[matches, , drop = FALSE]
    } else {
      results <- catalog
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
