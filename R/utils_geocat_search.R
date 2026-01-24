#' Recherche dans le catalogue geocat.ch via CSW
#' 
#' @param keyword Mot-clé de recherche
#' @param language Langue pour les résultats (par défaut "fr")
#' @param limit Nombre maximum de résultats (par défaut 50)
#' 
#' @return Data frame avec les résultats de recherche (id, title, abstract, etc.)
#' 
#' @noRd
search_geocat <- function(keyword, language = "fr", limit = 50) {
  # Vérifier que httr et xml2 sont disponibles
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Le package 'httr' est requis pour la recherche geocat.ch")
  }
  
  # Langue pour l'endpoint (fr, de, it, en)
  lang_code <- switch(
    language,
    "fr" = "fre",
    "de" = "ger", 
    "it" = "ita",
    "en" = "eng",
    "eng"  # default
  )
  
  # Essayer d'abord l'API REST de Geonetwork (plus simple et plus fiable)
  base_url <- "http://www.geocat.ch/geonetwork/srv"
  
  # Utiliser l'endpoint Elasticsearch de Geonetwork comme suggéré par l'erreur
  # Format: /srv/api/search/records/_search avec requête POST
  rest_url <- paste0(base_url, "/api/search/records/_search")
  
  # Construire la requête Elasticsearch
  # Format JSON pour Elasticsearch
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    # Si jsonlite n'est pas disponible, passer directement à CSW
  } else {
    es_query <- jsonlite::toJSON(list(
      query = list(
        query_string = list(
          query = keyword,
          default_field = "_all"
        )
      ),
      from = 0,
      size = limit
    ), auto_unbox = TRUE)
    
    # Essayer l'API REST Elasticsearch d'abord
    tryCatch({
      rest_response <- httr::POST(
        rest_url,
        body = es_query,
        httr::add_headers(
          "User-Agent" = "R-httr",
          "Content-Type" = "application/json",
          "Accept" = "application/json"
        ),
        httr::timeout(10)
      )
      
      status_rest <- httr::status_code(rest_response)
    
    if (status_rest == 200) {
      rest_content <- httr::content(rest_response, encoding = "UTF-8", type = "application/json")
      
      # Debug: voir la structure de la réponse
      message("DEBUG REST: Structure de la réponse: ", paste(names(rest_content), collapse = ", "))
      message("DEBUG REST: Type de rest_content: ", class(rest_content))
      if (is.list(rest_content) && length(rest_content) > 0) {
        message("DEBUG REST: Premier élément: ", paste(names(rest_content[[1]]), collapse = ", "))
      }
      
      # La réponse Elasticsearch a une structure hits.hits
      hits <- NULL
      if (is.list(rest_content)) {
        if ("hits" %in% names(rest_content) && is.list(rest_content$hits)) {
          if ("hits" %in% names(rest_content$hits)) {
            hits <- rest_content$hits$hits
            message("DEBUG REST: Utilisé 'hits.hits' (format Elasticsearch)")
          } else {
            hits <- rest_content$hits
            message("DEBUG REST: Utilisé 'hits'")
          }
        } else if ("metadata" %in% names(rest_content)) {
          hits <- rest_content$metadata
          message("DEBUG REST: Utilisé 'metadata'")
        }
      }
      
      message("DEBUG REST: Nombre de hits trouvés: ", if (is.null(hits)) 0 else length(hits))
      
      if (!is.null(hits) && length(hits) > 0) {
        # Parser les résultats REST Elasticsearch
        catalog <- lapply(hits, function(hit) {
          # Extraire _source (format Elasticsearch standard)
          source <- if ("_source" %in% names(hit)) hit$`_source` else hit
          
          uuid <- source$metadataIdentifier %||% source$uuid %||% hit$uuid %||% ""
          
          # Extraire le titre (format multilingue Geonetwork)
          title <- ""
          if ("resourceTitleObject" %in% names(source)) {
            title_obj <- source$resourceTitleObject
            if (is.list(title_obj)) {
              lang_key <- paste0("lang", lang_code)
              title <- title_obj[[lang_key]] %||% title_obj$default %||% title_obj[[1]] %||% ""
            } else {
              title <- as.character(title_obj)
            }
          } else if ("title" %in% names(source)) {
            if (is.list(source$title)) {
              title <- source$title[[language]] %||% source$title$default %||% source$title[[1]] %||% ""
            } else {
              title <- as.character(source$title)
            }
          }
          
          # Extraire l'abstract
          abstract <- ""
          if ("resourceAbstractObject" %in% names(source)) {
            abstract_obj <- source$resourceAbstractObject
            if (is.list(abstract_obj)) {
              lang_key <- paste0("lang", lang_code)
              abstract <- abstract_obj[[lang_key]] %||% abstract_obj$default %||% abstract_obj[[1]] %||% ""
            } else {
              abstract <- as.character(abstract_obj)
            }
          } else if ("abstract" %in% names(source)) {
            if (is.list(source$abstract)) {
              abstract <- source$abstract[[language]] %||% source$abstract$default %||% source$abstract[[1]] %||% ""
            } else {
              abstract <- as.character(source$abstract)
            }
          }
          
          # Extraire les mots-clés
          keywords <- ""
          if ("tag" %in% names(source) && is.list(source$tag)) {
            keywords <- paste(unlist(source$tag), collapse = "; ")
          } else if ("keywords" %in% names(source) && is.list(source$keywords)) {
            keyword_list <- lapply(source$keywords, function(kw) {
              if (is.list(kw) && "value" %in% names(kw)) {
                if (is.list(kw$value)) {
                  kw$value[[language]] %||% kw$value$default %||% kw$value[[1]] %||% ""
                } else {
                  as.character(kw$value)
                }
              } else if (is.character(kw)) {
                kw
              } else {
                ""
              }
            })
            keywords <- paste(unlist(keyword_list), collapse = "; ")
          }
          
          data.frame(
            id = uuid,
            title = title,
            abstract = if (nchar(abstract) > 200) {
              paste0(substr(abstract, 1, 200), "...")
            } else {
              abstract
            },
            subject = keywords,
            stringsAsFactors = FALSE
          )
        })
        
        catalog_df <- do.call(rbind, catalog)
        
        if (nrow(catalog_df) > 0) {
          return(catalog_df)
        }
      }
    } else {
      message("DEBUG REST: Status code non-200: ", status_rest)
      error_content <- httr::content(rest_response, encoding = "UTF-8", type = "text")
      message("DEBUG REST: Contenu de l'erreur (premiers 200 chars): ", substr(error_content, 1, 200))
    }
    }, error = function(e) {
      # Si l'API REST échoue, continuer avec CSW
      message("DEBUG REST: Erreur: ", e$message)
    })
  }
  
  # Si l'API REST n'a pas fonctionné, utiliser CSW
  # Endpoint CSW de geocat.ch
  csw_url <- paste0(base_url, "/", lang_code, "/csw")
  
  # Construire la requête CSW GetRecords avec filtre de recherche
  # Format CSW 2.0.2 avec filtre PropertyIsLike pour la recherche textuelle
  # Échapper les caractères spéciaux dans le keyword
  keyword_escaped <- gsub("&", "&amp;", keyword)
  keyword_escaped <- gsub("<", "&lt;", keyword_escaped)
  keyword_escaped <- gsub(">", "&gt;", keyword_escaped)
  keyword_escaped <- gsub('"', "&quot;", keyword_escaped)
  keyword_escaped <- gsub("'", "&apos;", keyword_escaped)
  
  csw_request <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<csw:GetRecords xmlns:csw="http://www.opengis.net/cat/csw/2.0.2"',
    ' xmlns:ogc="http://www.opengis.net/ogc"',
    ' xmlns:gmd="http://www.isotc211.org/2005/gmd"',
    ' xmlns:dc="http://purl.org/dc/elements/1.1/"',
    ' xmlns:dct="http://purl.org/dc/terms/"',
    ' service="CSW" version="2.0.2" resultType="results"',
    ' maxRecords="', limit, '"',
    ' startPosition="1"',
    ' outputFormat="application/xml"',
    ' outputSchema="http://www.opengis.net/cat/csw/2.0.2">',
    ' <csw:Query typeNames="csw:Record">',
    '   <csw:ElementSetName>summary</csw:ElementSetName>',
    '   <csw:Constraint version="1.1.0">',
    '     <ogc:Filter>',
    '       <ogc:Or>',
    '         <ogc:PropertyIsLike wildCard="*" singleChar="?" escapeChar="\\\\">',
    '           <ogc:PropertyName>dc:title</ogc:PropertyName>',
    '           <ogc:Literal>*', keyword_escaped, '*</ogc:Literal>',
    '         </ogc:PropertyIsLike>',
    '         <ogc:PropertyIsLike wildCard="*" singleChar="?" escapeChar="\\\\">',
    '           <ogc:PropertyName>dc:subject</ogc:PropertyName>',
    '           <ogc:Literal>*', keyword_escaped, '*</ogc:Literal>',
    '         </ogc:PropertyIsLike>',
    '         <ogc:PropertyIsLike wildCard="*" singleChar="?" escapeChar="\\\\">',
    '           <ogc:PropertyName>dct:abstract</ogc:PropertyName>',
    '           <ogc:Literal>*', keyword_escaped, '*</ogc:Literal>',
    '         </ogc:PropertyIsLike>',
    '       </ogc:Or>',
    '     </ogc:Filter>',
    '   </csw:Constraint>',
    ' </csw:Query>',
    '</csw:GetRecords>'
  )
  
  # Faire la requête POST
  response <- httr::POST(
    csw_url,
    body = csw_request,
    httr::content_type("application/xml"),
    httr::add_headers("User-Agent" = "R-httr")
  )
  
  # Vérifier le statut de la réponse
  status <- httr::status_code(response)
  if (status != 200) {
    # Essayer de récupérer le message d'erreur
    error_content <- httr::content(response, encoding = "UTF-8", type = "text")
    error_msg <- paste0(
      "Failed to connect to geocat.ch CSW API. ",
      "Status code: ", status, ". ",
      if (nchar(error_content) > 0 && nchar(error_content) < 500) {
        paste0("Response: ", error_content, ". ")
      } else {
        ""
      },
      "Please check your internet connection and try again."
    )
    stop(error_msg)
  }
  
  # Parser le XML
  tryCatch({
    # Vérifier si xml2 est disponible, sinon utiliser une méthode basique
    if (requireNamespace("xml2", quietly = TRUE)) {
      # httr::content avec type = "text/xml" retourne déjà un xml_document
      # On utilise as = "parsed" pour s'assurer d'obtenir un xml_document
      xml_doc <- httr::content(response, encoding = "UTF-8", type = "text/xml", as = "parsed")
      
      # Obtenir les namespaces
      ns <- xml2::xml_ns(xml_doc)
      
      # Extraire les enregistrements - essayer plusieurs chemins possibles
      # Geonetwork utilise SummaryRecord dans les réponses GetRecords avec ElementSetName=summary
      records <- xml2::xml_find_all(xml_doc, ".//csw:SummaryRecord", ns = ns)
      
      # Si aucun résultat avec SummaryRecord, essayer Record
      if (length(records) == 0) {
        records <- xml2::xml_find_all(xml_doc, ".//csw:Record", ns = ns)
      }
      
      # Si toujours aucun résultat, essayer sans namespace
      if (length(records) == 0) {
        records <- xml2::xml_find_all(xml_doc, ".//SummaryRecord", ns = ns)
      }
      
      if (length(records) == 0) {
        records <- xml2::xml_find_all(xml_doc, ".//Record", ns = ns)
      }
      
      # Si toujours aucun résultat, essayer avec un chemin plus large
      if (length(records) == 0) {
        # Chercher dans la réponse CSW
        records <- xml2::xml_find_all(xml_doc, ".//*[local-name()='SummaryRecord']", ns = ns)
      }
      
      if (length(records) == 0) {
        records <- xml2::xml_find_all(xml_doc, ".//*[local-name()='Record']", ns = ns)
      }
      
      # Debug: voir ce qui est dans le XML
      xml_text <- as.character(xml_doc)
      message("DEBUG CSW: Premiers 500 caractères de la réponse: ", substr(xml_text, 1, 500))
      message("DEBUG CSW: Nombre de records trouvés: ", length(records))
      message("DEBUG CSW: Namespaces disponibles: ", paste(names(ns), collapse = ", "))
      
      if (length(records) == 0) {
        # Vérifier s'il y a des erreurs dans la réponse (sans dépendre du namespace)
        error_nodes <- xml2::xml_find_all(xml_doc, ".//*[local-name()='ExceptionText']", ns = ns)
        if (length(error_nodes) == 0) {
          # Essayer aussi avec Exception
          error_nodes <- xml2::xml_find_all(xml_doc, ".//*[local-name()='Exception']", ns = ns)
        }
        if (length(error_nodes) > 0) {
          error_msg <- paste(xml2::xml_text(error_nodes), collapse = "; ")
          stop(paste0("Erreur CSW: ", error_msg))
        }
        
        # Si pas d'erreur mais pas de résultats, retourner un dataframe vide
        return(data.frame(
          id = character(0),
          title = character(0),
          abstract = character(0),
          subject = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Extraire les informations de chaque enregistrement
      catalog <- lapply(records, function(record) {
        # Extraire l'identifiant - essayer plusieurs chemins
        id <- ""
        id_node <- xml2::xml_find_first(record, ".//dc:identifier", ns = ns)
        if (length(id_node) == 0) {
          id_node <- xml2::xml_find_first(record, ".//*[local-name()='identifier']", ns = ns)
        }
        if (length(id_node) > 0) {
          id <- xml2::xml_text(id_node)
        }
        
        # Extraire le titre
        title <- ""
        title_node <- xml2::xml_find_first(record, ".//dc:title", ns = ns)
        if (length(title_node) == 0) {
          title_node <- xml2::xml_find_first(record, ".//*[local-name()='title']", ns = ns)
        }
        if (length(title_node) > 0) {
          title <- xml2::xml_text(title_node)
        }
        
        # Extraire l'abstract
        abstract <- ""
        abstract_node <- xml2::xml_find_first(record, ".//dct:abstract", ns = ns)
        if (length(abstract_node) == 0) {
          abstract_node <- xml2::xml_find_first(record, ".//*[local-name()='abstract']", ns = ns)
        }
        if (length(abstract_node) > 0) {
          abstract <- xml2::xml_text(abstract_node)
        }
        
        # Extraire les sujets (mots-clés)
        subjects <- ""
        subject_nodes <- xml2::xml_find_all(record, ".//dc:subject", ns = ns)
        if (length(subject_nodes) == 0) {
          subject_nodes <- xml2::xml_find_all(record, ".//*[local-name()='subject']", ns = ns)
        }
        if (length(subject_nodes) > 0) {
          subjects <- paste(xml2::xml_text(subject_nodes), collapse = "; ")
        }
        
        data.frame(
          id = id,
          title = title,
          abstract = if (nchar(abstract) > 200) {
            paste0(substr(abstract, 1, 200), "...")
          } else {
            abstract
          },
          subject = subjects,
          stringsAsFactors = FALSE
        )
      })
      
      catalog_df <- do.call(rbind, catalog)
      
      if (nrow(catalog_df) == 0) {
        return(data.frame(
          id = character(0),
          title = character(0),
          abstract = character(0),
          subject = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      return(catalog_df)
      
    } else {
      # Fallback: parser basique sans xml2 (moins robuste)
      xml_content <- httr::content(response, encoding = "UTF-8", type = "text")
      
      # Extraction basique avec regex (limité mais fonctionnel)
      # Extraire les enregistrements
      records <- regmatches(xml_content, gregexpr("<csw:Record[^>]*>.*?</csw:Record>", xml_content, perl = TRUE))[[1]]
      
      if (length(records) == 0) {
        return(data.frame(
          id = character(0),
          title = character(0),
          abstract = character(0),
          subject = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      catalog <- lapply(records, function(record) {
        # Extraire l'identifiant
        id_match <- regmatches(record, regexpr("<dc:identifier[^>]*>([^<]+)</dc:identifier>", record))
        id <- if (length(id_match) > 0) {
          gsub("<dc:identifier[^>]*>|</dc:identifier>", "", id_match[1])
        } else ""
        
        # Extraire le titre
        title_match <- regmatches(record, regexpr("<dc:title[^>]*>([^<]+)</dc:title>", record))
        title <- if (length(title_match) > 0) {
          gsub("<dc:title[^>]*>|</dc:title>", "", title_match[1])
        } else ""
        
        # Extraire l'abstract
        abstract_match <- regmatches(record, regexpr("<dct:abstract[^>]*>([^<]+)</dct:abstract>", record))
        abstract <- if (length(abstract_match) > 0) {
          gsub("<dct:abstract[^>]*>|</dct:abstract>", "", abstract_match[1])
        } else ""
        
        # Extraire les sujets
        subject_matches <- regmatches(record, gregexpr("<dc:subject[^>]*>([^<]+)</dc:subject>", record))[[1]]
        subjects <- if (length(subject_matches) > 0) {
          paste(gsub("<dc:subject[^>]*>|</dc:subject>", "", subject_matches), collapse = "; ")
        } else ""
        
        data.frame(
          id = id,
          title = title,
          abstract = if (nchar(abstract) > 200) {
            paste0(substr(abstract, 1, 200), "...")
          } else {
            abstract
          },
          subject = subjects,
          stringsAsFactors = FALSE
        )
      })
      
      catalog_df <- do.call(rbind, catalog)
      
      if (nrow(catalog_df) == 0) {
        return(data.frame(
          id = character(0),
          title = character(0),
          abstract = character(0),
          subject = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      return(catalog_df)
    }
    
  }, error = function(e) {
    error_msg <- paste0(
      "Erreur lors du parsing de la réponse CSW: ", e$message, ". ",
      "Veuillez réessayer plus tard."
    )
    stop(error_msg)
  })
}

# Helper function pour %||%
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Obtenir les détails complets d'un dataset geocat.ch
#' 
#' @param record_id Identifiant de l'enregistrement
#' @param language Langue pour les résultats (par défaut "fr")
#' 
#' @return Liste avec les détails complets du dataset
#' 
#' @noRd
get_geocat_record <- function(record_id, language = "fr") {
  # Vérifier que httr est disponible
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Le package 'httr' est requis")
  }
  
  # Langue pour l'endpoint CSW
  lang_code <- switch(
    language,
    "fr" = "fre",
    "de" = "ger", 
    "it" = "ita",
    "en" = "eng",
    "eng"  # default
  )
  
  # Endpoint CSW de geocat.ch
  base_url <- "http://www.geocat.ch/geonetwork/srv"
  csw_url <- paste0(base_url, "/", lang_code, "/csw")
  
  # Construire la requête CSW GetRecordById
  csw_request <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<csw:GetRecordById xmlns:csw="http://www.opengis.net/cat/csw/2.0.2"',
    ' service="CSW" version="2.0.2"',
    ' outputSchema="http://www.isotc211.org/2005/gmd"',
    ' outputFormat="application/xml">',
    ' <csw:Id>', record_id, '</csw:Id>',
    ' <csw:ElementSetName>full</csw:ElementSetName>',
    '</csw:GetRecordById>'
  )
  
  # Faire la requête POST
  response <- httr::POST(
    csw_url,
    body = csw_request,
    httr::content_type("application/xml"),
    httr::add_headers("User-Agent" = "R-httr")
  )
  
  # Vérifier le statut de la réponse
  status <- httr::status_code(response)
  if (status != 200) {
    error_msg <- paste0(
      "Failed to connect to geocat.ch CSW API. ",
      "Status code: ", status
    )
    stop(error_msg)
  }
  
  # Retourner le contenu XML brut (sera parsé par l'appelant si nécessaire)
  xml_content <- httr::content(response, encoding = "UTF-8", type = "text/xml")
  return(xml_content)
}
