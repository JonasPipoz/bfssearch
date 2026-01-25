#' @title Logique serveur de l'application
#' @description Server principal de l'application BFS Search
#' @noRd

app_server <- function(input, output, session) {
  
  # Fonction utilitaire pour formater les noms de variables avec backticks si nécessaire
  format_var_name <- function(var_name) {
    # Si le nom contient des espaces ou des caractères spéciaux, utiliser des backticks
    if (grepl("[^a-zA-Z0-9._]", var_name)) {
      return(paste0("`", var_name, "`"))
    }
    return(var_name)
  }
  
  # Fonction helper pour obtenir le type de source du dataset sélectionné
  get_dataset_source <- function(dataset = NULL) {
    if (is.null(dataset)) {
      dataset <- selected_dataset()
    }
    if (is.null(dataset)) {
      return(NULL)
    }
    
    # Vérifier la colonne "source" en premier
    if ("source" %in% names(dataset)) {
      return(dataset$source)
    }
    
    # Fallback: déterminer à partir des colonnes disponibles
    if ("agencyID" %in% names(dataset) || "version" %in% names(dataset)) {
      return("Swiss Stats Explorer")
    } else if ("organization" %in% names(dataset) || "author" %in% names(dataset)) {
      return("Opendata.swiss")
    } else if ("subject" %in% names(dataset) || "abstract" %in% names(dataset)) {
      return("geocat.ch")
    } else {
      return("BFS Catalog")
    }
  }
  
  # Variables réactives
  catalog_results <- reactiveVal(NULL)
  selected_dataset <- reactiveVal(NULL)
  dataset_metadata <- reactiveVal(NULL)
  metadata_tidy <- reactiveVal(NULL)
  sse_codelist <- reactiveVal(NULL)  # Pour SSE
  opendata_package <- reactiveVal(NULL)  # Pour Opendata.swiss
  geocat_record <- reactiveVal(NULL)  # Pour geocat.ch
  query_dimensions <- reactiveVal(NULL)
  queried_data <- reactiveVal(NULL)
  ai_visualizations <- reactiveVal(NULL)  # Pour stocker les visualisations générées
  
  # Observer pour réinitialiser les résultats lors du changement de catalogues actifs
  observeEvent(input$active_catalogs, {
    # Réinitialiser les résultats si aucun catalogue n'est activé
    if (length(input$active_catalogs) == 0) {
      catalog_results(NULL)
      selected_dataset(NULL)
      dataset_metadata(NULL)
      metadata_tidy(NULL)
      sse_codelist(NULL)
      opendata_package(NULL)
      queried_data(NULL)
    }
  })
  
  # Observer pour ouvrir le modal d'information SSE
  observeEvent(input$sse_info_btn, {
    # Ouvrir le modal Bootstrap via JavaScript
    session$sendCustomMessage("showSSEModal", list())
  })
  
  # Recherche dans les catalogues activés
  observeEvent(input$search_btn, {
    # Vérifier qu'au moins un catalogue est activé
    if (is.null(input$active_catalogs) || length(input$active_catalogs) == 0) {
      showNotification("Veuillez activer au moins un catalogue", type = "warning")
      return()
    }
    
    # Vérifier qu'un terme de recherche est fourni
    if (is.null(input$search_term) || trimws(input$search_term) == "") {
      showNotification("Veuillez entrer un terme de recherche", type = "warning")
      return()
    }
    
    search_keyword <- trimws(input$search_term)
    all_results <- list()
    
    showNotification("Recherche en cours dans les catalogues activés...", type = "message")
    
    # Recherche dans BFS Catalog
    if ("catalog" %in% input$active_catalogs) {
      tryCatch({
        showNotification("Recherche dans BFS Catalog...", type = "message")
        results <- BFS::bfs_get_catalog_data(
          language = "fr",
          extended_search = search_keyword,
          spatial_division = if(input$spatial_division == "") NULL else input$spatial_division,
          limit = input$limit
        )
        
        if (nrow(results) > 0) {
          results$source <- "BFS Catalog"
          all_results[["catalog"]] <- results
        }
      }, error = function(e) {
        showNotification(paste("Erreur BFS Catalog:", e$message), type = "error")
      })
    }
    
    # Recherche dans Swiss Stats Explorer
    if ("sse" %in% input$active_catalogs) {
      tryCatch({
        showNotification("Recherche dans Swiss Stats Explorer...", type = "message")
        results <- search_swiss_stats(keyword = search_keyword, language = "fr")
        
        if (nrow(results) > 0) {
          results_formatted <- results |>
            dplyr::mutate(
              title = name,
              number_bfs = id,
              publication_date = Sys.Date(),
              language_available = "fr",
              source = "Swiss Stats Explorer"
            ) |>
            dplyr::select(dplyr::any_of(c("title", "number_bfs", "publication_date", "language_available", "id", "agencyID", "version", "source")))
          
          all_results[["sse"]] <- results_formatted
        }
      }, error = function(e) {
        showNotification(paste("Erreur Swiss Stats Explorer:", e$message), type = "error")
      })
    }
    
    # Recherche dans Opendata.swiss
    if ("opendata" %in% input$active_catalogs) {
      tryCatch({
        showNotification("Recherche dans Opendata.swiss...", type = "message")
        results <- search_opendata_swiss(keyword = search_keyword, language = "fr")
        
        if (!is.null(results) && nrow(results) > 0) {
          results_formatted <- results |>
            dplyr::mutate(
              title = title,
              number_bfs = id,
              publication_date = Sys.Date(),
              language_available = "fr",
              source = "Opendata.swiss"
            ) |>
            dplyr::select(dplyr::any_of(c("title", "number_bfs", "organization", "author", "publisher", "description", "num_resources", "id", "source")))
          
          all_results[["opendata"]] <- results_formatted
        }
      }, error = function(e) {
        error_msg <- if (!is.null(e$message) && nchar(trimws(e$message)) > 0) {
          trimws(e$message)
        } else {
          "Erreur inconnue"
        }
        showNotification(paste("Erreur Opendata.swiss:", error_msg), type = "error")
      })
    }
    
    # Recherche dans geocat.ch
    if ("geocat" %in% input$active_catalogs) {
      tryCatch({
        showNotification("Recherche dans geocat.ch...", type = "message")
        results <- search_geocat(keyword = search_keyword, language = "fr", limit = input$limit)
        
        if (!is.null(results) && nrow(results) > 0) {
          results_formatted <- results |>
            dplyr::mutate(
              title = title,
              number_bfs = id,
              publication_date = Sys.Date(),
              language_available = "fr",
              source = "geocat.ch"
            ) |>
            dplyr::select(dplyr::any_of(c("title", "number_bfs", "abstract", "subject", "id", "source")))
          
          all_results[["geocat"]] <- results_formatted
        }
      }, error = function(e) {
        error_msg <- if (!is.null(e$message) && nchar(trimws(e$message)) > 0) {
          trimws(e$message)
        } else {
          "Erreur inconnue"
        }
        showNotification(paste("Erreur geocat.ch:", error_msg), type = "error")
      })
    }
    
    # Combiner tous les résultats
    if (length(all_results) > 0) {
      # Combiner les dataframes en gérant les colonnes différentes
      combined_results <- dplyr::bind_rows(all_results)
      catalog_results(combined_results)
      
      total_count <- nrow(combined_results)
      sources_summary <- table(combined_results$source)
      summary_msg <- paste0(total_count, " dataset(s) trouvé(s) (", 
                            paste(paste(sources_summary, names(sources_summary), sep = " "), collapse = ", "), ")")
      showNotification(summary_msg, type = "message", duration = 5)
    } else {
      showNotification("Aucun résultat trouvé dans les catalogues activés. Essayez un autre terme de recherche.", type = "warning")
      catalog_results(NULL)
    }
  })
  
  # Observer pour le numéro BFS direct (SSE uniquement)
  observeEvent(input$sse_number_bfs, {
    if (!is.null(input$sse_number_bfs) && trimws(input$sse_number_bfs) != "") {
      showNotification("Chargement des métadonnées SSE...", type = "message")
      
      tryCatch({
        # Charger le codelist SSE
        codelist <- BFS::bfs_get_sse_metadata(
          number_bfs = trimws(input$sse_number_bfs),
          language = "fr"
        )
        
        sse_codelist(codelist)
        
        # Créer un dataset factice pour la compatibilité
        fake_dataset <- data.frame(
          title = paste0("Dataset SSE: ", trimws(input$sse_number_bfs)),
          number_bfs = trimws(input$sse_number_bfs),
          publication_date = Sys.Date(),
          language_available = "fr",
          source = "Swiss Stats Explorer"
        )
        selected_dataset(fake_dataset)
        
        # Passer directement à l'onglet de configuration
        updateTabsetPanel(session, "main_tabs", selected = "Configuration des filtres")
        
        showNotification("Métadonnées SSE chargées avec succès", type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur lors du chargement SSE:", e$message), type = "error")
        sse_codelist(NULL)
      })
    }
  }, ignoreInit = TRUE)
  
  # Affichage du tableau de résultats
  output$catalog_table <- DT::renderDataTable({
    req(catalog_results())
    
    # Préparer les données pour l'affichage
    results_df <- catalog_results()
    
    # Colonnes principales à afficher
    main_cols <- c("source", "title", "description", "publication_date")
    # Ne garder que les colonnes qui existent dans les données
    main_cols <- main_cols[main_cols %in% colnames(results_df)]
    
    # Toutes les autres colonnes pour le tooltip (exclure id et les colonnes principales)
    all_cols <- colnames(results_df)
    tooltip_cols <- setdiff(all_cols, c("id", main_cols))
    
    # Sélectionner uniquement les colonnes principales
    display_data <- results_df |> 
      dplyr::select(dplyr::any_of(main_cols))
    
    # Créer les tooltips avec les informations supplémentaires
    tooltip_data <- results_df |>
      dplyr::select(dplyr::any_of(tooltip_cols))
    
    # Fonction pour créer le contenu du tooltip pour une ligne
    create_tooltip_text <- function(row_idx) {
      if (nrow(tooltip_data) == 0 || row_idx > nrow(tooltip_data)) {
        return("")
      }
      
      row_data <- tooltip_data[row_idx, , drop = FALSE]
      tooltip_lines <- character(0)
      
      for (col in colnames(row_data)) {
        val <- row_data[[col]]
        if (!is.null(val) && !is.na(val)) {
          val_str <- as.character(val)
          if (val_str != "" && val_str != "NA") {
            col_name <- gsub("_", " ", col)
            col_name <- tools::toTitleCase(col_name)
            # Limiter la longueur pour éviter des tooltips trop longs
            if (nchar(val_str) > 150) {
              val_str <- paste0(substr(val_str, 1, 147), "...")
            }
            tooltip_lines <- c(tooltip_lines, paste0(col_name, ": ", val_str))
          }
        }
      }
      
      if (length(tooltip_lines) > 0) {
        return(paste(tooltip_lines, collapse = "\\n"))
      } else {
        return("")
      }
    }
    
    # Préparer les données JSON pour JavaScript
    tooltip_json <- jsonlite::toJSON(tooltip_data, na = "null")
    
    # Renommer les colonnes pour un affichage plus lisible
    new_names <- colnames(display_data)
    new_names <- gsub("_", " ", new_names)
    new_names <- tools::toTitleCase(new_names)
    colnames(display_data) <- new_names
    
    # Créer le tableau DataTable avec callback pour ajouter les tooltips
    DT::datatable(
      display_data,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json",
          emptyTable = "Aucune donnée disponible",
          loadingRecords = "Chargement...",
          processing = "Traitement en cours..."
        ),
        # Callback pour ajouter les tooltips après le rendu
        drawCallback = DT::JS(paste0("
          function(settings) {
            var api = this.api();
            var rows = api.rows({page: 'current'}).nodes();
            var tooltipData = ", tooltip_json, ";
            var dataIndex = 0;
            
            $(rows).each(function() {
              var row = $(this);
              var rowIndex = api.row(this).index();
              
              if (tooltipData && tooltipData[rowIndex]) {
                var tooltipLines = [];
                var rowData = tooltipData[rowIndex];
                
                for (var key in rowData) {
                  if (rowData.hasOwnProperty(key) && rowData[key] !== null && rowData[key] !== '' && rowData[key] !== undefined) {
                    var keyName = key.replace(/_/g, ' ').replace(/\\b\\w/g, function(l) { return l.toUpperCase(); });
                    var value = String(rowData[key]);
                    if (value.length > 150) {
                      value = value.substring(0, 147) + '...';
                    }
                    tooltipLines.push(keyName + ': ' + value);
                  }
                }
                
                if (tooltipLines.length > 0) {
                  var tooltipText = tooltipLines.join('\\n');
                  row.attr('title', tooltipText);
                  row.css('cursor', 'help');
                }
              }
            });
          }
        "))
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # Gestion de la sélection d'un dataset
  observeEvent(input$catalog_table_rows_selected, {
    if (length(input$catalog_table_rows_selected) > 0 && !is.null(catalog_results())) {
      selected_row <- catalog_results()[input$catalog_table_rows_selected, ]
      selected_dataset(selected_row)
      
      # Changer vers l'onglet de configuration
      updateTabsetPanel(session, "main_tabs", selected = "Configuration des filtres")
      
      # Déterminer le type de source à partir de la colonne "source"
      dataset_source <- if ("source" %in% names(selected_row)) {
        selected_row$source
      } else {
      # Fallback: déterminer à partir des colonnes disponibles
      if ("agencyID" %in% names(selected_row) || "version" %in% names(selected_row)) {
        "Swiss Stats Explorer"
      } else if ("organization" %in% names(selected_row) || "author" %in% names(selected_row)) {
        "Opendata.swiss"
      } else if ("subject" %in% names(selected_row) || "abstract" %in% names(selected_row)) {
        "geocat.ch"
      } else {
        "BFS Catalog"
      }
      }
      
      # Charger les métadonnées selon le type de source
      if (dataset_source == "Swiss Stats Explorer") {
        # Pour SSE, charger directement le codelist
        load_sse_metadata(selected_row$number_bfs)
      } else if (dataset_source == "Opendata.swiss") {
        # Pour Opendata.swiss, charger les détails du package
        load_opendata_package(selected_row$number_bfs)  # number_bfs contient l'id
      } else if (dataset_source == "geocat.ch") {
        # Pour geocat.ch, charger les détails complets
        load_geocat_metadata(selected_row$number_bfs)  # number_bfs contient l'id
      } else if (dataset_source == "geocat.ch") {
        # Pour geocat.ch, charger les détails complets
        load_geocat_metadata(selected_row$number_bfs)  # number_bfs contient l'id
      } else {
        # Pour BFS Catalog, charger les métadonnées normales
        load_metadata(selected_row$number_bfs)
      }
    }
  })
  
  # Fonction pour charger les métadonnées SSE
  load_sse_metadata <- function(number_bfs) {
    showNotification("Chargement des métadonnées SSE...", type = "message")
    
    tryCatch({
      # Charger le codelist SSE
      codelist <- BFS::bfs_get_sse_metadata(
        number_bfs = number_bfs,
        language = "fr"
      )
      
      sse_codelist(codelist)
      showNotification("Métadonnées SSE chargées avec succès", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors du chargement des métadonnées SSE:", e$message), type = "error")
      sse_codelist(NULL)
    })
  }
  
  # Fonction pour charger les métadonnées Opendata.swiss
  load_opendata_package <- function(package_id) {
    showNotification("Chargement des détails du dataset opendata.swiss...", type = "message")
    
    tryCatch({
      # Charger les détails du package
      package_details <- get_opendata_swiss_package(package_id)
      
      opendata_package(package_details)
      showNotification("Détails du dataset chargés avec succès", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors du chargement:", e$message), type = "error")
      opendata_package(NULL)
    })
  }
  
  # Fonction pour charger les métadonnées geocat.ch
  load_geocat_metadata <- function(record_id) {
    showNotification("Chargement des détails du dataset geocat.ch...", type = "message")
    
    tryCatch({
      # Charger les détails complets du record
      record_xml <- get_geocat_record(record_id, language = "fr")
      
      geocat_record(record_xml)
      showNotification("Détails du dataset chargés avec succès", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors du chargement:", e$message), type = "error")
      geocat_record(NULL)
    })
  }
  
  # Fonction pour charger les métadonnées
  load_metadata <- function(number_bfs) {
    showNotification("Chargement des métadonnées...", type = "message")
    
    tryCatch({
      metadata <- BFS::bfs_get_metadata(number_bfs = number_bfs, language = "fr")
      dataset_metadata(metadata)
      
      # Tidy metadata
      metadata_tidy_df <- metadata |> 
        tidyr::unnest_longer(c(values, valueTexts))
      
      metadata_tidy(metadata_tidy_df)
      showNotification("Métadonnées chargées avec succès", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors du chargement des métadonnées:", e$message), type = "error")
      dataset_metadata(NULL)
      metadata_tidy(NULL)
    })
  }
  
  # Affichage des informations du dataset
  output$dataset_info <- renderUI({
    req(selected_dataset())
    
    dataset <- selected_dataset()
    
    dataset_source <- get_dataset_source(dataset)
    if (dataset_source == "Opendata.swiss" && !is.null(opendata_package())) {
      # Pour Opendata.swiss, afficher plus d'informations
      package <- opendata_package()
      
      # Extraire la description
      description <- ""
      if ("description" %in% names(package) && is.list(package$description)) {
        description <- package$description$fr %||% package$description$en %||% ""
      } else if ("description" %in% names(package)) {
        desc_val <- package$description
        if (length(desc_val) > 1) {
          description <- paste(desc_val, collapse = ", ")
        } else {
          description <- as.character(desc_val)
        }
      }
      
      # Extraire l'organisation
      org_name <- ""
      if ("organization" %in% names(package) && is.list(package$organization)) {
        if ("title" %in% names(package$organization) && is.list(package$organization$title)) {
          org_name <- package$organization$title$fr %||% package$organization$title$en %||% package$organization$title$de %||% ""
        } else if ("name" %in% names(package$organization)) {
          org_name <- package$organization$name
        }
      }
      
      # Construire la liste des éléments à afficher dans le div
      div_elements <- list(
        tags$p(tags$strong("Titre:"), dataset$title),
        tags$p(tags$strong("ID:"), dataset$number_bfs)
      )
      
      # Ajouter l'organisation si elle existe et n'est pas vide
      if (length(org_name) > 0 && !is.null(org_name)) {
        org_str <- if (length(org_name) > 1) paste(org_name, collapse = ", ") else as.character(org_name)
        if (nchar(trimws(org_str)) > 0) {
          div_elements[[length(div_elements) + 1]] <- tags$p(tags$strong("Organisation:"), org_str)
        }
      }
      
      # Ajouter la description si elle existe et n'est pas vide
      if (length(description) > 0 && !is.null(description)) {
        desc_str <- if (length(description) > 1) paste(description, collapse = ", ") else as.character(description)
        if (nchar(trimws(desc_str)) > 0) {
          div_elements[[length(div_elements) + 1]] <- tags$p(tags$strong("Description:"), tags$br(), desc_str)
        }
      }
      
      # Ajouter le nombre de ressources
      num_res <- package$num_resources %||% length(package$resources %||% list())
      if (length(num_res) > 1) num_res <- length(num_res)
      div_elements[[length(div_elements) + 1]] <- tags$p(tags$strong("Nombre de ressources:"), as.character(num_res))
      
      tagList(
        h4("Dataset sélectionné"),
        tags$div(
          class = "well",
          div_elements
        )
      )
    } else if (dataset_source == "geocat.ch") {
      # Pour geocat.ch, afficher les informations disponibles
      div_elements <- list(
        tags$p(tags$strong("Titre:"), dataset$title),
        tags$p(tags$strong("ID:"), dataset$number_bfs)
      )
      
      # Ajouter l'abstract si disponible
      if ("abstract" %in% names(dataset) && !is.null(dataset$abstract) && nchar(trimws(dataset$abstract)) > 0) {
        div_elements[[length(div_elements) + 1]] <- tags$p(tags$strong("Résumé:"), tags$br(), dataset$abstract)
      }
      
      # Ajouter les sujets si disponibles
      if ("subject" %in% names(dataset) && !is.null(dataset$subject) && nchar(trimws(dataset$subject)) > 0) {
        div_elements[[length(div_elements) + 1]] <- tags$p(tags$strong("Mots-clés:"), dataset$subject)
      }
      
      tagList(
        h4("Dataset sélectionné"),
        tags$div(
          class = "well",
          div_elements
        ),
        tags$p(
          tags$small(
            "Note: geocat.ch est un catalogue de métadonnées géographiques. ",
            "Les données peuvent nécessiter un accès spécialisé pour être téléchargées."
          )
        )
      )
    } else {
      # Pour BFS Catalog et SSE
      tagList(
        h4("Dataset sélectionné"),
        tags$div(
          class = "well",
          tags$p(tags$strong("Titre:"), dataset$title),
          tags$p(tags$strong("Numéro BFS:"), dataset$number_bfs),
          tags$p(tags$strong("Date de publication:"), as.character(dataset$publication_date)),
          tags$p(tags$strong("Langues disponibles:"), dataset$language_available)
        )
      )
    }
  })
  
  # Affichage conditionnel du bouton de requête
  output$query_button_ui <- renderUI({
    dataset_source <- get_dataset_source()
    if (!is.null(dataset_source) && dataset_source != "Opendata.swiss" && dataset_source != "geocat.ch") {
      return(actionButton("query_btn", "Interroger les données", class = "btn-success", width = "100%"))
    } else {
      return(NULL)
    }
  })
  
  # Génération dynamique des filtres
  output$dynamic_filters <- renderUI({
    # Vérifier le type de source
    dataset_source <- get_dataset_source()
    if (dataset_source == "geocat.ch") {
      # Pour geocat.ch, afficher un message informatif
      req(geocat_record())
      tagList(
        tags$div(
          class = "alert alert-info",
          tags$h5("Métadonnées geocat.ch"),
          tags$p("Les métadonnées complètes sont disponibles. ",
                 "Pour télécharger les données, veuillez consulter le catalogue geocat.ch directement."),
          tags$p(tags$strong("ID du record:"), selected_dataset()$number_bfs),
          tags$a(
            href = paste0("https://www.geocat.ch/geonetwork/srv/fre/catalog.search#/metadata/", selected_dataset()$number_bfs),
            target = "_blank",
            "Voir sur geocat.ch",
            class = "btn btn-primary"
          )
        )
      )
    } else if (dataset_source == "Opendata.swiss") {
      # Pour Opendata.swiss, afficher les ressources disponibles
      req(opendata_package())
      
      package <- opendata_package()
      resources <- package$resources %||% list()
      
      if (length(resources) == 0) {
        return(tags$p("Aucune ressource disponible pour ce dataset."))
      }
      
      # Créer une liste des ressources avec leurs informations
      resources_ui <- lapply(seq_along(resources), function(i) {
        res <- resources[[i]]
        
        # Extraire les informations de la ressource
        res_name <- ""
        if ("name" %in% names(res) && is.list(res$name)) {
          res_name <- res$name$fr %||% res$name$en %||% ""
        } else if ("name" %in% names(res)) {
          res_name <- as.character(res$name)
        }
        
        res_title <- ""
        if ("title" %in% names(res) && is.list(res$title)) {
          res_title <- res$title$fr %||% res$title$en %||% ""
        } else if ("title" %in% names(res)) {
          res_title <- as.character(res$title)
        }
        
        res_description <- ""
        if ("description" %in% names(res) && is.list(res$description)) {
          res_description <- res$description$fr %||% res$description$en %||% ""
        } else if ("description" %in% names(res)) {
          res_description <- as.character(res$description)
        }
        
        res_format <- res$format %||% "N/A"
        download_url <- res$download_url %||% res$url %||% ""
        
        tags$div(
          class = "well",
          style = "margin-bottom: 15px;",
          tags$h5(if (res_title != "") res_title else if (res_name != "") res_name else paste("Ressource", i)),
          if (res_description != "") tags$p(tags$em(res_description)),
          tags$p(tags$strong("Format:"), res_format),
          if (download_url != "") {
            tags$p(
              tags$a(
                href = download_url,
                target = "_blank",
                class = "btn btn-primary btn-sm",
                "Télécharger",
                tags$span(icon("external-link-alt"), style = "margin-left: 5px;")
              )
            )
          }
        )
      })
      
      # Fonction pour formater une valeur de métadonnée
      format_metadata_value <- function(value, field_name) {
        # Vérifier si value est NULL
        if (is.null(value)) {
          return(NULL)
        }
        
        # Pour les caractères, vérifier si c'est vide (en gérant les vecteurs)
        if (is.character(value)) {
          # Si c'est un vecteur, vérifier si tous les éléments sont vides
          if (length(value) > 1) {
            # Si c'est un vecteur avec plusieurs éléments, les joindre
            non_empty <- trimws(value) != ""
            if (any(non_empty)) {
              return(paste(value[non_empty], collapse = ", "))
            } else {
              return(NULL)
            }
          } else {
            # Un seul élément
            if (length(value) == 0 || trimws(value[1]) == "") {
              return(NULL)
            }
            return(value[1])
          }
        }
        
        if (is.list(value)) {
          # Si c'est une liste multilingue, afficher toutes les langues
          if (length(value) > 0) {
            lang_values <- lapply(names(value), function(lang) {
              val <- value[[lang]]
              if (!is.null(val)) {
                val_char <- as.character(val)
                # Vérifier que val_char n'est pas un vecteur vide
                if (length(val_char) > 0) {
                  non_empty <- trimws(val_char) != ""
                  if (any(non_empty)) {
                    val_clean <- paste(val_char[non_empty], collapse = ", ")
                    if (nchar(val_clean) > 0) {
                      return(tags$span(tags$strong(lang, ":"), val_clean, tags$br()))
                    }
                  }
                }
              }
              return(NULL)
            })
            # Filtrer les NULL
            lang_values <- lang_values[!sapply(lang_values, is.null)]
            if (length(lang_values) > 0) {
              return(tagList(lang_values))
            }
          }
          return(NULL)
        } else if (is.numeric(value) || is.logical(value)) {
          if (length(value) == 1) {
            return(as.character(value))
          } else {
            return(paste(value, collapse = ", "))
          }
        } else {
          return(toString(value))
        }
      }
      
      # Liste des métadonnées importantes à afficher
      metadata_fields <- list(
        "Identifiant" = "id",
        "Identifiant alternatif" = "identifier",
        "Titre" = "title",
        "Nom d'affichage" = "display_name",
        "Auteur" = "author",
        "Email auteur" = "author_email",
        "Éditeur" = "publisher",
        "Organisation" = "organization",
        "Mainteneur" = "maintainer",
        "Email mainteneur" = "maintainer_email",
        "Date de publication" = "issued",
        "Date de modification" = "modified",
        "Date de création (métadonnées)" = "metadata_created",
        "Date de modification (métadonnées)" = "metadata_modified",
        "Langue" = "language",
        "Licence" = "license_title",
        "ID Licence" = "license_id",
        "Couverture spatiale" = "spatial",
        "Couverture temporelle" = "temporals",
        "Fréquence de mise à jour" = "accrual_periodicity",
        "Mots-clés" = "keywords",
        "Tags" = "tags",
        "URL" = "url",
        "Version" = "version",
        "Type" = "type",
        "État" = "state",
        "Privé" = "private",
        "Ouvert" = "isopen",
        "Nombre de ressources" = "num_resources",
        "Nombre de tags" = "num_tags"
      )
      
      # Créer l'affichage des métadonnées
      metadata_ui <- lapply(names(metadata_fields), function(field_label) {
        field_name <- metadata_fields[[field_label]]
        if (field_name %in% names(package)) {
          value <- package[[field_name]]
          formatted_value <- format_metadata_value(value, field_name)
          
          if (!is.null(formatted_value)) {
            # Gestion spéciale pour certains champs
            if (field_name == "organization" && is.list(value)) {
              org_display <- ""
              if ("title" %in% names(value)) {
                if (is.list(value$title)) {
                  org_display <- value$title$fr %||% value$title$en %||% value$title[[1]] %||% ""
                } else {
                  org_display <- as.character(value$title)
                }
              } else if ("name" %in% names(value)) {
                org_display <- as.character(value$name)
              }
              if (org_display != "") {
                return(tags$p(tags$strong(field_label, ":"), org_display))
              }
            } else if (field_name == "keywords" && is.list(value)) {
              # Keywords peut être une liste de listes
              keywords_list <- unlist(value, recursive = TRUE)
              if (length(keywords_list) > 0) {
                keywords_str <- paste(unique(keywords_list), collapse = ", ")
                return(tags$p(tags$strong(field_label, ":"), keywords_str))
              }
            } else if (field_name == "tags" && is.list(value)) {
              # Tags est une liste d'objets avec 'name'
              tag_names <- sapply(value, function(tag) tag$name %||% "")
              if (length(tag_names) > 0) {
                tags_str <- paste(tag_names[tag_names != ""], collapse = ", ")
                return(tags$p(tags$strong(field_label, ":"), tags_str))
              }
            } else {
              return(tags$p(tags$strong(field_label, ":"), formatted_value))
            }
          }
        }
        return(NULL)
      })
      
      # Filtrer les NULL
      metadata_ui <- metadata_ui[!sapply(metadata_ui, is.null)]
      
      tagList(
        h4("Métadonnées du dataset"),
        tags$div(
          class = "well",
          style = "max-height: 400px; overflow-y: auto;",
          metadata_ui
        ),
        br(),
        h4("Ressources disponibles"),
        tags$div(
          class = "alert alert-info",
          tags$p(
            tags$strong("Note:"),
            " Les ressources opendata.swiss sont des fichiers à télécharger. ",
            "Cliquez sur 'Télécharger' pour accéder au fichier."
          )
        ),
        resources_ui
      )
    } else if (dataset_source == "Swiss Stats Explorer") {
      # Pour SSE, utiliser le codelist
      req(sse_codelist())
      
      codelist_df <- sse_codelist()
      
      # Obtenir les dimensions uniques (codes)
      dimensions <- unique(codelist_df$code)
      
      if (length(dimensions) == 0) {
        return(tags$p("Aucune dimension disponible pour ce dataset SSE."))
      }
      
      # Créer un filtre pour chaque dimension
      filter_ui <- lapply(dimensions, function(dim_code) {
        dim_data <- codelist_df |> 
          dplyr::filter(code == dim_code)
        
        dim_name <- unique(dim_data$text)[1]
        dim_valueTexts <- unique(dim_data$valueText)
        dim_values <- unique(dim_data$value)
        
        # Créer les choix pour le selectInput (utiliser valueText comme label)
        choices <- setNames(dim_valueTexts, dim_valueTexts)
        
        selectInput(
          inputId = paste0("sse_filter_", dim_code),
          label = paste0(dim_name, " (", dim_code, ")"),
          choices = choices,
          multiple = TRUE,
          selectize = TRUE
        )
      })
      
      # Ajouter les champs de période pour SSE
      period_ui <- tagList(
        h5("Période"),
        fluidRow(
          column(6,
            textInput("sse_start_period", label = "Début (année)", placeholder = "Ex: 2020")
          ),
          column(6,
            textInput("sse_end_period", label = "Fin (année)", placeholder = "Ex: 2023")
          )
        )
      )
      
      tagList(
        h4("Filtres disponibles (SSE)"),
        tags$div(
          class = "alert alert-info",
          tags$p(tags$strong("Note:"), "Sélectionnez les valeurs pour chaque dimension. Laissez vide pour inclure toutes les valeurs."),
          tags$p("Pour SSE, vous devez sélectionner au moins une valeur par dimension.")
        ),
        period_ui,
        br(),
        filter_ui
      )
    } else {
      # Pour BFS Catalog, utiliser metadata_tidy
      req(metadata_tidy())
      
      metadata_df <- metadata_tidy()
      
      # Obtenir les dimensions uniques
      dimensions <- unique(metadata_df$code)
      
      if (length(dimensions) == 0) {
        return(tags$p("Aucune dimension disponible pour ce dataset."))
      }
      
      # Créer un filtre pour chaque dimension
      filter_ui <- lapply(dimensions, function(dim_code) {
        dim_data <- metadata_df |> 
          dplyr::filter(code == dim_code)
        
        dim_name <- unique(dim_data$text)[1]
        dim_values <- unique(dim_data$valueTexts)
        dim_codes <- unique(dim_data$values)
        
        # Créer les choix pour le selectInput
        choices <- setNames(dim_codes, dim_values)
        
        selectInput(
          inputId = paste0("filter_", dim_code),
          label = paste0(dim_name, " (", dim_code, ")"),
          choices = choices,
          multiple = TRUE,
          selectize = TRUE
        )
      })
      
      tagList(
        h4("Filtres disponibles"),
        tags$div(
          class = "alert alert-info",
          tags$p(tags$strong("Note:"), "Sélectionnez les valeurs pour chaque dimension. Laissez vide pour inclure toutes les valeurs."),
          tags$p("Vous pouvez interroger les données sans sélectionner de filtres pour obtenir toutes les données disponibles.")
        ),
        filter_ui
      )
    }
  })
  
  # Construction de la requête
  observeEvent(input$query_btn, {
    showNotification("Interrogation des données...", type = "message")
    
    tryCatch({
      dataset_source <- get_dataset_source()
      if (dataset_source == "Swiss Stats Explorer") {
        # Pour SSE
        req(sse_codelist())
        req(selected_dataset())
        
        codelist_df <- sse_codelist()
        dataset <- selected_dataset()
        number_bfs <- dataset$number_bfs
        
        # Construire la requête SSE en filtrant le codelist
        dimensions <- unique(codelist_df$code)
        filter_conditions <- list()
        
        for (dim_code in dimensions) {
          input_id <- paste0("sse_filter_", dim_code)
          selected_valueTexts <- input[[input_id]]
          
          if (!is.null(selected_valueTexts) && length(selected_valueTexts) > 0) {
            # Filtrer le codelist pour cette dimension
            dim_filter <- codelist_df |> 
              dplyr::filter(code == dim_code & valueText %in% selected_valueTexts)
            filter_conditions[[dim_code]] <- dim_filter
          }
        }
        
        if (length(filter_conditions) == 0) {
          showNotification("Veuillez sélectionner au moins une valeur pour chaque dimension.", type = "warning")
          return()
        }
        
        # Combiner tous les filtres
        querydat <- dplyr::bind_rows(filter_conditions)
        
        # Construire la requête avec tapply (comme dans la doc)
        query <- tapply(querydat$value, querydat$code, c)
        
        query_dimensions(query)
        
        # Récupérer les périodes
        start_period <- if(input$sse_start_period == "") NULL else input$sse_start_period
        end_period <- if(input$sse_end_period == "") NULL else input$sse_end_period
        
        # Interroger les données SSE
        data_result <- BFS::bfs_get_sse_data(
          number_bfs = number_bfs,
          language = "fr",
          query = query,
          start_period = start_period,
          end_period = end_period
        )
        
      } else {
        # Pour BFS Catalog
        req(metadata_tidy())
        req(selected_dataset())
        
        metadata_df <- metadata_tidy()
        dimensions <- unique(metadata_df$code)
        
        # Construire l'objet query
        query_list <- list()
        
        for (dim_code in dimensions) {
          input_id <- paste0("filter_", dim_code)
          selected_values <- input[[input_id]]
          
          if (!is.null(selected_values) && length(selected_values) > 0) {
            query_list[[dim_code]] <- selected_values
          }
        }
        
        query_dimensions(query_list)
        
        # Interroger les données
        dataset <- selected_dataset()
        
        # Si aucun filtre n'est sélectionné, récupérer toutes les données
        if (length(query_list) == 0) {
          showNotification("Aucun filtre sélectionné. Récupération de toutes les données...", type = "message")
          data_result <- BFS::bfs_get_data(
            number_bfs = dataset$number_bfs,
            language = "fr",
            delay = 2  # Délai pour éviter les erreurs "Too Many Requests"
          )
        } else {
          data_result <- BFS::bfs_get_data(
            number_bfs = dataset$number_bfs,
            language = "fr",
            query = query_list,
            delay = 2  # Délai pour éviter les erreurs "Too Many Requests"
          )
        }
      }
      
      queried_data(data_result)
      updateTabsetPanel(session, "main_tabs", selected = "Données")
      showNotification("Données chargées avec succès!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'interrogation:", e$message), type = "error")
      queried_data(NULL)
    })
  })
  
  # Affichage des données
  output$data_table <- DT::renderDataTable({
    req(queried_data())
    
    DT::datatable(
      queried_data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json")
      ),
      rownames = FALSE
    )
  })
  
  # Téléchargement des données
  output$download_data <- downloadHandler(
    filename = function() {
      dataset <- selected_dataset()
      paste0("bfs_data_", dataset$number_bfs, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(queried_data())
      write.csv(queried_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Informations sur les données
  output$data_info <- renderText({
    req(queried_data())
    paste0(
      "Nombre de lignes: ", nrow(queried_data()), "\n",
      "Nombre de colonnes: ", ncol(queried_data())
    )
  })
  
  # Statut de recherche
  output$search_status <- renderText({
    if (is.null(catalog_results())) {
      "Aucune recherche effectuée. Entrez un terme de recherche et cliquez sur 'Rechercher'."
    } else {
      paste0(
        "Résultats trouvés: ", nrow(catalog_results()), " dataset(s)\n",
        "Colonnes disponibles: ", paste(colnames(catalog_results()), collapse = ", ")
      )
    }
  })
  
  # Debug info
  output$debug_info <- renderText({
    if (!is.null(catalog_results())) {
      paste0(
        "Debug - Nombre de lignes: ", nrow(catalog_results()), "\n",
        "Debug - Colonnes: ", paste(colnames(catalog_results()), collapse = ", ")
      )
    }
  })
  
  # Statut de la requête
  output$query_status <- renderText({
    if (is.null(query_dimensions())) {
      "Aucune requête effectuée. Configurez les filtres et cliquez sur 'Interroger les données'."
    } else {
      paste0("Requête configurée avec ", length(query_dimensions()), " dimension(s)")
    }
  })
  
  # Génération du code R
  output$r_code <- renderText({
    # Vérifier qu'un dataset est sélectionné
    if (is.null(selected_dataset())) {
      return("# Sélectionnez d'abord un dataset dans l'onglet 'Résultats de recherche'")
    }
    
    dataset <- selected_dataset()
    number_bfs <- dataset$number_bfs
    
    # Construire le code R selon le type de source
    dataset_source <- get_dataset_source(dataset)
    if (dataset_source == "Swiss Stats Explorer") {
      # Code pour Swiss Stats Explorer
      code_lines <- c(
        "# Charger les données du Swiss Stats Explorer (SSE)",
        paste0("# Dataset: ", dataset$title),
        paste0("# Numéro BFS: ", number_bfs),
        "",
        "# Installer le package BFS si nécessaire",
        "# install.packages('BFS')",
        "",
        "library(BFS)",
        "library(dplyr)",
        "",
        "# Obtenir le codelist (métadonnées)",
        paste0('codelist <- bfs_get_sse_metadata("', number_bfs, '", language = "fr")'),
        ""
      )
      
      # Ajouter les filtres si configurés
      if (!is.null(sse_codelist())) {
        codelist_df <- sse_codelist()
        dimensions <- unique(codelist_df$code)
        filter_code_lines <- c()
        
        for (dim_code in dimensions) {
          input_id <- paste0("sse_filter_", dim_code)
          selected_valueTexts <- input[[input_id]]
          
          if (!is.null(selected_valueTexts) && length(selected_valueTexts) > 0) {
            dim_data <- codelist_df |> 
              dplyr::filter(code == dim_code)
            dim_name <- unique(dim_data$text)[1]
            
            # Générer le code de filtre
            valueTexts_str <- paste0('c("', paste(selected_valueTexts, collapse = '", "'), '")')
            filter_code_lines <- c(
              filter_code_lines,
              paste0("# Filtrer pour ", dim_name, " (", dim_code, ")"),
              paste0('querydat_', dim_code, ' <- codelist |>  '),
              paste0('  filter(code == "', dim_code, '" & valueText %in% ', valueTexts_str, ')')
            )
          }
        }
        
        if (length(filter_code_lines) > 0) {
          # Générer les noms des variables querydat
          dim_vars <- unique(sapply(strsplit(filter_code_lines[grep("querydat_", filter_code_lines)], " <-"), function(x) x[1]))
          bind_rows_code <- paste0("querydat <- bind_rows(", paste(dim_vars, collapse = ", "), ")")
          
          code_lines <- c(
            code_lines,
            "# Construire la requête en filtrant le codelist",
            "# Filtrer pour chaque dimension",
            filter_code_lines,
            "",
            "# Combiner les filtres",
            bind_rows_code,
            "",
            "# Construire l'objet query avec tapply",
            "query <- tapply(querydat$value, querydat$code, c)",
            "",
            "# Charger les données avec filtres"
          )
        } else {
          code_lines <- c(
            code_lines,
            "# Construire la requête (sélectionner toutes les valeurs)",
            "# Note: Vous devez filtrer le codelist selon vos besoins",
            "# Exemple: querydat <- codelist |>  filter(code == 'GR_KT_GDE' & valueText %in% c('Aarau', 'Olten'))",
            "querydat <- codelist",
            "query <- tapply(querydat$value, querydat$code, c)",
            "",
            "# Charger les données"
          )
        }
        
        # Ajouter les périodes
        start_period <- if(input$sse_start_period != "") input$sse_start_period else "NULL"
        end_period <- if(input$sse_end_period != "") input$sse_end_period else "NULL"
        
        code_lines <- c(
          code_lines,
          paste0('data <- bfs_get_sse_data('),
          paste0('  number_bfs = "', number_bfs, '",'),
          paste0('  language = "fr",'),
          paste0('  query = query'),
          if(start_period != "NULL") paste0(', start_period = "', start_period, '"') else "",
          if(end_period != "NULL") paste0(', end_period = "', end_period, '"') else "",
          paste0(')'),
          "",
          "# Afficher les données",
          "print(data)",
          "",
          "# Ou sauvegarder en CSV",
          paste0('# write.csv(data, "sse_data_', number_bfs, '_', Sys.Date(), '.csv", row.names = FALSE)')
        )
      } else {
        code_lines <- c(
          code_lines,
          "# Charger les données (sans filtres)",
          paste0('data <- bfs_get_sse_data('),
          paste0('  number_bfs = "', number_bfs, '",'),
          paste0('  language = "fr"'),
          paste0(')'),
          "",
          "# Afficher les données",
          "print(data)"
        )
      }
    } else {
      # Code pour BFS Catalog (PXWeb)
      code_lines <- c(
        "# Charger les données du BFS Catalog (PXWeb)",
        paste0("# Dataset: ", dataset$title),
        paste0("# Numéro BFS: ", number_bfs),
        "",
        "# Installer le package BFS si nécessaire",
        "# install.packages('BFS')",
        "",
        "library(BFS)",
        ""
      )
      
      # Vérifier si des filtres sont configurés
      if (!is.null(metadata_tidy())) {
        metadata_df <- metadata_tidy()
        dimensions <- unique(metadata_df$code)
        
        # Construire l'objet query
        query_list <- list()
        query_code_lines <- c()
        
        for (dim_code in dimensions) {
          input_id <- paste0("filter_", dim_code)
          selected_values <- input[[input_id]]
          
          if (!is.null(selected_values) && length(selected_values) > 0) {
            query_list[[dim_code]] <- selected_values
            
            # Générer le code pour cette dimension
            dim_data <- metadata_df |> 
              dplyr::filter(code == dim_code)
            dim_name <- unique(dim_data$text)[1]
            
            # Formater les valeurs sélectionnées
            values_str <- paste0('c("', paste(selected_values, collapse = '", "'), '")')
            # Formater le nom de variable avec backticks si nécessaire
            formatted_dim_code <- format_var_name(dim_code)
            query_code_lines <- c(
              query_code_lines,
              paste0("# ", dim_name, " (", dim_code, ")"),
              paste0('query$', formatted_dim_code, ' <- ', values_str)
            )
          }
        }
        
        if (length(query_list) > 0) {
          code_lines <- c(
            code_lines,
            "# Construire la requête avec filtres",
            "query <- list()",
            "",
            query_code_lines,
            "",
            "# Charger les données avec filtres",
            paste0('data <- bfs_get_data('),
            paste0('  number_bfs = "', number_bfs, '",'),
            paste0('  language = "fr",'),
            paste0('  query = query'),
            paste0(')'),
            "",
            "# Afficher les données",
            "print(data)",
            "",
            "# Ou sauvegarder en CSV",
            paste0('# write.csv(data, "bfs_data_', number_bfs, '_', Sys.Date(), '.csv", row.names = FALSE)')
          )
        } else {
          # Pas de filtres, charger toutes les données
          code_lines <- c(
            code_lines,
            "# Charger toutes les données (sans filtres)",
            paste0('data <- bfs_get_data('),
            paste0('  number_bfs = "', number_bfs, '",'),
            paste0('  language = "fr"'),
            paste0(')'),
            "",
            "# Afficher les données",
            "print(data)",
            "",
            "# Ou sauvegarder en CSV",
            paste0('# write.csv(data, "bfs_data_', number_bfs, '_', Sys.Date(), '.csv", row.names = FALSE)')
          )
        }
      } else {
        # Métadonnées non disponibles, code simple
        code_lines <- c(
          code_lines,
          "# Charger les données",
          paste0('data <- bfs_get_data('),
          paste0('  number_bfs = "', number_bfs, '",'),
          paste0('  language = "fr"'),
          paste0(')'),
          "",
          "# Afficher les données",
          "print(data)"
        )
      }
    }
    
    paste(code_lines, collapse = "\n")
  })
  
  # Bouton pour copier le code
  observeEvent(input$copy_code_btn, {
    # Générer le code à copier
    if (is.null(selected_dataset())) {
      showNotification("Veuillez d'abord sélectionner un dataset", type = "warning")
      return()
    }
    
    dataset <- selected_dataset()
    number_bfs <- dataset$number_bfs
    
    # Générer le code selon le type de source
    dataset_source <- get_dataset_source(dataset)
    if (dataset_source == "Swiss Stats Explorer") {
      code_lines <- c(
        "library(BFS)",
        "library(dplyr)",
        "",
        paste0('codelist <- bfs_get_sse_metadata("', number_bfs, '", language = "fr")'),
        ""
      )
      
      if (!is.null(sse_codelist())) {
        codelist_df <- sse_codelist()
        dimensions <- unique(codelist_df$code)
        filter_code_lines <- c()
        
        for (dim_code in dimensions) {
          input_id <- paste0("sse_filter_", dim_code)
          selected_valueTexts <- input[[input_id]]
          
          if (!is.null(selected_valueTexts) && length(selected_valueTexts) > 0) {
            valueTexts_str <- paste0('c("', paste(selected_valueTexts, collapse = '", "'), '")')
            filter_code_lines <- c(
              filter_code_lines,
              paste0('querydat_', dim_code, ' <- codelist |>  filter(code == "', dim_code, '" & valueText %in% ', valueTexts_str, ')')
            )
          }
        }
        
        if (length(filter_code_lines) > 0) {
          dim_vars <- unique(sapply(strsplit(filter_code_lines[grep("querydat_", filter_code_lines)], " <-"), function(x) x[1]))
          bind_rows_code <- paste0("querydat <- bind_rows(", paste(dim_vars, collapse = ", "), ")")
          
          code_lines <- c(
            code_lines,
            filter_code_lines,
            "",
            bind_rows_code,
            "",
            "query <- tapply(querydat$value, querydat$code, c)",
            ""
          )
        } else {
          code_lines <- c(
            code_lines,
            "querydat <- codelist",
            "query <- tapply(querydat$value, querydat$code, c)",
            ""
          )
        }
        
        start_period <- if(input$sse_start_period != "") paste0(', start_period = "', input$sse_start_period, '"') else ""
        end_period <- if(input$sse_end_period != "") paste0(', end_period = "', input$sse_end_period, '"') else ""
        
        code_lines <- c(
          code_lines,
          paste0('data <- bfs_get_sse_data(number_bfs = "', number_bfs, '", language = "fr", query = query', start_period, end_period, ')')
        )
      } else {
        code_lines <- c(
          code_lines,
          paste0('data <- bfs_get_sse_data(number_bfs = "', number_bfs, '", language = "fr")')
        )
      }
    } else {
      # BFS Catalog
      code_lines <- c(
        "library(BFS)",
        ""
      )
      
      if (!is.null(metadata_tidy())) {
        metadata_df <- metadata_tidy()
        dimensions <- unique(metadata_df$code)
        query_code_lines <- c()
        
        for (dim_code in dimensions) {
          input_id <- paste0("filter_", dim_code)
          selected_values <- input[[input_id]]
          
          if (!is.null(selected_values) && length(selected_values) > 0) {
            values_str <- paste0('c("', paste(selected_values, collapse = '", "'), '")')
            # Formater le nom de variable avec backticks si nécessaire
            formatted_dim_code <- format_var_name(dim_code)
            query_code_lines <- c(
              query_code_lines,
              paste0('query$', formatted_dim_code, ' <- ', values_str)
            )
          }
        }
        
        if (length(query_code_lines) > 0) {
          code_lines <- c(
            code_lines,
            "query <- list()",
            "",
            query_code_lines,
            "",
            paste0('data <- bfs_get_data(number_bfs = "', number_bfs, '", language = "fr", query = query)')
          )
        } else {
          code_lines <- c(
            code_lines,
            paste0('data <- bfs_get_data(number_bfs = "', number_bfs, '", language = "fr")')
          )
        }
      } else {
        code_lines <- c(
          code_lines,
          paste0('data <- bfs_get_data(number_bfs = "', number_bfs, '", language = "fr")')
        )
      }
    }
    
    code_text <- paste(code_lines, collapse = "\n")
    
    # Utiliser JavaScript pour copier dans le presse-papiers
    session$sendCustomMessage("copyToClipboard", code_text)
    showNotification("Code copié dans le presse-papiers!", type = "message")
  })
  
  # Vérifier si des données sont chargées pour l'onglet Visualisation
  output$has_queried_data <- reactive({
    !is.null(queried_data()) && nrow(queried_data()) > 0
  })
  outputOptions(output, "has_queried_data", suspendWhenHidden = FALSE)
  
  # Générer des visualisations avec l'AI
  observeEvent(input$generate_visualizations_btn, {
    # Vérifier que des données sont disponibles
    if (is.null(queried_data()) || nrow(queried_data()) == 0) {
      showNotification("Veuillez d'abord charger des données dans l'onglet 'Données'", type = "warning")
      return()
    }
    
    # Vérifier les identifiants
    if (is.null(input$infomaniak_api_token) || trimws(input$infomaniak_api_token) == "" ||
        is.null(input$infomaniak_product_id) || trimws(input$infomaniak_product_id) == "") {
      showNotification("Veuillez configurer votre API Token et Product ID Infomaniak", type = "warning")
      return()
    }
    
    showNotification("Génération des visualisations en cours...", type = "message")
    ai_visualizations(NULL)  # Réinitialiser
    
    # Appeler l'API Infomaniak
    result <- call_infomaniak_ai(
      data = queried_data(),
      api_token = input$infomaniak_api_token,
      product_id = input$infomaniak_product_id
    )
    
    if (!result$success) {
      showNotification(paste("Erreur:", result$error), type = "error")
      return()
    }
    
    # Exécuter le code généré
    exec_result <- execute_visualization_code(result$code, queried_data())
    
    if (!exec_result$success) {
      showNotification(paste("Erreur lors de l'exécution:", exec_result$error), type = "error")
      return()
    }
    
    # Stocker les visualisations
    ai_visualizations(exec_result$plots)
    showNotification(paste("Visualisations générées avec succès! (", length(exec_result$plots), " graphiques)"), type = "message")
  })
  
  # Observer pour créer les outputs dynamiques des visualisations
  observe({
    plots <- ai_visualizations()
    if (!is.null(plots) && length(plots) > 0) {
      # Créer un output pour chaque visualisation
      for (i in seq_along(plots)) {
        output_id <- paste0("ai_plot_", i)
        local({
          my_i <- i
          my_plot <- plots[[my_i]]
          output[[output_id]] <- plotly::renderPlotly({
            my_plot
          })
        })
      }
    }
  })
  
  # Afficher les visualisations
  output$ai_visualizations <- renderUI({
    req(ai_visualizations())
    
    plots <- ai_visualizations()
    if (is.null(plots) || length(plots) == 0) {
      return(tags$p("Aucune visualisation disponible"))
    }
    
    # Créer un UI pour chaque visualisation
    plot_outputs <- lapply(seq_along(plots), function(i) {
      plot_name <- names(plots)[i]
      if (is.null(plot_name) || plot_name == "") {
        plot_name <- paste0("Visualisation ", i)
      }
      
      output_id <- paste0("ai_plot_", i)
      
      tags$div(
        class = "well",
        style = "margin-bottom: 20px;",
        tags$h4(plot_name),
        plotly::plotlyOutput(output_id, height = "500px")
      )
    })
    
    do.call(tagList, plot_outputs)
  })
}
