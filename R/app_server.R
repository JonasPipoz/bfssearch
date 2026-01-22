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
  
  # Variables réactives
  catalog_results <- reactiveVal(NULL)
  selected_dataset <- reactiveVal(NULL)
  dataset_metadata <- reactiveVal(NULL)
  metadata_tidy <- reactiveVal(NULL)
  sse_codelist <- reactiveVal(NULL)  # Pour SSE
  query_dimensions <- reactiveVal(NULL)
  queried_data <- reactiveVal(NULL)
  api_type <- reactiveVal("catalog")  # "catalog" ou "sse"
  
  # Observer pour le type d'API
  observeEvent(input$api_type, {
    api_type(input$api_type)
    # Réinitialiser les résultats lors du changement de type
    catalog_results(NULL)
    selected_dataset(NULL)
    dataset_metadata(NULL)
    metadata_tidy(NULL)
    sse_codelist(NULL)
    queried_data(NULL)
  })
  
  # Observer pour ouvrir le modal d'information SSE
  observeEvent(input$sse_info_btn, {
    # Ouvrir le modal Bootstrap via JavaScript
    session$sendCustomMessage("showSSEModal", list())
  })
  
  # Recherche dans le catalogue (BFS Catalog)
  observeEvent(input$search_btn, {
    if (input$api_type == "catalog") {
      req(input$search_term)
      
      showNotification("Recherche en cours...", type = "message")
      
      tryCatch({
        # Recherche dans le catalogue BFS
        results <- BFS::bfs_get_catalog_data(
          language = "fr",
          extended_search = input$search_term,
          spatial_division = if(input$spatial_division == "") NULL else input$spatial_division,
          limit = input$limit
        )
        
        if (nrow(results) == 0) {
          showNotification("Aucun résultat trouvé. Essayez un autre terme de recherche.", type = "warning")
          catalog_results(NULL)
        } else {
          catalog_results(results)
          showNotification(paste(nrow(results), "dataset(s) trouvé(s)"), type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Erreur lors de la recherche:", e$message), type = "error")
        catalog_results(NULL)
      })
    } else if (input$api_type == "sse") {
      # Pour SSE, vérifier si recherche ou numéro direct
      if (!is.null(input$sse_search_term) && input$sse_search_term != "") {
        # Recherche dans le catalogue SSE
        showNotification("Recherche dans le catalogue SSE...", type = "message")
        
        tryCatch({
          # Utiliser la fonction de recherche SSE
          results <- search_swiss_stats(keyword = input$sse_search_term, language = "fr")
          
          if (nrow(results) == 0) {
            showNotification("Aucun résultat trouvé. Essayez un autre terme de recherche.", type = "warning")
            catalog_results(NULL)
          } else {
            # Adapter les résultats au format attendu
            results_formatted <- results |>
              dplyr::mutate(
                title = name,
                number_bfs = id,
                publication_date = Sys.Date(),
                language_available = "fr"
              ) |>
              dplyr::select(title, number_bfs, publication_date, language_available, id, agencyID, version)
            
            catalog_results(results_formatted)
            showNotification(paste(nrow(results), "dataset(s) SSE trouvé(s)"), type = "message")
          }
        }, error = function(e) {
          showNotification(paste("Erreur lors de la recherche SSE:", e$message), type = "error")
          catalog_results(NULL)
        })
      } else if (!is.null(input$sse_number_bfs) && input$sse_number_bfs != "") {
        # Charger directement les métadonnées avec le numéro BFS
        showNotification("Chargement des métadonnées SSE...", type = "message")
        
        tryCatch({
          # Charger le codelist SSE
          codelist <- BFS::bfs_get_sse_metadata(
            number_bfs = input$sse_number_bfs,
            language = "fr"
          )
          
          sse_codelist(codelist)
          
          # Créer un dataset factice pour la compatibilité
          fake_dataset <- data.frame(
            title = paste0("Dataset SSE: ", input$sse_number_bfs),
            number_bfs = input$sse_number_bfs,
            publication_date = Sys.Date(),
            language_available = "fr"
          )
          selected_dataset(fake_dataset)
          
          # Passer directement à l'onglet de configuration
          updateTabsetPanel(session, "main_tabs", selected = "Configuration des filtres")
          
          showNotification("Métadonnées SSE chargées avec succès", type = "message")
        }, error = function(e) {
          showNotification(paste("Erreur lors du chargement SSE:", e$message), type = "error")
          sse_codelist(NULL)
        })
      } else {
        showNotification("Veuillez entrer un terme de recherche ou un numéro BFS", type = "warning")
      }
    }
  })
  
  # Affichage du tableau de résultats
  output$catalog_table <- DT::renderDataTable({
    req(catalog_results())
    
    # Préparer les données pour l'affichage
    results_df <- catalog_results()
    
    # Vérifier quelles colonnes sont disponibles
    available_cols <- colnames(results_df)
    
    # Colonnes préférées à afficher (dans l'ordre)
    # Pour SSE, on a aussi id, agencyID, version
    preferred_cols <- c("title", "number_bfs", "id", "publication_date", "language_available", "language", "number_asset", "agencyID", "version")
    
    # Sélectionner les colonnes disponibles
    cols_to_show <- preferred_cols[preferred_cols %in% available_cols]
    
    # Si aucune colonne préférée n'est trouvée, utiliser toutes les colonnes disponibles
    if (length(cols_to_show) == 0) {
      cols_to_show <- available_cols
    }
    
    # Sélectionner les colonnes (utiliser any_of pour éviter les erreurs si colonnes manquantes)
    display_data <- results_df |> 
      dplyr::select(dplyr::any_of(cols_to_show))
    
    # Renommer les colonnes pour un affichage plus lisible
    new_names <- colnames(display_data)
    new_names <- gsub("_", " ", new_names)
    new_names <- tools::toTitleCase(new_names)
    colnames(display_data) <- new_names
    
    # Créer le tableau DataTable
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
        )
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
      
      # Charger les métadonnées selon le type d'API
      if (input$api_type == "sse") {
        # Pour SSE, charger directement le codelist
        load_sse_metadata(selected_row$number_bfs)
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
  })
  
  # Génération dynamique des filtres
  output$dynamic_filters <- renderUI({
    # Vérifier le type d'API
    if (input$api_type == "sse") {
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
      if (input$api_type == "sse") {
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
    
    # Construire le code R selon le type d'API
    if (input$api_type == "sse") {
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
    
    # Générer le code selon le type d'API
    if (input$api_type == "sse") {
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
}
