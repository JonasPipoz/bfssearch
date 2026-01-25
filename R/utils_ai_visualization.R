#' Fonctions pour générer des visualisations avec l'API Infomaniak GPT-OSS
#' 
#' @noRd

#' Appeler l'API Infomaniak GPT-OSS pour générer du code de visualisation
#' 
#' @param data Les données à visualiser (data.frame)
#' @param api_token API Token Infomaniak
#' @param product_id Product ID pour GPT-OSS
#' @return Liste contenant le code R généré et les informations de visualisation
call_infomaniak_ai <- function(data, api_token, product_id) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(success = FALSE, error = "Aucune donnée disponible"))
  }
  
  # Vérifier les identifiants
  if (is.null(api_token) || trimws(api_token) == "" ||
      is.null(product_id) || trimws(product_id) == "") {
    return(list(success = FALSE, error = "API Token et Product ID sont requis"))
  }
  
  # Préparer un résumé des données pour le prompt
  # Identifier les colonnes catégorielles (character, factor) et numériques
  column_types <- sapply(data, class)
  categorical_cols <- names(column_types)[column_types %in% c("character", "factor")]
  numeric_cols <- names(column_types)[column_types %in% c("numeric", "integer", "double")]
  
  # Compter les valeurs uniques pour chaque colonne catégorielle
  unique_counts <- sapply(data[categorical_cols], function(x) length(unique(x)), simplify = FALSE)
  
  # Détecter si les données sont en format long (plusieurs lignes par catégorie)
  is_long_format <- length(categorical_cols) > 0 && any(sapply(unique_counts, function(x) x > 1 && x < nrow(data) * 0.8))
  
  data_summary <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    column_names = colnames(data),
    column_types = column_types,
    categorical_cols = categorical_cols,
    numeric_cols = numeric_cols,
    unique_counts = unique_counts,
    is_long_format = is_long_format,
    sample_data = utils::head(data, 10)  # Plus d'exemples pour mieux comprendre la structure
  )
  
  # Créer le prompt pour l'AI
  prompt <- paste0(
    "Tu es un expert en visualisation de données statistiques avec R, dplyr et plotly. ",
    "Analyse les données suivantes et génère du code R pour créer plusieurs visualisations pertinentes.\n\n",
    "IMPORTANT - Type de données:\n",
    "Ces données sont des STATISTIQUES AGRÉGÉES (données officielles agrégées, pas des données individuelles). ",
    "Chaque ligne représente déjà une valeur agrégée (moyenne, total, pourcentage, etc.). ",
    "ÉVITE absolument les visualisations de distribution comme les histogrammes ou boxplots qui n'ont pas de sens pour des données agrégées.\n\n",
    "Résumé des données:\n",
    "- Nombre de lignes: ", data_summary$n_rows, "\n",
    "- Nombre de colonnes: ", data_summary$n_cols, "\n",
    "- Noms des colonnes: ", paste(data_summary$column_names, collapse = ", "), "\n",
    "- Types des colonnes: ", paste(paste(names(data_summary$column_types), data_summary$column_types, sep = "="), collapse = ", "), "\n"
  )
  
  # Ajouter des informations sur les colonnes catégorielles
  if (length(data_summary$categorical_cols) > 0) {
    prompt <- paste0(
      prompt,
      "- Colonnes catégorielles: ", paste(data_summary$categorical_cols, collapse = ", "), "\n"
    )
    for (cat_col in data_summary$categorical_cols) {
      unique_vals <- unique(data[[cat_col]])
      if (length(unique_vals) <= 20) {
        prompt <- paste0(
          prompt,
          "  * ", cat_col, " (", length(unique_vals), " valeurs uniques): ", 
          paste(head(unique_vals, 10), collapse = ", "), 
          ifelse(length(unique_vals) > 10, "...", ""), "\n"
        )
      } else {
        prompt <- paste0(
          prompt,
          "  * ", cat_col, " (", length(unique_vals), " valeurs uniques, exemples: ", 
          paste(head(unique_vals, 5), collapse = ", "), "...)\n"
        )
      }
    }
  }
  
  if (length(data_summary$numeric_cols) > 0) {
    prompt <- paste0(
      prompt,
      "- Colonnes numériques: ", paste(data_summary$numeric_cols, collapse = ", "), "\n"
    )
  }
  
  prompt <- paste0(
    prompt,
    "\n",
    "Exemple de données (10 premières lignes):\n",
    paste(capture.output(print(data_summary$sample_data)), collapse = "\n"), "\n\n",
    "IMPORTANT - Format des données:\n",
    ifelse(data_summary$is_long_format,
      "Les données sont en FORMAT LONG. Cela signifie qu'il y a plusieurs lignes pour chaque combinaison de catégories. ",
      "Les données peuvent être en format long ou large. "
    ),
    "Tu dois analyser la structure et créer des visualisations pertinentes.\n\n",
    "Instructions pour générer le code R:\n",
    "1. Commence par 'library(plotly)' et 'library(dplyr)'\n",
    "2. Les données sont disponibles dans une variable appelée 'data'\n",
    "3. Crée 3 à 5 visualisations pertinentes selon les données disponibles\n",
    "4. Si les données contiennent des colonnes catégorielles avec plusieurs valeurs:\n",
    "   - FILTRE les données par différentes catégories pour créer des visualisations variées\n",
    "   - Utilise dplyr::filter() pour sélectionner des sous-ensembles intéressants\n",
    "   - Par exemple: data_filtered <- data %>% dplyr::filter(categorie == 'valeur1')\n",
    "   - Crée une visualisation avec chaque filtre pour montrer différents aspects\n",
    "5. Types de visualisations à PRIVILÉGIER (données agrégées):\n",
    "   - Graphiques en barres (type='bar') pour comparer des valeurs agrégées entre catégories\n",
    "   - Graphiques en lignes (type='scatter', mode='lines') pour les séries temporelles et tendances\n",
    "   - Graphiques en aires (type='scatter', mode='lines', fill='tozeroy') pour les séries temporelles avec remplissage\n",
    "   - Graphiques en donut (type='pie' avec hole) pour les proportions et parts\n",
    "   - Nuages de points (type='scatter', mode='markers') pour les relations entre variables numériques agrégées\n",
    "   - Graphiques combinés (barres + lignes) pour comparer plusieurs métriques\n",
    "   ÉVITER: histogrammes, boxplots, violin plots (réservés aux données individuelles, pas aux agrégats)\n",
    "6. Chaque visualisation doit être stockée dans 'plot1', 'plot2', 'plot3', etc.\n",
    "7. Utilise plotly::plot_ly() pour créer les graphiques\n",
    "8. Ajoute des titres descriptifs avec layout(title = '...') en français\n",
    "9. Utilise des couleurs différentes (color = ~categorie) pour distinguer les catégories\n",
    "10. À la fin, crée: plots <- list(plot1 = plot1, plot2 = plot2, ...)\n\n",
    "Exemple de code avec filtrage (données agrégées):\n",
    "library(plotly)\n",
    "library(dplyr)\n",
    "# Visualisation 1: Graphique en barres filtré par catégorie\n",
    "data_cat1 <- data %>% dplyr::filter(categorie == unique(data$categorie)[1])\n",
    "plot1 <- plotly::plot_ly(data_cat1, x = ~variable_x, y = ~valeur_agregee, type = 'bar') %>%\n",
    "  plotly::layout(title = 'Valeurs agrégées pour catégorie 1', yaxis = list(title = 'Valeur'))\n",
    "# Visualisation 2: Graphique en lignes pour série temporelle\n",
    "data_cat2 <- data %>% dplyr::filter(categorie == unique(data$categorie)[2])\n",
    "plot2 <- plotly::plot_ly(data_cat2, x = ~annee, y = ~valeur_agregee, type = 'scatter', mode = 'lines+markers') %>%\n",
    "  plotly::layout(title = 'Évolution temporelle pour catégorie 2', xaxis = list(title = 'Année'), yaxis = list(title = 'Valeur'))\n",
    "# Visualisation 3: Graphique en donut pour proportions\n",
    "plot3 <- plotly::plot_ly(data, labels = ~categorie, values = ~valeur_agregee, type = 'pie', hole = 0.4) %>%\n",
    "  plotly::layout(title = 'Répartition par catégorie')\n",
    "# Visualisation 4: Graphique en aires pour tendances\n",
    "plot4 <- plotly::plot_ly(data, x = ~annee, y = ~valeur_agregee, type = 'scatter', mode = 'lines', fill = 'tozeroy', color = ~categorie) %>%\n",
    "  plotly::layout(title = 'Évolution par catégorie', xaxis = list(title = 'Année'), yaxis = list(title = 'Valeur'))\n",
    "plots <- list(plot1 = plot1, plot2 = plot2, plot3 = plot3, plot4 = plot4)\n\n",
    "RÈGLES STRICTES:\n",
    "- Retourne UNIQUEMENT le code R, sans explications, sans markdown, sans ```r ou ```\n",
    "- Le code doit être directement exécutable\n",
    "- Utilise plotly::plot_ly() et dplyr::filter() avec le préfixe du package\n",
    "- Gère les erreurs: vérifie que les colonnes existent avant de les utiliser\n",
    "- Si une colonne n'existe pas, utilise une autre colonne disponible\n",
    "- Adapte le nombre de visualisations au nombre de catégories disponibles\n",
    "- Varie les types de graphiques (barres, lignes, donuts, aires, scatter) pour montrer différents aspects\n",
    "- N'UTILISE JAMAIS: histogrammes (type='histogram'), boxplots (type='box'), violin plots\n",
    "- Les données sont déjà agrégées: chaque ligne = une valeur statistique, pas une observation individuelle"
  )
  
  # Appeler l'API Infomaniak (endpoint v1 comme dans le projet Python)
  api_url <- paste0("https://api.infomaniak.com/2/ai/", product_id, "/openai/v1/chat/completions")
  
  tryCatch({
    response <- httr::POST(
      api_url,
      httr::add_headers(
        "Authorization" = paste0("Bearer ", trimws(api_token)),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = "openai/gpt-oss-120b",
        messages = list(
          list(
            role = "system",
            content = "Tu es un expert en visualisation de données statistiques agrégées avec R, dplyr et plotly. Tu génères uniquement du code R valide et exécutable. Les données sont des statistiques officielles agrégées (chaque ligne = valeur agrégée). Tu utilises dplyr pour filtrer les données en format long. Tu privilégies les graphiques en barres, lignes, donuts, aires et scatter. Tu évites absolument les histogrammes et boxplots qui ne sont pas adaptés aux données agrégées."
          ),
          list(
            role = "user",
            content = prompt
          )
        ),
        temperature = 0.7,
        stream = FALSE
      ), auto_unbox = TRUE),
      httr::timeout(60)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "parsed")
      code_text <- content$choices[[1]]$message$content
      
      # Nettoyer le code (enlever les markdown code blocks si présents)
      code_text <- gsub("^```r\\n|^```R\\n|^```\\n", "", code_text)
      code_text <- gsub("\\n```$|```$", "", code_text)
      code_text <- trimws(code_text)
      
      return(list(success = TRUE, code = code_text))
    } else {
      error_content <- httr::content(response, as = "text")
      return(list(
        success = FALSE,
        error = paste0("Erreur API (", httr::status_code(response), "): ", error_content)
      ))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = paste0("Erreur lors de l'appel API: ", e$message)))
  })
}

#' Exécuter le code R généré et retourner les visualisations
#' 
#' @param code Code R généré par l'AI
#' @param data Les données à utiliser
#' @return Liste des objets plotly ou NULL en cas d'erreur
execute_visualization_code <- function(code, data) {
  if (is.null(code) || trimws(code) == "") {
    return(list(success = FALSE, error = "Code vide"))
  }
  
  # Vérifier que plotly est installé
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(list(success = FALSE, error = "Le package plotly n'est pas installé. Installez-le avec: install.packages('plotly')"))
  }
  
  # Vérifier que dplyr est disponible
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    return(list(success = FALSE, error = "Le package dplyr n'est pas installé. Installez-le avec: install.packages('dplyr')"))
  }
  
  # Créer un environnement isolé pour exécuter le code
  # Utiliser l'environnement global comme parent pour que library() fonctionne
  env <- new.env(parent = .GlobalEnv)
  env$data <- data
  
  # Ajouter les fonctions plotly à l'environnement pour être sûr qu'elles sont disponibles
  env$plot_ly <- plotly::plot_ly
  env$subplot <- plotly::subplot
  
  # Ajouter les fonctions dplyr pour le filtrage
  env$filter <- dplyr::filter
  env$select <- dplyr::select
  env$mutate <- dplyr::mutate
  env$group_by <- dplyr::group_by
  env$summarise <- dplyr::summarise
  env$summarize <- dplyr::summarize
  env$`%>%` <- magrittr::`%>%`
  
  # Surcharger layout pour qu'il fonctionne avec le pipe
  env$layout <- function(p, ...) {
    if (inherits(p, "plotly")) {
      plotly::layout(p, ...)
    } else {
      plotly::layout(...)
    }
  }
  
  tryCatch({
    # Exécuter le code
    # Le code peut contenir library(plotly) qui chargera plotly dans l'environnement global
    # mais les fonctions seront aussi disponibles via notre env
    eval(parse(text = code), envir = env)
    
    # Récupérer les plots
    if (exists("plots", envir = env)) {
      plots_list <- get("plots", envir = env)
      # Vérifier que c'est une liste et qu'elle contient des objets plotly
      if (is.list(plots_list) && length(plots_list) > 0) {
        # Filtrer les NULL
        plots_list <- plots_list[!sapply(plots_list, is.null)]
        if (length(plots_list) > 0) {
          return(list(success = TRUE, plots = plots_list))
        }
      }
    }
    
    # Essayer de trouver les plots individuellement (plot1, plot2, etc.)
    plot_names <- ls(envir = env, pattern = "^plot[0-9]+$")
    if (length(plot_names) > 0) {
      plots_list <- mget(plot_names, envir = env)
      # Filtrer les NULL
      plots_list <- plots_list[!sapply(plots_list, is.null)]
      if (length(plots_list) > 0) {
        names(plots_list) <- plot_names[!sapply(plots_list, is.null)]
        return(list(success = TRUE, plots = plots_list))
      }
    }
    
    return(list(success = FALSE, error = "Aucune visualisation valide trouvée dans le code généré. Le code doit créer des objets plotly nommés plot1, plot2, etc., ou une liste 'plots'."))
  }, error = function(e) {
    return(list(success = FALSE, error = paste0("Erreur lors de l'exécution du code: ", e$message, "\nLigne: ", e$call)))
  })
}
