#' @title Interface utilisateur de l'application
#' @description UI principale de l'application BFS Search
#' 
#' @noRd

app_ui <- function(request) {
  fluidPage(
    # JavaScript et CSS
    get_js_scripts(),
    get_sse_modal(),
    get_modal_css(),
    
    titlePanel("Moteur de recherche BFS - Office fédéral de la statistique"),
    
    # Sidebar pour la recherche
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Recherche de données"),
        
        # Sélecteur de type d'API
        tags$div(
          tags$label("Type d'API", class = "control-label"),
          radioButtons(
            "api_type",
            label = NULL,
            choices = list(
              "BFS Catalog (PXWeb)" = "catalog",
              "Swiss Stats Explorer" = "sse"
            ),
            selected = "catalog"
          ),
          # Icône info pour SSE
          tags$div(
            style = "margin-top: -10px; margin-left: 20px;",
            actionLink("sse_info_btn", 
                      tags$span(icon("info-circle"), " Comment trouver un dataset SSE?"),
                      style = "font-size: 0.9em; color: #337ab7; text-decoration: none;")
          )
        ),
        
        hr(),
        
        # Champ de recherche (pour BFS Catalog)
        conditionalPanel(
          condition = "input.api_type == 'catalog'",
          textInput(
            "search_term",
            label = "Terme de recherche",
            placeholder = "Ex: étudiants, population, logement..."
          )
        ),
        
        # Champ pour number_bfs (pour SSE)
        conditionalPanel(
          condition = "input.api_type == 'sse'",
          textInput(
            "sse_number_bfs",
            label = "Numéro BFS (SSE)",
            placeholder = "Ex: DF_LWZ_1"
          ),
          tags$small("Entrez le numéro BFS du dataset SSE (ex: DF_LWZ_1)")
        ),
        
        # Bouton de recherche
        actionButton("search_btn", "Rechercher", class = "btn-primary", width = "100%"),
        
        br(), br(),
        
        # Options de recherche avancée
        h5("Options de recherche"),
        
        selectInput(
          "spatial_division",
          label = "Division spatiale",
          choices = c(
            "Toutes" = "",
            "Suisse" = "Switzerland",
            "Cantons" = "Cantons",
            "Districts" = "Districts",
            "Communes" = "Communes",
            "Autres divisions spatiales" = "Other spatial divisions",
            "International" = "International"
          ),
          selected = ""
        ),
        
        numericInput(
          "limit",
          label = "Nombre maximum de résultats",
          value = 50,
          min = 1,
          max = 350
        ),
        
        hr(),
        
        # Informations
        h5("Instructions"),
        p("1. Entrez un terme de recherche"),
        p("2. Sélectionnez un dataset dans les résultats"),
        p("3. Configurez les filtres dynamiques"),
        p("4. Téléchargez les données")
      ),
      
      # Panneau principal
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "main_tabs",
          
          # Onglet 1: Résultats de recherche
          tabPanel(
            "Résultats de recherche",
            br(),
            verbatimTextOutput("search_status"),
            br(),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("catalog_table"),
              type = 4,
              color = "#0dc5c1"
            ),
            br(),
            # Debug: afficher les colonnes disponibles
            conditionalPanel(
              condition = "output.catalog_table",
              verbatimTextOutput("debug_info")
            )
          ),
          
          # Onglet 2: Configuration des filtres
          tabPanel(
            "Configuration des filtres",
            br(),
            uiOutput("dataset_info"),
            br(),
            uiOutput("dynamic_filters"),
            br(),
            actionButton("query_btn", "Interroger les données", class = "btn-success", width = "100%"),
            br(), br(),
            verbatimTextOutput("query_status")
          ),
          
          # Onglet 3: Résultats des données
          tabPanel(
            "Données",
            br(),
            downloadButton("download_data", "Télécharger CSV", class = "btn-info"),
            br(), br(),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("data_table"),
              type = 4,
              color = "#0dc5c1"
            ),
            br(),
            verbatimTextOutput("data_info")
          ),
          
          # Onglet 4: Code R
          tabPanel(
            "Code R",
            br(),
            tags$div(
              class = "alert alert-info",
              tags$h5("Code R généré"),
              tags$p("Copiez et collez ce code dans votre script R pour charger les données directement.")
            ),
            br(),
            tags$div(
              style = "position: relative; margin-bottom: 10px;",
              actionButton("copy_code_btn", "📋 Copier le code", class = "btn-primary", 
                          style = "position: absolute; top: 5px; right: 5px; z-index: 1000;"),
              tags$pre(
                id = "r_code_output",
                style = "background-color: #f5f5f5; padding: 20px 100px 20px 20px; border: 1px solid #ddd; border-radius: 4px; overflow-x: auto; font-family: 'Courier New', monospace; font-size: 12px; min-height: 200px;",
                verbatimTextOutput("r_code", placeholder = TRUE)
              )
            ),
            br(),
            tags$div(
              class = "well",
              tags$h5("Instructions"),
              tags$ol(
                tags$li("Assurez-vous d'avoir installé le package BFS : ", tags$code("install.packages('BFS')")),
                tags$li("Copiez le code ci-dessus"),
                tags$li("Collez-le dans votre script R"),
                tags$li("Exécutez le code pour charger les données")
              )
            )
          )
        )
      )
    )
  )
}
