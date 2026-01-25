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
        
        # Sélecteur de catalogues (checkboxes pour activer/désactiver)
        tags$div(
          tags$label("Catalogues de recherche", class = "control-label"),
          tags$p("Activez les catalogues dans lesquels vous souhaitez rechercher:", style = "font-size: 0.9em; color: #666; margin-bottom: 10px;"),
          checkboxGroupInput(
            "active_catalogs",
            label = NULL,
            choices = list(
              "BFS Catalog (PXWeb)" = "catalog",
              "Swiss Stats Explorer" = "sse",
              "Opendata.swiss" = "opendata",
              "geocat.ch" = "geocat"
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
        
        # Champ de recherche unifié
        textInput(
          "search_term",
          label = "Terme de recherche",
          placeholder = "Ex: étudiants, population, logement..."
        ),
        tags$small("La recherche sera effectuée dans tous les catalogues activés ci-dessus."),
        
        # Champ pour numéro BFS direct (SSE uniquement)
        tags$div(
          style = "margin-top: 15px;",
          h5("Ou numéro BFS direct (SSE uniquement)"),
          textInput(
            "sse_number_bfs",
            label = "Numéro BFS (SSE)",
            placeholder = "Ex: DF_LWZ_1"
          ),
          tags$small("Entrez le numéro BFS du dataset SSE pour charger directement ses métadonnées (ex: DF_LWZ_1)")
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
            uiOutput("query_button_ui"),
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
          ),
          
          # Onglet 5: Visualisation
          tabPanel(
            "Visualisation",
            br(),
            tags$div(
              class = "alert alert-warning",
              tags$h5("Configuration API Infomaniak"),
              tags$p("Pour générer des visualisations automatiques, vous devez configurer vos identifiants API Infomaniak."),
              tags$p(
                tags$strong("Comment obtenir vos identifiants :"),
                tags$ol(
                  tags$li("Créez un token API sur ", tags$a(href = "https://manager.infomaniak.com/v3/ng/profile/user/token/list", target = "_blank", "Infomaniak Manager")),
                  tags$li("Sélectionnez le produit AI approprié"),
                  tags$li("Copiez votre API Token"),
                  tags$li("Notez votre Product ID")
                )
              )
            ),
            br(),
            tags$div(
              class = "well",
              tags$h4("Identifiants API Infomaniak"),
              passwordInput(
                "infomaniak_api_token",
                label = "API Token",
                placeholder = "Votre API Token Infomaniak",
                width = "100%"
              ),
              textInput(
                "infomaniak_product_id",
                label = "Product ID",
                placeholder = "Votre Product ID",
                width = "100%"
              ),
              tags$small(
                style = "color: #666;",
                "Ces informations sont stockées localement dans votre session et ne sont pas transmises ailleurs."
              )
            ),
            br(),
            conditionalPanel(
              condition = "output.has_queried_data",
              tags$div(
                tags$h4("Générer des visualisations"),
                tags$p("L'IA analysera vos données et proposera plusieurs visualisations avec plotly."),
                actionButton(
                  "generate_visualizations_btn",
                  "🎨 Générer des visualisations",
                  class = "btn-success",
                  style = "width: 100%; margin-bottom: 15px;"
                ),
                br(),
                shinycssloaders::withSpinner(
                  uiOutput("ai_visualizations"),
                  type = 4,
                  color = "#0dc5c1"
                )
              )
            ),
            conditionalPanel(
              condition = "!output.has_queried_data",
              tags$div(
                class = "alert alert-info",
                tags$p("Veuillez d'abord charger des données dans l'onglet 'Données' avant de générer des visualisations.")
              )
            )
          )
        )
      )
    )
  )
}
