#' Modal pour les instructions SSE
#' 
#' @noRd
get_sse_modal <- function() {
  tags$div(
    id = "sse_modal",
    class = "modal fade",
    tabindex = "-1",
    role = "dialog",
    tags$div(
      class = "modal-dialog",
      role = "document",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h4(class = "modal-title", "Comment trouver un dataset Swiss Stats Explorer"),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Fermer",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          tags$h5("Méthode 1 : Recherche par mot-clé"),
          tags$p(
            "Vous pouvez rechercher un dataset en utilisant un mot-clé dans le champ de recherche.",
            tags$br(),
            tags$strong("Langues acceptées :"),
            " La recherche fonctionne avec des mots-clés en ",
            tags$strong("anglais"), " ou ", tags$strong("allemand"), 
            " (les noms des datasets sont principalement en anglais).",
            tags$br(),
            tags$small(
              tags$strong("Exemples :"), 
              tags$code("housing"), ", ", tags$code("population"), 
              ", ", tags$code("education"), ", ", tags$code("Wohnung"), 
              ", ", tags$code("Bevölkerung"), " etc."
            )
          ),
          tags$hr(),
          tags$h5("Méthode 2 : Recherche par numéro BFS"),
          tags$ol(
            tags$li(
              tags$strong("Se rendre à l'adresse :"),
              tags$a(href = "https://stats.swiss/?lc=fr", target = "_blank", "https://stats.swiss/?lc=fr")
            ),
            tags$li("Rechercher un dataset"),
            tags$li(
              "Dans la page du dataset, cliquer sur ",
              tags$code("\"Libellés > Identifiant\""),
              " et copier l'identifiant du dataset pour l'entrer dans l'application",
              tags$br(),
              tags$small("Exemple: ", tags$code("DF_GWS_REG6"), style = "color: #666;")
            )
          ),
          tags$div(
            class = "alert alert-info",
            style = "margin-top: 15px;",
            tags$p(
              tags$strong("Note:"),
              " L'identifiant du dataset commence généralement par ",
              tags$code("DF_"),
              " ou ",
              tags$code("px-"),
              " et peut être trouvé dans l'URL ou dans les métadonnées du dataset."
            )
          )
        ),
        tags$div(
          class = "modal-footer",
          tags$button(
            type = "button",
            class = "btn btn-default",
            `data-dismiss` = "modal",
            "Fermer"
          )
        )
      )
    )
  )
}

#' CSS pour le modal
#' 
#' @noRd
get_modal_css <- function() {
  tags$style("
    .modal-header {
      background-color: #f5f5f5;
      border-bottom: 1px solid #ddd;
    }
    .modal-body ol {
      padding-left: 20px;
    }
    .modal-body li {
      margin-bottom: 10px;
    }
    .modal-body code {
      background-color: #f4f4f4;
      padding: 2px 6px;
      border-radius: 3px;
      font-size: 0.9em;
    }
    /* Styles pour les tooltips du tableau */
    #catalog_table tbody tr[title] {
      cursor: help;
    }
    #catalog_table tbody tr[title]:hover {
      background-color: #f0f8ff;
    }
  ")
}
