#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {

  # # Your application server logic
  formatted_date <- reactive({
    format(as.Date(input$map_date, format="%d %b %y"), "%Y-%m-%d")
  })

  r_global <- reactiveValues()

  r_global$dataset <- reactive({
      filter(tournament_atp_final_df, Categorie %in% input$categorie & Surface %in% input$surface & Debut <= formatted_date())
  })

  # Your application server logic
  mod_mapping_tournois_server("mapping_tournois_1", r_global = r_global)
  mod_graphique_evolution_tournois_server("graphique_evolution_tournois_1", r_global = r_global)
  mod_afficher_table_server("afficher_table_1")
  mod_afficher_table_ranking_server("afficher_table_ranking_1")

}
