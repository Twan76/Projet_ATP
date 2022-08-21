#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # # Your application server logic
  formatted_date <- reactive({
    format(as.Date(input$map_date, format="%d %b %y"), "%Y-%m-%d")
  })

  r_global <- reactiveValues()
  r_global$dataset <- reactive({
    # Attribue TRUE si l'option sélectionnée correspond, FALSE sinon
    match_date <- (tournament_atp_final_df$Debut <= formatted_date())
    match_categorie <- (match(tournament_atp_final_df$Categorie,input$categorie, nomatch=FALSE) > 0)
    match_surface <- (match(tournament_atp_final_df$Surface,input$surface, nomatch=FALSE) > 0)
    intersec_option <- (match_date == TRUE & match_categorie == TRUE & match_surface == TRUE)
    tournament_atp_final_df[intersec_option,]
  })

  # Your application server logic
  mod_mapping_tournois_server("mapping_tournois_1", r_global = r_global)
  mod_afficher_table_server("afficher_table_1", r_global = r_global)

}
