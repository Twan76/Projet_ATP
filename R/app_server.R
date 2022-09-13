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
      dplyr::filter(tournament_atp_final_df, Categorie %in% input$categorie & Surface %in% input$surface & Debut <= formatted_date())
  })

  r_global$dataset_timeline <- reactive({
    dplyr::filter(tournament_details_final_df, Categorie %in% input$timeline_categorie & Surface %in% input$timeline_surface)
  })

  r_global$dataset_ranking_2022 <- reactive({
      if (input$country_ranking_2022 == "Tous les pays") {
        ranking_atp_2022_df
      } else {
        dplyr::filter(ranking_atp_2022_df, Pays == input$country_ranking_2022)
      }
  })

  r_global$dataset_ranking_2001 <- reactive({
    if (input$country_ranking_2001 == "Tous les pays") {
      ranking_atp_mois_2001_df
    } else {
      dplyr::filter(ranking_atp_mois_2001_df, Pays == input$country_ranking_2001)
    }
  })

  r_global$top_n_2022 <- reactive({
    as.integer(input$top_n_2022)
  })

  r_global$top_n_2001 <- reactive({
    as.integer(input$top_n_2001)
  })

  r_global$player_selected_radar <- reactive({
     input$Player
  })

  r_global$dataset1 <- reactive({
    player_stats_df %>%
    filter(player_stats_df$Joueur != input$Joueur)
  })

  r_global$dataset2 <- reactive({
    player_stats_df %>%
      filter(player_stats_df$Joueur == input$Joueur)
  })

  # Your application server logic
  mod_mapping_tournois_server("mapping_tournois_1", r_global = r_global)
  mod_graphique_evolution_tournois_server("graphique_evolution_tournois_1", r_global = r_global)
  mod_timeline_tournois_server("timeline_tournois_1", r_global = r_global)
  mod_barchart_race_ranking_2022_server("barchart_race_ranking_2022_1",  r_global = r_global)
  mod_barchart_race_ranking_2001_server("barchart_race_ranking_2001_1", r_global = r_global)
  mod_radar_server("radar_1", r_global = r_global)
  mod_afficher_table_server("afficher_table_1")
  mod_afficher_table_ranking_server("afficher_table_ranking_1")
  mod_afficher_table_num1_server("afficher_table_num1_1")

  # Google charts avec indexplayer, points et age, faire liaison couleur ? continent?
  # https://shiny.rstudio.com/gallery/scotpho-profiles.html, trend en rajoutant des joueurs (evolution classement)
}
