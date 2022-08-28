#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @import shinyWidgets
#' @import shinythemes
#' @import readr
#' @noRd

tournament_atp_final_df <- read_rds("tournament_atp_final.rds")
tournament_atp_final_df$Debut<- as.Date(tournament_atp_final_df$Debut,origin="1899-12-30")
tournament_atp_final_df$Fin <- as.Date(tournament_atp_final_df$Fin,origin="1899-12-30")

current_date <- as.Date(max(tournament_atp_final_df$Debut),"%Y-%m-%d")

ranking_atp_2022_df <- read_rds("ranking_atp_2022.rds")
ranking_atp_2022_df$Date<- as.Date(ranking_atp_2022_df$Date,origin="1899-12-30")

ranking_atp_annee_df <- read_rds("ranking_atp_annee.rds")
ranking_atp_annee_df$Date<- as.Date(ranking_atp_annee_df$Date,origin="1899-12-30")


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bootstrapPage(
      navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">ATP World Tour</a>'), id="nav",
                 windowTitle = "ATP World Tour",
                 tabPanel(title ="Cartographie", icon = icon("house"),
                          div(class="outer",
                              mod_mapping_tournois_ui("mapping_tournois_1"),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 75, right = 20, fixed=TRUE,
                                            draggable = TRUE, height = "auto", width = "400px",
                                            pickerInput("categorie", label = "Catégorie :", width="375px",
                                                        choices = list(`Circuit secondaire` = c("Challenger 50", "Challenger 80", "Challenger 90", "Challenger 100", "Challenger 125"),
                                                                       `Circuit principal` = c("ATP 250", "ATP 500", "Masters 1000", "Grand Chelem")),
                                                        selected = c("ATP 250", "ATP 500", "Masters 1000", "Grand Chelem"),
                                                        multiple = TRUE,
                                                        options = list(`actions-box` = TRUE,
                                                                       `deselect-all-text` = "Tout désélectionner",
                                                                       `select-all-text` = "Tout sélectionner",
                                                                       `none-selected-text` = "Aucune catégorie choisie")
                                            ),
                                            pickerInput("surface", label = "Surface :", width="375px",
                                                        choices = sort(unique(tournament_atp_final_df$Surface)),
                                                        selected = sort(unique(tournament_atp_final_df$Surface)),
                                                        multiple = TRUE,
                                                        options = list(`actions-box` = TRUE,
                                                                       `deselect-all-text` = "Tout désélectionner",
                                                                       `select-all-text` = "Tout sélectionner",
                                                                       `none-selected-text` = "Aucune surface choisie")
                                            ),
                                            sliderTextInput("map_date",
                                                            label = h5("Défiler le temps :"),
                                                            choices = format(sort(unique(tournament_atp_final_df$Debut)), "%d %b %y"),
                                                            selected = format(current_date, "%d %b %y"),
                                                            grid = FALSE,
                                                            animate=animationOptions(interval = 1000, loop = FALSE)),
                                            mod_graphique_evolution_tournois_ui("graphique_evolution_tournois_1")
                              ),
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://github.com/Twan76/Projet_ATP/', tags$i(class = "fa fa-github", style = "font-size:40px; color: black;"))
                              ),
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 80, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://www.atptour.com/', tags$img(src='www/atp.png', height='40', width='35'))
                              )
                          )
                 ),
                 # tabPanel(title ="Evolution", icon = icon("chart-area"),
                 #          mod_graphique_evolution_tournois_ui("graphique_evolution_tournois_1")
                 # )
                 tabPanel(title ="Données", icon = icon("table"),
                          br(),
                          h4("Type de données"),
                          radioButtons("details_donnees", label = "", inline = T, choices = list("Tournois joués" = 0, "Classement ATP" = 1), selected = 0),
                          br(),
                          conditionalPanel(condition = "input.details_donnees == 0",
                                           mod_afficher_table_ui("afficher_table_1")
                          ),
                          conditionalPanel(condition = "input.details_donnees == 1",
                                           mod_afficher_table_ranking_ui("afficher_table_ranking_1")
                          )
                 )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Projet Tennis"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
