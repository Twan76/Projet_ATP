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

source(file = "barchart_race.R")

tournament_atp_final_df <- readr::read_rds("tournament_atp_final.rds")
tournament_atp_final_df$Debut<- as.Date(tournament_atp_final_df$Debut,origin="1899-12-30")
tournament_atp_final_df$Fin <- as.Date(tournament_atp_final_df$Fin,origin="1899-12-30")

tournament_details_final_df <- readr::read_rds("tournament_details_final.rds")
tournament_details_final_df$Debut<- as.Date(tournament_details_final_df$Debut,origin="1899-12-30")
tournament_details_final_df$Fin <- as.Date(tournament_details_final_df$Fin,origin="1899-12-30")

ranking_atp_2022_df <- readr::read_rds("ranking_atp_2022.rds")
ranking_atp_2022_df$Date<- as.Date(ranking_atp_2022_df$Date,origin="1899-12-30")

ranking_atp_mois_2001_df <- readr::read_rds("ranking_atp_mois_2001.rds")
ranking_atp_mois_2001_df$Date<- as.Date(ranking_atp_mois_2001_df$Date,origin="1899-12-30")

player_stats_df <- readr::read_rds("player_stats.rds")

ranking_atp_num1_df <- readr::read_rds("ranking_atp_num1.rds")

current_date <- as.Date(max(tournament_atp_final_df$Debut),"%Y-%m-%d")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bootstrapPage(
      navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">ATP 2022</a>'), id="nav",
                 windowTitle = "ATP 2022",
                 tabPanel(title ="Cartographie", icon = icon("house"),
                          div(class="outer",
                              mod_mapping_tournois_ui("mapping_tournois_1"),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 75, right = 20, fixed=TRUE,
                                            draggable = TRUE, height = "auto", width = "500px",
                                            br(),
                                            pickerInput("categorie", label = "Cat??gorie :", width="450px",
                                                        choices = list(`Circuit challenger` = c("Challenger 50", "Challenger 80", "Challenger 90", "Challenger 100", "Challenger 125"),
                                                                       `Circuit principal` = c("ATP 250", "ATP 500", "Masters 1000", "Grand Chelem")),
                                                        selected = c("ATP 250", "ATP 500", "Masters 1000", "Grand Chelem"),
                                                        multiple = TRUE,
                                                        options = list(`actions-box` = TRUE,
                                                                       `deselect-all-text` = "Tout d??s??lectionner",
                                                                       `select-all-text` = "Tout s??lectionner",
                                                                       `none-selected-text` = "Aucune cat??gorie choisie")
                                            ),
                                            pickerInput("surface", label = "Surface :", width="450px",
                                                        choices = sort(unique(tournament_atp_final_df$Surface)),
                                                        selected = sort(unique(tournament_atp_final_df$Surface)),
                                                        multiple = TRUE,
                                                        options = list(`actions-box` = TRUE,
                                                                       `deselect-all-text` = "Tout d??s??lectionner",
                                                                       `select-all-text` = "Tout s??lectionner",
                                                                       `none-selected-text` = "Aucune surface choisie")
                                            ),
                                            sliderTextInput("map_date", width="450px",
                                                            label = h5("D??filer le temps :"),
                                                            choices = format(sort(unique(tournament_atp_final_df$Debut)), "%d %b %y"),
                                                            selected = format(current_date, "%d %b %y"),
                                                            grid = FALSE,
                                                            animate=animationOptions(interval = 1000, loop = FALSE)),

                                            mod_graphique_evolution_tournois_ui("graphique_evolution_tournois_1")
                              ),
                              absolutePanel(id = "logo", class = "card", bottom = 60, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://github.com/Twan76/Projet_ATP/', tags$i(class = "fa fa-github", style = "font-size:40px; color: black;"))
                              ),
                              absolutePanel(id = "logo", class = "card", bottom = 60, left = 80, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://www.atptour.com/', tags$img(src='www/atp.png', height='40', width='35'))
                              )
                          )
                 ),
                 tabPanel(title = "Timeline", icon = icon("calendar"),
                          fluidRow(
                            column(5,
                                   pickerInput("timeline_categorie", label = "Cat??gorie :", width="375px",
                                               choices = list(`Circuit secondaire` = c("Challenger 50", "Challenger 80", "Challenger 90", "Challenger 100", "Challenger 125"),
                                                              `Circuit principal` = c("ATP 250", "ATP 500", "Masters 1000", "Grand Chelem"),
                                                              "Autre"),
                                               selected = unique(tournament_details_final_df$Categorie),
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE,
                                                              `deselect-all-text` = "Tout d??s??lectionner",
                                                              `select-all-text` = "Tout s??lectionner",
                                                              `none-selected-text` = "Aucune cat??gorie choisie")
                                   )
                            ),
                            column(5,
                                   pickerInput("timeline_surface", label = "Surface :", width="375px",
                                               choices = sort(unique(tournament_details_final_df$Surface)),
                                               selected = sort(unique(tournament_details_final_df$Surface)),
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE,
                                                              `deselect-all-text` = "Tout d??s??lectionner",
                                                              `select-all-text` = "Tout s??lectionner",
                                                              `none-selected-text` = "Aucune surface choisie")
                                   )
                            )
                          ),
                          mod_timeline_tournois_ui("timeline_tournois_1")
                 ),
                 tabPanel(title = "Similarit??s", icon = icon("person"),
                          column(4,
                                 pickerInput('Joueur', label = p("Les 3 joueurs ayant le style de jeu ressemblant le plus ?? :", style = "font-size:20px"),
                                             choices = sort(unique(player_stats_df$Joueur)),
                                             selected = "Rafael Nadal",
                                             multiple = FALSE,
                                             inline = TRUE,
                                             options = list(`live-search` = TRUE)
                                 ),
                                 hr(), h2(""),
                                 p("La similarit?? entre les joueurs est d??termin??e en utilisant une technique de data mining,
                                        appel??e la", a("m??thode des plus proches voisins", href="https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm", target="_blank"), ", ?? partir des statistiques suivantes.",style = "font-size:20px;"),
                                 hr(), h2(""),
                                 p("D??finition des abbr??viatitons :",style = "font-size:20px; font-weight: bold;"),
                                 p("PctSW : % de services gagn??s",style = "font-size:15px;"),
                                 p("Pct1S : % de premi??re balle au service",style = "font-size:15px;"),
                                 p("Pct1SW : % de points gagn??s sur premi??re balle",style = "font-size:15px;"),
                                 p("Pct2SW : % de points gagn??s sur seconde balle",style = "font-size:15px;"),
                                 p("PctBPS : % de balles de breaks sauv??es",style = "font-size:15px;"),
                                 p("P1SR : % de points gagn??s sur premi??re balle adverse",style = "font-size:15px;"),
                                 p("P2SR : % de points gagn??s sur deuxi??me balle advserse",style = "font-size:15px;")
                          ),
                          column(8,
                                 mod_radar_ui("radar_1")
                          )
                 ),
                 tabPanel(title ="Evolution", icon = icon("chart-area"),
                          br(),
                          h4("Choix type de classement"),
                          radioButtons("type_classement", label = "", inline = T, choices = list("Classement 2022" = 0, "Classement depuis 2001" = 1), selected = 0),
                          br(),
                          conditionalPanel(condition = "input.type_classement == 0",
                                           fluidRow(
                                             column(5,
                                                    pickerInput("country_ranking_2022", label = "Choix pays", width="375px",
                                                                choices = list("Tous les pays", Pays = sort(unique(ranking_atp_2022_df$Pays))),
                                                                selected = "Tous les pays",
                                                                multiple = FALSE,
                                                                options = list(`live-search` = TRUE)
                                                    )
                                             ),
                                             column(5,
                                                    pickerInput("top_n_2022", label = "# Bars", width="375px",
                                                                choices = c(5,10,15,20,25),
                                                                selected = 10,
                                                                multiple = FALSE,
                                                                options = list(`actions-box` = TRUE,
                                                                               `none-selected-text` = "Aucune s??lection choisie")
                                                    )
                                             )
                                           ),
                                           mod_barchart_race_ranking_2022_ui("barchart_race_ranking_2022_1")
                          ),
                          conditionalPanel(condition = "input.type_classement == 1",
                                           fluidRow(
                                             column(5,
                                                    pickerInput("country_ranking_2001", label = "Choix pays", width="375px",
                                                                choices = list("Tous les pays", Pays = sort(unique(ranking_atp_mois_2001_df$Pays))),
                                                                selected = "Tous les pays",
                                                                multiple = FALSE,
                                                                options = list(`live-search` = TRUE)
                                                    )
                                             ),
                                             column(5,
                                                    pickerInput("top_n_2001", label = "# Bars", width="375px",
                                                                choices = c(5,10,15,20,25),
                                                                selected = 10,
                                                                multiple = FALSE,
                                                                options = list(`actions-box` = TRUE,
                                                                               `none-selected-text` = "Aucune s??lection choisie")
                                                    )
                                             )
                                           ),
                                           mod_barchart_race_ranking_2001_ui("barchart_race_ranking_2001_1")
                          )
                 ),
                 tabPanel(title ="Donn??es", icon = icon("table"),
                          br(),
                          h4("Type de donn??es"),
                          radioButtons("details_donnees", label = "", inline = T, choices = list("Tournois jou??s" = 0, "Classement ATP" = 1, "Num??ros 1 mondiaux" = 2), selected = 0),
                          br(),
                          conditionalPanel(condition = "input.details_donnees == 0",
                                           mod_afficher_table_ui("afficher_table_1")
                          ),
                          conditionalPanel(condition = "input.details_donnees == 1",
                                           mod_afficher_table_ranking_ui("afficher_table_ranking_1")
                          ),
                          conditionalPanel(condition = "input.details_donnees == 2",
                                           mod_afficher_table_num1_ui("afficher_table_num1_1")
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
