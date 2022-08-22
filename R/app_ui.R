#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import readr
#' @import leaflet
#' @import shinyWidgets
#' @import shinythemes
#' @noRd

tournament_atp_final_df <- read_rds("tournament_atp_final.rds")
tournament_atp_final_df$Debut<- as.Date(tournament_atp_final_df$Debut,origin="1899-12-30")
tournament_atp_final_df$Fin <- as.Date(tournament_atp_final_df$Fin,origin="1899-12-30")

current_date <- as.Date(max(tournament_atp_final_df$Debut),"%Y-%m-%d")

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
                              tags$head(includeCSS("styles.css")),
                              mod_mapping_tournois_ui("mapping_tournois_1"),
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 0, right = 0, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            pickerInput("categorie", label = "Sélectionner une catégorie",
                                                        choices = list(`Circuit secondaire` = c("Challenger 50", "Challenger 80", "Challenger 90", "Challenger 100", "Challenger 125"),
                                                                       `Circuit principal` = c("ATP 250", "ATP 500", "Masters 1000", "Grand Chelem")),
                                                        selected = unique(tournament_atp_final_df$Categorie),
                                                        multiple = TRUE,
                                                        options = list(`actions-box` = TRUE,
                                                                       `deselect-all-text` = "Tout désélectionner",
                                                                       `select-all-text` = "Tout sélectionner",
                                                                       `none-selected-text` = "Aucune catégorie choisie")
                                            ),
                                            checkboxGroupInput("surface", "Surface :", inline=F, choices = sort(unique(tournament_atp_final_df$Surface)), selected = sort(unique(tournament_atp_final_df$Surface)))
                              )
                          )
                 ),
                 tabPanel(title ="Evolution", icon = icon("chart-area"),
                 )
                 # tabPanel(title ="Données", icon = icon("table"),
                 #          mod_afficher_table_ui("afficher_table_1")
                 # )
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
