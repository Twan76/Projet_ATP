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
    navbarPage(title = "", theme = shinytheme("flatly"),
      tabPanel(title ="Cartographie", icon = icon("house"),
               sidebarPanel(
                 sliderTextInput("map_date",
                                 label = h5("Répatition des tournois à travers le temps :"),
                                 choices = format(unique(tournament_atp_final_df$Debut), "%d %b %y"),
                                 selected = format(current_date, "%d %b %y"),
                                 grid = FALSE,
                                 animate=animationOptions(interval = 1500, loop = FALSE)
                 ),
                 checkboxGroupInput("categorie", "Categorie :", inline=F, choices = c("ATP 250","ATP 500","Masters 1000","Grand Chelem"), selected = sort(unique(tournament_atp_final_df$Categorie))),
                 checkboxGroupInput("surface", "Surface :", inline=F, choices = sort(unique(tournament_atp_final_df$Surface)), selected = sort(unique(tournament_atp_final_df$Surface)))
               ),
               mainPanel(
                 mod_mapping_tournois_ui("mapping_tournois_1")
               )
      ),
      tabPanel(title ="Evolution", icon = icon("chart-area"),
      ),
      tabPanel(title ="Données", icon = icon("table"),
               mod_afficher_table_ui("afficher_table_1")
      ),
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
