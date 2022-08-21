#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import readr
#' @import leaflet
#' @import shinymaterial
#' @noRd

tournament_atp_final_df <- read_rds("tournament_atp_final.rds")

# flagIcon <- makeIcon(
#   iconUrl = case_when(
#     tournament_atp_final_dfcountry == "argentina" ~ "inst/app/www/flags/argentina.png",
#     tournament_atp_final_dfcountry == "australia" ~ "inst/app/www/flags/australia.png",
#     tournament_atp_final_dfcountry == "chile" ~ "inst/app/www/flags/chile.png",
#     tournament_atp_final_dfcountry == "croatia" ~ "inst/app/www/flags/croatia.png",
#     tournament_atp_final_dfcountry == "france" ~ "inst/app/www/flags/france.png",
#     tournament_atp_final_dfcountry == "germany" ~ "inst/app/www/flags/germany.png",
#     tournament_atp_final_dfcountry == "india" ~ "inst/app/www/flags/india.png",
#     tournament_atp_final_dfcountry == "morocco" ~ "inst/app/www/flags/morocco.png",
#     tournament_atp_final_dfcountry == "netherlands" ~ "inst/app/www/flags/netherlands.png",
#     tournament_atp_final_dfcountry == "portugal" ~ "inst/app/www/flags/portugal.png",
#     tournament_atp_final_dfcountry == "qatar" ~ "inst/app/www/flags/qatar.png",
#     tournament_atp_final_dfcountry == "serbia" ~ "inst/app/www/flags/serbia.png",
#     tournament_atp_final_dfcountry == "spain" ~ "inst/app/www/flags/spain.png",
#     tournament_atp_final_dfcountry == "sweden" ~ "inst/app/www/flags/sweden.png",
#     tournament_atp_final_dfcountry == "switzerland" ~ "inst/app/www/flags/switzerland.png",
#     tournament_atp_final_dfcountry == "uk" ~ "inst/app/www/flags/uk.png",
#     tournament_atp_final_dfcountry == "usa" ~ "inst/app/www/flags/usa.png"
#   ),
#   iconWidth = 25, iconHeight = 25,
#   shadowWidth = 10, shadowHeight = 10
# )

flagIcon <- makeIcon(
  iconUrl = case_when(
    tournament_atp_final_df$Vainqueur == "Rafael Nadal" ~ "inst/app/www/players/nadal.png",
    tournament_atp_final_df$Vainqueur == "Aslan Karatsev" ~ "inst/app/www/players/karatsev.png",
    tournament_atp_final_df$Vainqueur == "Thanasi Kokkinakis" ~ "inst/app/www/players/kokkinakis.png",
    tournament_atp_final_df$Vainqueur == "Joao Sousa" ~ "inst/app/www/players/sousa.png",
    tournament_atp_final_df$Vainqueur == "Gael Monfils" ~ "inst/app/www/players/monfils.png",
    tournament_atp_final_df$Vainqueur == "Albert Ramos-Vinolas" ~ "inst/app/www/players/ramos-vinolas.png",
    tournament_atp_final_df$Vainqueur == "Alexander Bublik" ~ "inst/app/www/players/bublik.png",
    tournament_atp_final_df$Vainqueur == "Carlos Alcaraz" ~ "inst/app/www/players/alcaraz.png",
    tournament_atp_final_df$Vainqueur == "Felix Auger-Aliassime" ~ "inst/app/www/players/auger-aliassime.png",
    tournament_atp_final_df$Vainqueur == "Sebastian Baez" ~ "inst/app/www/players/baez.png",
    tournament_atp_final_df$Vainqueur == "Roberto Bautista Agut" ~ "inst/app/www/players/bautista-agut.png",
    tournament_atp_final_df$Vainqueur == "Matteo Berrettini" ~ "inst/app/www/players/berrettini.png",
    tournament_atp_final_df$Vainqueur == "Francisco Cerundolo" ~ "inst/app/www/players/cerundolo.png",
    tournament_atp_final_df$Vainqueur == "Maxime Cressy" ~ "inst/app/www/players/cressy.png",
    tournament_atp_final_df$Vainqueur == "Alex de Minaur" ~ "inst/app/www/players/de_minaur.png",
    tournament_atp_final_df$Vainqueur == "Novak Djokovic" ~ "inst/app/www/players/djokovic.png",
    tournament_atp_final_df$Vainqueur == "Taylor Fritz" ~ "inst/app/www/players/fritz.png",
    tournament_atp_final_df$Vainqueur == "David Goffin" ~ "inst/app/www/players/goffin.png",
    tournament_atp_final_df$Vainqueur == "Hubert Hurkacz" ~ "inst/app/www/players/hurkacz.png",
    tournament_atp_final_df$Vainqueur == "Pedro Martinez" ~ "inst/app/www/players/martinez.png",
    tournament_atp_final_df$Vainqueur == "Lorenzo Musetti" ~ "inst/app/www/players/musetti.png",
    tournament_atp_final_df$Vainqueur == "Cameron Norrie" ~ "inst/app/www/players/norrie.png",
    tournament_atp_final_df$Vainqueur == "Reilly Opelka" ~ "inst/app/www/players/opelka.png",
    tournament_atp_final_df$Vainqueur == "Andrey Rublev" ~ "inst/app/www/players/rublev.png",
    tournament_atp_final_df$Vainqueur == "Holger Rune" ~ "inst/app/www/players/rune.png",
    tournament_atp_final_df$Vainqueur == "Casper Ruud" ~ "inst/app/www/players/ruud.png",
    tournament_atp_final_df$Vainqueur == "Jannik Sinner" ~ "inst/app/www/players/sinner.png",
    tournament_atp_final_df$Vainqueur == "Stefanos Tsitsipas" ~ "inst/app/www/players/tsitsipas.png",
    tournament_atp_final_df$Vainqueur == "Tim Van Rijhtoven" ~ "inst/app/www/players/van_rijthoven.png"
  ),
  iconWidth = 35, iconHeight = 35,
  shadowWidth = 10, shadowHeight = 10
)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    material_page(
      title = "",
      include_nav_bar = FALSE,
      ##-- Sidebar
      material_side_nav(
        fixed = TRUE,
        image_source = "www/atp.png",
        material_side_nav_tabs(
          side_nav_tabs = c(
            "Cartographie" = "carto",
            "Evolutions" = "evol"
          ),
          icons = c("ac_unit", "build")
        )
      ),
      material_side_nav_tab_content(
        side_nav_tab_id = "carto",
        mod_mapping_tournois_ui("mapping_tournois_1")
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
      app_title = "Projet"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
