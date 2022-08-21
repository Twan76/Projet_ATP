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

# flagIcon <- makeIcon(
#   iconUrl = case_when(
#     tournament_atp_final_df$Vainqueur == "Rafael Nadal" ~ "inst/app/www/players/nadal.png",
#     tournament_atp_final_df$Vainqueur == "Aslan Karatsev" ~ "inst/app/www/players/karatsev.png",
#     tournament_atp_final_df$Vainqueur == "Thanasi Kokkinakis" ~ "inst/app/www/players/kokkinakis.png",
#     tournament_atp_final_df$Vainqueur == "Joao Sousa" ~ "inst/app/www/players/sousa.png",
#     tournament_atp_final_df$Vainqueur == "Gael Monfils" ~ "inst/app/www/players/monfils.png",
#     tournament_atp_final_df$Vainqueur == "Albert Ramos-Vinolas" ~ "inst/app/www/players/ramos-vinolas.png",
#     tournament_atp_final_df$Vainqueur == "Alexander Bublik" ~ "inst/app/www/players/bublik.png",
#     tournament_atp_final_df$Vainqueur == "Carlos Alcaraz" ~ "inst/app/www/players/alcaraz.png",
#     tournament_atp_final_df$Vainqueur == "Felix Auger-Aliassime" ~ "inst/app/www/players/auger-aliassime.png",
#     tournament_atp_final_df$Vainqueur == "Sebastian Baez" ~ "inst/app/www/players/baez.png",
#     tournament_atp_final_df$Vainqueur == "Roberto Bautista Agut" ~ "inst/app/www/players/bautista-agut.png",
#     tournament_atp_final_df$Vainqueur == "Matteo Berrettini" ~ "inst/app/www/players/berrettini.png",
#     tournament_atp_final_df$Vainqueur == "Francisco Cerundolo" ~ "inst/app/www/players/cerundolo.png",
#     tournament_atp_final_df$Vainqueur == "Maxime Cressy" ~ "inst/app/www/players/cressy.png",
#     tournament_atp_final_df$Vainqueur == "Alex de Minaur" ~ "inst/app/www/players/de_minaur.png",
#     tournament_atp_final_df$Vainqueur == "Novak Djokovic" ~ "inst/app/www/players/djokovic.png",
#     tournament_atp_final_df$Vainqueur == "Taylor Fritz" ~ "inst/app/www/players/fritz.png",
#     tournament_atp_final_df$Vainqueur == "David Goffin" ~ "inst/app/www/players/goffin.png",
#     tournament_atp_final_df$Vainqueur == "Hubert Hurkacz" ~ "inst/app/www/players/hurkacz.png",
#     tournament_atp_final_df$Vainqueur == "Pedro Martinez" ~ "inst/app/www/players/martinez.png",
#     tournament_atp_final_df$Vainqueur == "Lorenzo Musetti" ~ "inst/app/www/players/musetti.png",
#     tournament_atp_final_df$Vainqueur == "Cameron Norrie" ~ "inst/app/www/players/norrie.png",
#     tournament_atp_final_df$Vainqueur == "Reilly Opelka" ~ "inst/app/www/players/opelka.png",
#     tournament_atp_final_df$Vainqueur == "Andrey Rublev" ~ "inst/app/www/players/rublev.png",
#     tournament_atp_final_df$Vainqueur == "Holger Rune" ~ "inst/app/www/players/rune.png",
#     tournament_atp_final_df$Vainqueur == "Casper Ruud" ~ "inst/app/www/players/ruud.png",
#     tournament_atp_final_df$Vainqueur == "Jannik Sinner" ~ "inst/app/www/players/sinner.png",
#     tournament_atp_final_df$Vainqueur == "Stefanos Tsitsipas" ~ "inst/app/www/players/tsitsipas.png",
#     tournament_atp_final_df$Vainqueur == "Tim Van Rijhtoven" ~ "inst/app/www/players/van_rijthoven.png"
#   ),
#   iconWidth = 35, iconHeight = 35,
#   shadowWidth = 10, shadowHeight = 10
# )

flagIcon <- makeIcon(
  iconUrl = case_when(
    tournament_atp_final_df$Categorie == "ATP 250" ~ "inst/app/www/category/categorystamps_250.png",
    tournament_atp_final_df$Categorie == "ATP 500" ~ "inst/app/www/category/categorystamps_500.png",
    tournament_atp_final_df$Categorie == "Masters 1000" ~ "inst/app/www/category/categorystamps_1000.png",
    tournament_atp_final_df$Categorie == "Grand Chelem" ~ "inst/app/www/category/categorystamps_grandslam.png",
  ),
  iconWidth = 40, iconHeight = 25,
  shadowWidth = 10, shadowHeight = 10
)

# flagIcon <- makeIcon(
#   iconUrl = case_when(
#     tournament_atp_final_df$Tournoi == "Abierto Mexicano Telcel presentado por HSBC" ~ "inst/app/www/tournaments/acapulco.png",
#     tournament_atp_final_df$Tournoi == "ABN AMRO Open" ~ "inst/app/www/tournaments/rotterdam.png",
#     tournament_atp_final_df$Tournoi == "Adelaide International 1" ~ "inst/app/www/tournaments/adelaide.png",
#     tournament_atp_final_df$Tournoi == "Adelaide International 2" ~ "inst/app/www/tournaments/adelaide.png",
#     tournament_atp_final_df$Tournoi == "Argentina Open" ~ "inst/app/www/tournaments/buenos_aires.png",
#     tournament_atp_final_df$Tournoi == "Atlanta Open" ~ "inst/app/www/tournaments/atlanta.png",
#     tournament_atp_final_df$Tournoi == "Australian Open" ~ "inst/app/www/tournaments/australian_open.png",
#     tournament_atp_final_df$Tournoi == "Barcelona Open Banc Sabadell" ~ "inst/app/www/tournaments/barcelone.png",
#     tournament_atp_final_df$Tournoi == "BMW Open by American Express" ~ "inst/app/www/tournaments/munich.png",
#     tournament_atp_final_df$Tournoi == "BNP Paribas Open" ~ "inst/app/www/tournaments/indian_wells.png",
#     tournament_atp_final_df$Tournoi == "BOSS OPEN" ~ "inst/app/www/tournaments/stuttgart.png",
#     tournament_atp_final_df$Tournoi == "Chile Dove Men+Care Open" ~ "inst/app/www/tournaments/santiago.png",
#     tournament_atp_final_df$Tournoi == "Cinch Championships" ~ "inst/app/www/tournaments/london.png",
#     tournament_atp_final_df$Tournoi == "Cordoba Open" ~ "inst/app/www/tournaments/cordoba.png",
#     tournament_atp_final_df$Tournoi == "Dallas Open" ~ "inst/app/www/tournaments/dallas.png",
#     tournament_atp_final_df$Tournoi == "Delray Beach Open by VITACOST.com" ~ "inst/app/www/tournaments/delray_beach.png",
#     tournament_atp_final_df$Tournoi == "Dubai Duty Free Tennis Championships" ~ "inst/app/www/tournaments/dubai.png",
#     tournament_atp_final_df$Tournoi == "EFG Swiss Open Gstaad" ~ "inst/app/www/tournaments/gstaad.png",
#     tournament_atp_final_df$Tournoi == "Fayez Sarofim & Co. U.S. Men's Clay Court Championship" ~ "inst/app/www/tournaments/houston.png",
#     tournament_atp_final_df$Tournoi == "Generali Open" ~ "inst/app/www/tournaments/kitzbuhel.png",
#     tournament_atp_final_df$Tournoi == "Gonet Geneva Open" ~ "inst/app/www/tournaments/geneve.png",
#     tournament_atp_final_df$Tournoi == "Grand Prix Hassan II" ~ "inst/app/www/tournaments/marrakech.png",
#     tournament_atp_final_df$Tournoi == "Hamburg European Open" ~ "inst/app/www/tournaments/hamburg.png",
#     tournament_atp_final_df$Tournoi == "Infosys Hall of Fame Open" ~ "inst/app/www/tournaments/newport.png",
#     tournament_atp_final_df$Tournoi == "Internazionali BNL d'Italia" ~ "inst/app/www/tournaments/rome.png",
#     tournament_atp_final_df$Tournoi == "Libema Open" ~ "inst/app/www/tournaments/'s_hertogenbosch.png",
#     tournament_atp_final_df$Tournoi == "Mallorca Championships" ~ "inst/app/www/tournaments/mallorca.png",
#     tournament_atp_final_df$Tournoi == "Melbourne Summer Set" ~ "inst/app/www/tournaments/melbourne.png",
#     tournament_atp_final_df$Tournoi == "Miami Open presented by Itau" ~ "inst/app/www/tournaments/miami.png",
#     tournament_atp_final_df$Tournoi == "Millennium Estoril Open" ~ "inst/app/www/tournaments/estoril.png",
#     tournament_atp_final_df$Tournoi == "Mutua Madrid Open" ~ "inst/app/www/tournaments/madrid.png",
#     tournament_atp_final_df$Tournoi == "Nordea Open" ~ "inst/app/www/tournaments/bastad.png",
#     tournament_atp_final_df$Tournoi == "Open 13 Provence" ~ "inst/app/www/tournaments/marseille.png",
#     tournament_atp_final_df$Tournoi == "Open Parc Auvergne-Rhone-Alpes Lyon" ~ "inst/app/www/tournaments/lyon.png",
#     tournament_atp_final_df$Tournoi == "Open Sud de France – Montpellier" ~ "inst/app/www/tournaments/montpellier.png",
#     tournament_atp_final_df$Tournoi == "Plava Laguna Croatia Open Umag" ~ "inst/app/www/tournaments/umag.png",
#     tournament_atp_final_df$Tournoi == "Qatar ExxonMobil Open" ~ "inst/app/www/tournaments/doha.png",
#     tournament_atp_final_df$Tournoi == "Rio Open presented by Claro" ~ "inst/app/www/tournaments/rio.png",
#     tournament_atp_final_df$Tournoi == "Roland Garros" ~ "inst/app/www/tournaments/roland_garros.png",
#     tournament_atp_final_df$Tournoi == "Rolex Monte-Carlo Masters" ~ "inst/app/www/tournaments/monte_carlo.png",
#     tournament_atp_final_df$Tournoi == "Rothesay International" ~ "inst/app/www/tournaments/eastbourne.png",
#     tournament_atp_final_df$Tournoi == "Serbia Open" ~ "inst/app/www/tournaments/belgrade.png",
#     tournament_atp_final_df$Tournoi == "Sydney Tennis Classic" ~ "inst/app/www/tournaments/sydney.png",
#     tournament_atp_final_df$Tournoi == "Tata Open Maharashtra" ~ "inst/app/www/tournaments/pune.png",
#     tournament_atp_final_df$Tournoi == "Terra Wortmann Open" ~ "inst/app/www/tournaments/halle.png",
#     tournament_atp_final_df$Tournoi == "Wimbledon" ~ "inst/app/www/tournaments/wimbledon.png"
#   ),
#   iconWidth = 45, iconHeight = 30,
# )

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
