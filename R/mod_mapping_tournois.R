#' mapping_tournois UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mapping_tournois_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = "900px")
  )
}

#' mapping_tournois Server Functions
#'
#' @noRd
mod_mapping_tournois_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$map <- renderLeaflet({
      leaflet(tournament_atp_final_df) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addMarkers(~lon, ~lat,
                   icon = flagIcon,
                   label = ~label_text,
                   labelOptions = labelOptions(textsize = "12px"),
                   popup = ~popup_text)
    })
  })
}

## To be copied in the UI
# mod_mapping_tournois_ui("mapping_tournois_1")

## To be copied in the server
# mod_mapping_tournois_server("mapping_tournois_1")
