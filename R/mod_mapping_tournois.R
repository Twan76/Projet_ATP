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
    leafletOutput(ns("map"), height="700px")
  )
}

#' mapping_tournois Server Functions
#'
#' @noRd
mod_mapping_tournois_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$map <- renderLeaflet({
      leaflet(r_global$dataset()) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addMarkers(~lon, ~lat,
                   icon = makeIcon(iconUrl = ~icon_categorie,iconWidth = 40, iconHeight = 25, shadowWidth = 10, shadowHeight = 10),
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
