#' afficher_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DT

mod_afficher_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("tournois_details"))
  )
}

#' afficher_table Server Functions
#'
#' @noRd
mod_afficher_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tournois_details <- renderDT({
      datatable(tournament_atp_final_df[,c(1:6,9,7,19,12)],filter="top", class = 'cell-border stripe', rownames = F) %>%
        formatDate(c(4,5), method = 'toLocaleDateString', params = list('fr-FR'))
    })

  })
}

## To be copied in the UI
# mod_afficher_table_ui("afficher_table_1")

## To be copied in the server
# mod_afficher_table_server("afficher_table_1")
