#' afficher_table_num1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_afficher_table_num1_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("num1_details"))
  )
}

#' afficher_table_num1 Server Functions
#'
#' @noRd
mod_afficher_table_num1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$num1_details <- renderDT({
      datatable(ranking_atp_num1_df ,filter="top", class = 'cell-border stripe', rownames = F)
    })

  })
}

## To be copied in the UI
# mod_afficher_table_num1_ui("afficher_table_num1_1")

## To be copied in the server
# mod_afficher_table_num1_server("afficher_table_num1_1")
