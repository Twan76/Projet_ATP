#' afficher_table_ranking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_afficher_table_ranking_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("ranking_details"))
  )
}

#' afficher_table_ranking Server Functions
#'
#' @noRd
mod_afficher_table_ranking_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ranking_details <- renderDT({

      dataset <- dplyr::filter(ranking_atp_2022_df, Date == max(Date))

      datatable(dataset[,1:7],filter="top", class = 'cell-border stripe', rownames = F)  %>%
        formatStyle(names(dataset)[2], fontWeight = 'bold',
                    color = styleInterval(0, c('red', 'green')))
    })

  })
}

## To be copied in the UI
# mod_afficher_table_ranking_ui("afficher_table_ranking_1")

## To be copied in the server
# mod_afficher_table_ranking_server("afficher_table_ranking_1")
