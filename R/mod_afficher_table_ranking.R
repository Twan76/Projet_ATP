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

      dataset <- subset(ranking_atp_2022_df, Date == max(Date))
      dataset[,1] <- as.integer(dataset[,1])
      dataset[,2] <- as.integer(dataset[,2])
      dataset[,5] <- as.integer(dataset[,5])
      dataset[,6] <- as.integer(dataset[,6])
      datatable(dataset[,1:6],filter="top", class = 'cell-border stripe', rownames = F)  %>%
        formatStyle(names(dataset)[2], fontWeight = 'bold',
                    color = styleInterval(0, c('red', 'green')))
    })

  })
}

## To be copied in the UI
# mod_afficher_table_ranking_ui("afficher_table_ranking_1")

## To be copied in the server
# mod_afficher_table_ranking_server("afficher_table_ranking_1")
