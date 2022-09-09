#' radar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import plotly
#' @import FNN

mod_radar_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("radar"), width = 800, height=700),
    p("Pour visualiser le graphique d'un joueur, cliquer sur le nom du joueur dans la légende du graphique",
      style = "font-size:25px")
  )
}

#' radar Server Functions
#'
#' @noRd
mod_radar_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$radar <- renderPlotly({

      dataset <- rbind(r_global$dataset2(),r_global$dataset1())

      similarity <- as.numeric(knnx.index(dataset[,2:8], dataset[1,2:8], k=4))

      dataset_final <- dataset[similarity,]

      plot_ly(
        type = 'scatterpolar',
        mode = "closest",
        fill = 'toself'
      ) %>%
        add_trace(
          r = as.matrix(dataset_final[1,2:8]),
          theta = c("PctSW","Pct1S","Pct1SW","Pct2SW","PctBPS","P1SR","P2SR"),
          showlegend = TRUE,
          mode = "markers",
          name = dataset_final[1,1]
        ) %>%
        add_trace(
          r = as.matrix(dataset_final[2,2:8]),
          theta = c("PctSW","Pct1S","Pct1SW","Pct2SW","PctBPS","P1SR","P2SR"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = dataset_final[2,1]
        ) %>%
        add_trace(
          r = as.matrix(dataset_final[3,2:8]),
          theta = c("PctSW","Pct1S","Pct1SW","Pct2SW","PctBPS","P1SR","P2SR"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = dataset_final[3,1]
        ) %>%
        add_trace(
          r = as.matrix(dataset_final[4,2:8]),
          theta = c("PctSW","Pct1S","Pct1SW","Pct2SW","PctBPS","P1SR","P2SR"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = dataset_final[4,1]
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,1)
            )
          ),
          showlegend=TRUE
        )
    })
  })
}

## To be copied in the UI
# mod_radar_ui("radar_1")

## To be copied in the server
# mod_radar_server("radar_1")
