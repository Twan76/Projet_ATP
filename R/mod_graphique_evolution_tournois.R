#' graphique_evolution_tournois UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom tidyr gather
#' @importFrom stringr str_replace_all
#' @import tibble
#' @import echarts4r

mod_graphique_evolution_tournois_ui <- function(id){
  ns <- NS(id)
  tagList(
    echarts4rOutput(ns("tournois_surface"), height="300px", width="100%")
  )
}

#' graphique_evolution_tournois Server Functions
#'
#' @noRd
mod_graphique_evolution_tournois_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tournois_surface <-renderEcharts4r({
      # Construction table, nombre de tournois joué par surface
      dataset <- data.frame(apply(table(tournament_atp_final_df$Debut,tournament_atp_final_df$Surface), 2, cumsum))
      dataset <- rownames_to_column(dataset)
      names(dataset)[1] <- "DateDebut"

      dataset |>
        e_charts(DateDebut) |>
        e_line(Dur.ext.,color="lightblue") |>
        e_line(Dur.int.,color="purple") |>
        e_line(Gazon, color="green") |>
        e_line(TB.ext.,color="red") |>
        e_line(TB.int.,color="orange") |>
        e_title("Tournois par surface (ATP, Challenger), en cumulé") |>
        e_legend(bottom=20)
    })

  })
}

## To be copied in the UI
# mod_graphique_evolution_tournois_ui("graphique_evolution_tournois_1")

## To be copied in the server
# mod_graphique_evolution_tournois_server("graphique_evolution_tournois_1")
