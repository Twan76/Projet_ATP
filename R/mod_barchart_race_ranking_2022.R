#' barchart_race_ranking_2022 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinycssloaders

mod_barchart_race_ranking_2022_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(
      htmlOutput(ns("evol_ranking_2022"), height = '515px'),
      type = 6
    )
  )
}

#' barchart_race_ranking_2022 Server Functions
#'
#' @noRd
mod_barchart_race_ranking_2022_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$evol_ranking_2022 <- renderUI({

      if(nrow(r_global$dataset_ranking_2022())>0) {

        ranking_atp_df <- r_global$dataset_ranking_2022() %>%
          mutate(frame_label = format(Date,"%d/%m/%Y"))

        gd <- barchartrace_r2d3(
          data = ranking_atp_df,
          name = "Joueur", date = "Date", value = "Points", date_label = "frame_label", colour = "Joueur",
          cumulative = FALSE,
          title = "Evolution classement ATP 2022",
          subtitle = paste0("Top ", r_global$top_n_2022(), ", en points ATP"),
          caption = "Source: ATP World Tour",
          mood = "neutral", top_n = r_global$top_n_2022(), duration = 2500,
          width = "1200px", height = "500px"
        )

        file_out <- "inst/app/www/out_bcr/evol_atp_2022.html"
        saveWidgetFix(widget = gd, file = file_out, selfcontained = TRUE)
        tags$iframe(src = paste0("www/out_bcr/", basename(file_out)), height = "600", width = "100%", frameBorder = "0")
      }
    })
  })
}

## To be copied in the UI
# mod_barchart_race_ranking_2022_ui("barchart_race_ranking_2022_1")

## To be copied in the server
# mod_barchart_race_ranking_2022_server("barchart_race_ranking_2022_1")
