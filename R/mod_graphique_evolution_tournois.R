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
#' @import ggplot2

mod_graphique_evolution_tournois_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("tournois_surface"), height="300px", width="100%")
  )
}

#' graphique_evolution_tournois Server Functions
#'
#' @noRd
mod_graphique_evolution_tournois_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tournois_surface <-renderPlot({
      # if(nrow(r_global$dataset())>1){
      if(length(unique(r_global$dataset()$Debut))>1){
      # Construction table, nombre de tournois joué par surface
      dataset <- data.frame(apply(table(r_global$dataset()$Debut,r_global$dataset()$Surface), 2, cumsum))
      dataset$DateDebut <- as.Date(row.names(dataset))

      dataset <- dataset %>%
              gather(key = "Surface", value = "value", -DateDebut)

      dataset$Surface <- str_replace_all(dataset$Surface,"TB.int.","TB int.")
      dataset$Surface <- str_replace_all(dataset$Surface,"Dur.int.","Dur int.")
      dataset$Surface <- str_replace_all(dataset$Surface,"TB.ext.","TB ext.")
      dataset$Surface <- str_replace_all(dataset$Surface,"Dur.ext.","Dur ext.")

      ggplot(dataset, aes(x = DateDebut, y = value)) +
        geom_line(aes(color = Surface, linetype = Surface)) +
        geom_point(aes(color = Surface), size=1) +
        ylab("Nombre tournois") +  xlab("Date") + theme_bw() +
        ggtitle("Nombre tournois par surface (cumulé)") +
        theme(legend.title = element_blank(),     panel.border=element_blank(), legend.position = "bottom")

      }
    })

  })
}

## To be copied in the UI
# mod_graphique_evolution_tournois_ui("graphique_evolution_tournois_1")

## To be copied in the server
# mod_graphique_evolution_tournois_server("graphique_evolution_tournois_1")
