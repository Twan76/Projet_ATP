#' timeline_tournois UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import timevis
#' @import lubridate

mod_timeline_tournois_ui <- function(id){
  ns <- NS(id)

  tagList(
    shinycssloaders::withSpinner(
      timevisOutput(ns("tournament_timeline")),
      type = 6
    )
  )
}

#' timeline_tournois Server Functions
#'
#' @noRd
mod_timeline_tournois_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create vaccine timeline ####
    output$tournament_timeline <- renderTimevis({


      if(nrow(r_global$dataset_timeline())==0){
        timevis()
      } else {

      config <- list(
        zoomKey = "ctrlKey"
      )

      dataset <- r_global$dataset_timeline()

      # Ajout d'une couche style css pour colorier les boutons par surface
      dataset$style = case_when(
        dataset$Surface == "TB int." ~ "border-color: #FF3333; color: white; background-color: #FF3333;",
        dataset$Surface == "TB ext." ~ "border-color: #FF9933; color: white; background-color: #FF9933;",
        dataset$Surface == "Gazon" ~ "border-color: #00FF80;color: white;  background-color: #00FF80;",
        dataset$Surface == "Dur int." ~ "border-color: #99CCFF; color: white;  background-color:  #99CCFF;",
        dataset$Surface == "Dur ext." ~ "border-color: #0080FF; color: white; background-color: #0080FF;",
        dataset$Surface == "Moquette" ~ "border-color: #009999;color: white;  background-color: #009999;",
        dataset$Surface %in% c("Indoor","Outdoor") ~ "border-color: #808080;color: white;  background-color: #808080;"
        )

      timevis(data.frame(
        start = dataset$Debut, end = dataset$Fin,
        content = dataset$Tournoi, style = dataset$style,
        title = paste0(" <b>Tournoi : </b> ", dataset$Tournoi,", ", dataset$Ville,", ", dataset$Pays,
                       "</br><b>Surface : </b> ", dataset$Surface,
                       "</br><b> Date : </b> ", "Du ", format(dataset$Debut,"%d/%m/%Y"), " au ", format(dataset$Fin, "%d/%m/%Y"),
                       "</br><b>Catégorie :</b> ", dataset$Categorie),
        group = dataset$Categorie),
        groups = data.frame(id = c("Challenger 50", "Challenger 80", "Challenger 90", "Challenger 100", "Challenger 125","ATP 250", "ATP 500", "Masters 1000", "Grand Chelem","Autre"), content = c("Challenger 50", "Challenger 80", "Challenger 90", "Challenger 100", "Challenger 125","ATP 250", "ATP 500", "Masters 1000", "Grand Chelem","Autre")), options=config)  %>%
        setWindow(Sys.Date() %m-% days(14), Sys.Date() %m+% days(14))
      }
    })

  })
}

## To be copied in the UI
# mod_timeline_tournois_ui("timeline_tournois_1")

## To be copied in the server
# mod_timeline_tournois_server("timeline_tournois_1")
