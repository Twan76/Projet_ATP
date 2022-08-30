#' timeline_tournois UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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

      config <- list(
        zoomKey = "ctrlKey"
      )

      timevis(data.frame(
        start = tournament_atp_final_df$Debut, end = tournament_atp_final_df$Fin,
        content = tournament_atp_final_df$Surface,
        group = tournament_atp_final_df$Categorie),
        groups = data.frame(id = unique(tournament_atp_final_df$Categorie), content = unique(tournament_atp_final_df$Categorie)))

        # vacc_data <- tournament_atp_final_df %>%
        #   rowid_to_column(var = "id") %>%
        #   filter(pet_name %in% input$pet, current_flag %in% input$vacc) %>%
        #   mutate(title = paste("Date Given: ", format(as.Date(start), format = "%m-%d-%Y"), "\n", "Date Expires: ", format(as.Date(end), format = "%m-%d-%Y"), "\n" ,"Vet: ", vet_name, sep = ""),
        #          group = content)
        #
        # groups <- data.frame(
        #   id = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test"),
        #   content = c("Rabies", "Distemper", "Bordetella (drops)", "Bordetella (injection)", "Flu", "Lepto", "Rattlesnake", "Fecal Test", "Heartworm Test")
        # )
        # timevis(vacc_data, groups = groups, options = config)
    })

  })
}

## To be copied in the UI
# mod_timeline_tournois_ui("timeline_tournois_1")

## To be copied in the server
# mod_timeline_tournois_server("timeline_tournois_1")
