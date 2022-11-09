#' questions_buttons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_questions_buttons_ui <- function(id){
  # ag 	
  # 
  # htmltools::[tag][htmltools::tag], generally <button/> or <a/>, into which to embed the tooltip
  # title 	
  # 
  # character, title for the tooltip
  # placement 	
  # haracter, placement of the tooltip with respect to tag
  # other named arguments, passed to bs_set_data()
  ns <- NS(id)
  tagList(
    uiOutput(ns("generalFilters"))
   
  )
}

#' questions_buttons Server Functions
#'
#' @noRd 
mod_questions_buttons_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    
    output$generalFilters <- renderUI({
      question_buttons(c("todas",  "Transparencia", "Temporalidad", "Competitividad", "Trazabilidad", "Confiabilidad"), 
                       c("Todas", "Transparencia", "Temporalidad", "Competitividad", "Trazabilidad", "Confiabilidad"),
                       c("Todas las categorías", 
                       "Análisis de la cantidad de información disponible de los procedimientos de contratación conforme al OCDS y la normativa ecuatoriana.
",
                       "Los procedimientos se llevaron a cabo en los tiempos establecidos por la normativa ecuatoriana",
                       "La evaluación de las condiciones que permiten una competencia abierta y la participación de varios oferentes en los procedimientos",
                       "Hay capacidad de rastrear información de los procedimientos de contratación a partir de los datos publicados",
                       "Las partes que participan en los procedimientos son confiables, de acuerdo con su desempeño"
                       )
      )
      
     
      # bsTooltip(id, title, placement = "bottom", trigger = "hover",
      #           options = NULL)
      
    })
    
 
  })
}

## To be copied in the UI
# mod_questions_buttons_ui("questions_buttons_ui_1")

## To be copied in the server
# mod_questions_buttons_server("questions_buttons_ui_1")
