#' selected_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selected_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' selected_data Server Functions
#'
#' @noRd 
mod_selected_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    data_select <- reactive({
      req(r$quest_choose)
     #  colnames(summaries) = c("RUC","description.party","description.locality","description.region","description.role","category","categoryValue", "year")
     # # colnames(results) = c("RUC","description.party","description.locality","description.region","description.role","flag","category","result", "year", "categoryValue")
      
      df <- summaries %>% dplyr::filter(!is.na("description.region"))
      # unique(summaries %>% dplyr::select(description.region) %>% dplyr::filter(description.region != "null")%>% dplyr::arrange(description.region))
      # summaries %>% dplyr::select(description.region) %>% dplyr::filter(description.region == "null") 
      # df$categoryValue = df$categoryValue*100
      # df$category
      # df = df %>% dplyr::mutate(category = dplyr::case_when(category== "trans" ~ "Transparencia",
      #                                                       category == "temp" ~ "Temporalidad",
      #                                                       category == "comp" ~ "Competencia",
      #                                                       category == "traz" ~ "Trazabilidad",
      #                                                       category== "network" ~ "Confiabilidad",
      #                                                       category== "total_score" ~ "total_score",
      #                                           TRUE ~ category))
     if(r$quest_choose!="todas") {
      df <- df %>% dplyr::filter(category == r$quest_choose | category=="Cantidad de contratos") 
      # print("choooooose_selected_data")
      # print(r$quest_choose)
      # print(nrow(df))
      # print(colnames(df))
      #print(df)
      }
      df
    })
    
    observe({
      
      r$d_sel <- data_select()
    })
    

    
  })
}

## To be copied in the UI
# mod_selected_data_ui("selected_data_ui_1")

## To be copied in the server
# mod_selected_data_server("selected_data_ui_1")
