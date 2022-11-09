#' viz_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' viz_data Server Functions
#'
#' @noRd 
mod_viz_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_viz <- reactive({
      req(r$d_sel_fil)
      req(r$quest_choose)
      # print("mov_vis_data")
      # #print(r$d_sel_fil)
      # print(r$quest_choose)
      # #req(r$active_viz)
      df <- r$d_sel_fil
      # print("active")
      
      if (is.null(r$active_viz)) r$active_viz = "line"
      # print("typeviz")
      # print(r$active_viz)
      # print(r$InsId_band)
      # print(colnames(df))
      if(r$active_viz == "line"){
        df = df  %>% dplyr::filter(category!="Cantidad de contratos" )
         if(!is.null(r$InsId_band) & !is.null(df$flag)){
          df <- df[,c("category", "year", "categoryValue","flag")]
         
        }
        else{    df <- df[,c("category", "year", "categoryValue")]
       
        }
       
          
          # df_temp <- df %>% dplyr::select(category,year, categoryValue) %>% dplyr::group_by(category,year) %>%
          #   dplyr::summarize(categoryvalue= mean(categoryValue, na.rm=TRUE),ds= sd(categoryValue, na.rm=TRUE))
          # # saveRDS(df_temp,"df_temp")
          # print(df_temp)
          if(r$quest_choose != "todas") {
            # print("choooooose_vizdatta")
            # print(r$quest_choose)
            # print(nrow(df))
            # print(colnames(df))
            if(!is.null(r$InsId_band)){
              df <- df[,c("year", "categoryValue","flag")]
            }
            else{    df <- df[,c( "year", "categoryValue")]}
            # print("MVD")
            # print(colnames(df))
            # df <- df[,-1]
          }
      }
      else{
        # print("in map")
        df <- df %>% dplyr::filter(category!="Cantidad de contratos" ) %>%
                                  dplyr::select(description.region, categoryValue) %>% dplyr::group_by(description.region) %>%
          # dplyr::summarize(categoryvalue= mean(categoryValue, na.rm=TRUE),ds= round(sd(categoryValue, na.rm=TRUE),digits = 1))
          dplyr::summarize(categoryvalue= mean(categoryValue, na.rm=TRUE))
        
        # print(df)
      }
      df
    })
    
    observe({
      r$d_viz <- data_viz()
    })
    
  })
}

## To be copied in the UI
# mod_viz_data_ui("viz_data_ui_1")

## To be copied in the server
# mod_viz_data_server("viz_data_ui_1")
