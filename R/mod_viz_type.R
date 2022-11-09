#' viz_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_viz_type_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' viz_type Server Functions
#'
#' @noRd 
mod_viz_type_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    viz_type <- reactive({
      req(r$d_viz)
      tv <- "YeaNum"
      if(!is.null(r$InsId_band)){
        if (ncol(r$d_viz) == 4) tv <- "CatYeaNum" 
      }
      else{
        if (ncol(r$d_viz) == 3) tv <- "CatYeaNum" 
      }
      tv
    })
    
    
    
    
    viz_name <- reactive({
      tryCatch({
        req(viz_type())
        if (r$active_viz == "table") return()
        # print("vizzzzzz")
        # print(r$active_viz)
        if(r$active_viz == "line"){
         # print("viz_name") 
         vp <- paste0("hgchmagic::", paste0("hgch_", r$active_viz, "_", viz_type()))
         # print(vp)
        }
        if(r$active_viz == "map"){
          vp <- paste0("lfltmagic::", "lflt_choropleth_GnmNum") # TODO update with active_viz and vi type
         }
        vp
      },
      error = function(cond) {
        return()
      })
    })
    
    
    observe({
      r$v_type <- viz_name()
    })
    
    
  })
}

## To be copied in the UI
# mod_viz_type_ui("viz_type_ui_1")

## To be copied in the server
# mod_viz_type_server("viz_type_ui_1")
