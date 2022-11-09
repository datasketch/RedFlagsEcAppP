#' download_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("descargas"))
  )
}
    
#' download_viz Server Functions
#'
#' @noRd 
mod_download_viz_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$descargas <- renderUI({
      
      if (is.null(r$active_viz)) return()
      if (r$active_viz != "table") {
        dsmodules::downloadImageUI(ns("download_viz"), dropdownLabel ="Descargar", text = "Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown")
      } else {
        dsmodules::downloadTableUI(ns("dropdown_table"), dropdownLabel = "Descargar", text = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
      }
    })
    
    observe({
      req(r$d_sel_fil)
      df <- r$d_sel_fil 
      
      if (r$active_viz == "table") {
     
      df$description.role =  toupper(df$description.role)
      df$category = toupper(df$category)
      # if(!is.null(r$InsId_band)){
      print(r$InsId_band)
      if(!is.null(r$InsId_band)){
        
        colnames(df) = c("Nombre","RUC","Tipo","Categoría","Bandera","Cumplimiento %","Año","Ciudad","Provincia")
        # `Número de banderas`=n()
        if(r$quest_choose=="todas") {
          t1 = df %>% dplyr::filter(`Categoría` != "CANTIDAD DE CONTRATOS") %>% dplyr::group_by(Nombre, RUC, Tipo, Año, Ciudad, Provincia) %>% dplyr::summarize(Total = mean(`Cumplimiento %`)) %>% dplyr::ungroup()
          df = df %>%  inner_join(t1) %>% tidyr::pivot_wider(names_from = Bandera, values_from = `Cumplimiento %`) 
          
          if("CANTIDAD DE CONTRATOS" %in% names(df)){
            df = df %>% relocate("CANTIDAD DE CONTRATOS", .after = Provincia)
          }
        }
        else{
          
          df = df %>% tidyr::pivot_wider(names_from = Bandera, values_from = `Cumplimiento %`) 
          if("CANTIDAD DE CONTRATOS" %in% names(df)){
            df = df %>% relocate(`CANTIDAD DE CONTRATOS`, .after = Provincia)
          }
        }
      }
      else{ colnames(df) = c("Nombre","RUC","Tipo","Categoría","Cumplimiento %","Año","Ciudad","Provincia")
      # print( df %>% dplyr::filter(`Categoría` == "Cantidad de contratos") %>% dplyr::select(`Cumplimiento %`))
      # print( df %>% dplyr::filter(`Categoría` != "Cantidad de contratos") %>% dplyr::select(`Cumplimiento %`))
      # print( df %>% dplyr::distinct(`Categoría` ))
      # write.csv(df %>% dplyr::filter(`Categoría` != "CANTIDAD DE CONTRATOS") %>% dplyr::group_by(Nombre, RUC, Tipo, Año, Ciudad, Provincia) %>% dplyr::summarize(coun=n()), "tempcount.csv")
      if(r$quest_choose=="todas") {
        t1 = df %>% dplyr::filter(`Categoría` != "CANTIDAD DE CONTRATOS") %>% dplyr::group_by(Nombre, RUC, Tipo, Año, Ciudad, Provincia) %>% dplyr::summarize(Total = mean(`Cumplimiento %`)) %>% dplyr::ungroup() %>% as.data.frame()
        df= df %>% inner_join(t1) %>% distinct() %>% tidyr::pivot_wider(names_from = Categoría, values_from = `Cumplimiento %`) 
        print(colnames(df))
        print(names(df))
        if("CANTIDAD DE CONTRATOS" %in% names(df)){
          df = df %>% relocate(`CANTIDAD DE CONTRATOS`, .after = Provincia)
        }
      }
      else{
        df= df %>% tidyr::pivot_wider(names_from = Categoría, values_from = `Cumplimiento %`) 
        print(colnames(df))
        print(names(df))
        if("CANTIDAD DE CONTRATOS" %in% names(df)){
          df = df %>% relocate(`CANTIDAD DE CONTRATOS`, .after = Provincia)
        }
        
      }
      }
      
      # else{
      #   colnames(df) = c("Nombre","RUC","Tipo","Categoría","Cumplimiento","Año","Ciudad","Provincia")
      #   
      # }
      # #temporal transformation TODO:do it with javascript
      
      # total= sum( df$Cumplimiento )
      # df$Cumplimiento = df$Cumplimiento/total
      # df$Cumplimiento = df$Cumplimiento*100
      # print(df$Cumplimiento)
      print("before")
      print(colnames(df))
      colnames(df)=lapply(colnames(df),stringr::str_to_sentence)
      print("after")
      df=  df %>% 
        # tidyr:::unnest
        tidyr:::unnest()
      # print(df %>%
      #   dplyr::group_by(Nombre, Ruc, Tipo, Categoría, Año, Ciudad, Provincia, Total, `Análisis de red`) %>%
      #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      #   dplyr::filter(n > 1L))
    print(str(df))
       
      # df$`Análisis de red`= unlist(df$`Análisis de red`)
      dsmodules::downloadTableServer("dropdown_table", element = reactive(df), formats = c("csv", "xlsx", "json"))
      }
      dsmodules::downloadImageServer("download_viz", element = reactive(r$downViz), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
    })
    
  })
}
    
## To be copied in the UI
# mod_download_viz_ui("download_viz_ui_1")
    
## To be copied in the server
# mod_download_viz_server("download_viz_ui_1")
