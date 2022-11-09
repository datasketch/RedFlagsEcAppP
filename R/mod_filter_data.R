#' filter_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' filter_data Server Functions
#'
#' @noRd 
mod_filter_data_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #####  option choose
    
    data_select_opt <- reactive({
      req(r$InsId_rb)
      req(r$d_sel)
      if (is.null(r$InsId_date_sli)) return()
      #req(r$InsId_sli)
      df <- r$d_sel
      #   print(r$InsId_reg_sli)
      # # if(r$InsId_rb="buyer") {
      df <- df %>% dplyr::filter(description.role == r$InsId_rb)# #buyer  supplier
      if(!is.null(r$InsId_sli)){
        if(r$InsId_sli != "") df <- df %>% dplyr::filter(description.party == r$InsId_sli) #party - instiucion
      }
      
      if(!is.null(r$InsId_reg_sli)){ #region
       df <- df %>% dplyr::filter(description.region %in% r$InsId_reg_sli) %>% dplyr::arrange(description.region) #party - instiucion
      }
      
      
      # if(!is.null(r$InsId_reg_sli) ){
      #   df <- df %>% dplyr::filter(description.region %in% r$InsId_reg_sli) %>% dplyr::arrange(description.region) #party - instiucion
      # }
      
    
      
      
      if(!is.null(r$InsId_reg_sli_locality) ){ #ciudad
        df <- df %>% dplyr::filter(description.locality %in% r$InsId_reg_sli_locality) %>% dplyr::arrange(description.locality) #party - instiucion
      }
      
      if (length(r$InsId_date_sli) == 1) { 
        df <-df %>% dplyr::filter(year %in% r$InsId_date_sli)
      }
      
      if (length(r$InsId_date_sli) == 2) {
        yearId <- r$InsId_date_sli[1]:r$InsId_date_sli[2] 
        df <-df %>% dplyr::filter(year %in% yearId)
      }
      # print("flags")
      # print(r$InsId_band)
      
      if(!is.null(r$InsId_band)){ #banderas
       #print("inmodfilter_band")
        
        # temp =  df %>% dplyr::inner_join(results , by= c("RUC"="RUC","description.party"="description.party","description.locality"="description.locality","description.region"="description.region","description.role"="description.role","category"="category"))#, "year"="year"))
        df =  results %>% dplyr::filter(RUC %in% df$RUC & description.party %in% df$description.party &
                                          description.locality %in% df$description.locality & 
                                          description.region %in% df$description.region & 
                                          description.role %in% df$description.role &
                                          category %in% df$category &  year %in% df$year &  flag  %in% r$InsId_band)
        
      }
      
      
      df
    })
    
    observe({
      r$d_sel_fil <- data_select_opt()
    })
    
    
    
  })
}

## To be copied in the UI
# mod_filter_data_ui("filter_data_ui_1")

## To be copied in the server
# mod_filter_data_server("filter_data_ui_1")
