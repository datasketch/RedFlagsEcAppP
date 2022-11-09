#' load_parmesan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_parmesan_ui <- function(id){
  ns <- NS(id)
  tagList(
      uiOutput(ns("controls"))
  )
}

#' load_parmesan Server Functions
#'
#' @noRd 
mod_load_parmesan_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    type_opts <- reactive({
      
      req(r$d_sel)
      df <- r$d_sel 
      unique(df$description.role)
      # df %>% dplyr::distinct(description.role)
    })
    
    sel_in_opts <- reactive({
      req(r$d_sel)
      req(r$InsId_rb)
      df <- r$d_sel 
      selin <- df %>% dplyr::filter(description.role %in% r$InsId_rb) %>%
        # dplyr::arrange(description.role) %>%
        dplyr::distinct(description.party) %>%
        dplyr::arrange(desc(description.party))
      # print("selin")
      # print(selin %>% head(10))
      # selin=unique(df$description.party)
      selin$description.party
      # print("CLASSSELIN")
      # print(class(selin))
     
      #print(selin)
      
    })
    
    
    
    sel_reg_in_opts_locality <- reactive({
      req(r$d_sel)
      req(r$InsId_rb)
      
      df <- r$d_sel
      # print("selin_locality")
      # print(r$InsId_reg_sli)
      if(is.null(r$InsId_reg_sli)){
        selin = df %>% dplyr::distinct(description.locality)  %>% dplyr::arrange(description.locality)
        
      }else{
        df <- df %>% dplyr::select(description.region,description.locality) %>%  dplyr::filter(description.region %in% r$InsId_reg_sli)
        selin = df %>% dplyr::distinct(description.locality)  %>% dplyr::arrange(description.locality)
        
      }
     
      # selin=unique(df$description.locality)
      # print("CLASSSELIN")
      # print(class(selin))
      selin$description.locality
      #print(selin)
      
    })
    
    
    
    sel_in_opts_band  <- reactive({
      # if (is.null(r$d_sel)) return()
      req(r$d_sel)
      req(r$quest_choose)
      df <- r$d_sel 
      # print("results")
      # print(colnames(results))
      # 
      # print(colnames(summaries))
      # print("df")
      # print(colnames(df))
      # print(df %>% head(10))
      # 
      # # colnames(results) = c("RUC","description.party","description.locality","description.region","description.role","flag","category","result", "year", "categoryValue")
      # print(results %>% head(10))
      # print(colnames(results))
      # # temp =  summaries  %>% dplyr::inner_join(results , by= c("description.parties.id"="description.parties.id","description.parties.name"="description.parties.name","description.parties.address.locality"="description.parties.address.locality","description.parties.address.region"="description.parties.address.region","description.parties.roles"="description.parties.roles","category"="result.category","year"="year.year","categoryValue"="year.value"))
      # temp =  df %>% dplyr::inner_join(results , by= c("description.parties.id"="description.parties.id","description.parties.name"="description.parties.name","description.parties.address.locality"="description.parties.address.locality","description.parties.address.region"="description.parties.address.region","description.parties.roles"="description.parties.roles","category"="result.category","year"="year.year","categoryValue"="year.value"))
      # 
      if(r$quest_choose!="todas") {
      #   
          # print("parmesandiftodas")
          # temp =  df %>% dplyr::inner_join(results , by= c("RUC"="RUC","description.party"="description.party","description.locality"="description.locality","description.region"="description.region","description.role"="description.role","category"="category"))#, "year"="year"))
          temp =  results %>% dplyr::filter(RUC %in% df$RUC & description.party %in% df$description.party & description.locality %in% df$description.locality & description.region %in% df$description.region & description.role %in% df$description.role & category %in% df$category &  year %in% df$year)
          
          # print("temp")
          # print(colnames(temp))
          bands= temp %>% dplyr::distinct(flag)  %>% dplyr::arrange(flag) %>% dplyr::filter(flag != "Cantidad de contratos")
         # bands=unique(temp %>% dplyr::select(flag) %>% dplyr::filter(flag != "null")%>% dplyr::arrange(flag))
          # print(bands %>% head(10))
          # print(class(bands))
          #str(bands)
          # 
      }else{
       # print("parmesn-todas")
        bands= results %>% dplyr::distinct(flag)  %>% dplyr::arrange(flag) %>% dplyr::filter(flag != "Cantidad de contratos")
        
       # bands =unique(results %>% dplyr::select(flag) %>% dplyr::filter(flag != "null")%>% dplyr::arrange(flag))
      }
      bands$flag 
      # 
      })
    
    
    sel_reg_in_opts <- reactive({
      req(r$d_sel)
      df <- r$d_sel 
      #print("in_opts")
      #print(colnames(df))
      in_opts=df %>% dplyr::distinct(description.region) %>% dplyr::filter(description.region != "null")%>% dplyr::arrange(description.region)
      
      # in_opts=unique(df %>% dplyr::select(description.region) %>% dplyr::filter(description.region != "null")%>% dplyr::arrange(description.region))
      #print(in_opts)
      in_opts$description.region
    })
    
    # min: sel_date_min_opts()
    # min: sel_date_max_opts()
    # value: sel_date_mean_opts()
    # 
    sel_date_min_opts <- reactive({
      
      req(r$d_sel)
      df <- r$d_sel 
      min(as.numeric(df$year),na.rm= TRUE)
    })
    
    sel_date_max_opts <- reactive({
      
      req(r$d_sel)
      df <- r$d_sel 
      max(as.numeric(df$year),na.rm= TRUE)
    })
    
    
    sel_date_mean_opts <- reactive({
      
      req(sel_date_min_opts())
      req(sel_date_max_opts())
      c(sel_date_min_opts(),  sel_date_max_opts())
      # req(r$d_sel)
      # df <- r$d_sel 
      # max(as.numeric(df$year),na.rm= TRUE)
    })
    
    
    
    
    # data_select <- reactive({
    #   req(r$quest_choose)
    #   df <- summaries
    #   if(r$quest_choose!="todas") {
    #     df <- df %>% dplyr::filter(category == r$quest_choose)
    #   }
    #   df
    # })
    # 
    # observe({
    #   
    #   r$d_sel <- data_select()
    # })
    # 
    # 
    
    # # Initialize parmesan
    path <- app_sys("app/app_config/parmesan")
    parmesan <- parmesan::parmesan_load(path)
    parmesan_input <- parmesan::parmesan_watch(input, parmesan)

    parmesan::output_parmesan("controls",
                              parmesan = parmesan,
                              #r = r,
                              input = input,
                              output = output,
                              session = session,
                              env = environment())
    # # ======================================================================================================================
    # Pass all inputs from parmesan to other parts of the app as reactiveValues
    parmesan_inputs <- purrr::map(parmesan, function(.x) { purrr::map_chr(.x$inputs, "id")}) %>% unlist(use.names = FALSE)

    observe({
      for(parmesan_input in parmesan_inputs){
        get_input <- input[[parmesan_input]]
        #if(!is.null(get_input)){
        r[[parmesan_input]] <- get_input
        #}
      }
    })

    observe({
      r$parmesan_input <- parmesan_input()
    })
    
    
  })
}

## To be copied in the UI
# mod_load_parmesan_ui("load_parmesan_ui_1")

## To be copied in the server
# mod_load_parmesan_server("load_parmesan_ui_1")
