#' load_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("viz_print"))
  )
}

#' load_viz Server Functions
#'
#' @noRd 
mod_load_viz_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$test_data  <- renderPrint({r$d_viz})
   
    viz_opts <- reactive({
     req(r$d_viz)
      #print("r_dviz")
      #print
     if (is.null(r$active_viz)) return()  
     if(r$active_viz=="line"){
           # print("line")
           #print(r$d_viz$category)
           req(r$quest_choose)
           # print(r$d_viz)
           if(r$quest_choose != "todas") {
             # print("in")
             # print(r$InsId_band)
             if(!is.null(r$InsId_band)){
               tooltip_t = "<p> Bandera: {flag}  Año: {year} Cumplimiento: {categoryValue} % </p>"
             }
             else{  tooltip_t = "<p> Año: {year} Cumplimiento: {categoryValue} % </p>"}
             # print(tooltip_t)
             # temp= r$d_viz
             # total= sum( temp$categoryValue )
             # temp$categoryValue = temp$categoryValue/total
             # temp$categoryValue =temp$categoryValue*100
             list(
               data = r$d_viz,
               agg = "mean",
               hor_title="Años",
               y_max=100,
               y_min=0,
               ver_title="Cumplimiento",
               format_sample_num = "12.4",
               tooltip=tooltip_t,
               suffix="%"
           )}
            else{
              # print("in2")
              if(!is.null(r$InsId_band)){
                tooltip_t = "<p> Bandera: {flag} Categoria: {category} Año: {year} Cumplimiento: {categoryValue}  % </p>"
              }
              else{   tooltip_t = "<p> Categoria: {category} Año: {year} Cumplimiento: {categoryValue}  %</p>"}
              # temp= r$d_viz
              # total= sum( temp$categoryValue )
              # temp$categoryValue = temp$categoryValue/total
              # temp$categoryValue =temp$categoryValue*100
              # temp = temp %>% group_by(Categoria) %>% summarize(total=n)
              list(
                data = r$d_viz,
                agg = "mean",
                hor_title="Años",
                y_max=100,
                y_min=0,
                ver_title="Cumplimiento",
                format_sample_num = "12.4",
                tooltip=tooltip_t,
                suffix="%"
                   
              )
            }
     }else {
       
       if(r$active_viz=="map"){
         if(!is.null(r$InsId_band)){
           tooltip_t = "<p>Provincia: {description.region}</p><p> Cumplimiento promedio: {categoryvalue} </p>"
         }
         else{ tooltip_t = "<p> Provincia: {description.region} </p><p> Cumplimiento promedio: {categoryvalue}  </p>"}
         # temp= r$d_viz
         # total= sum( temp$categoryvalue )
         # temp$categoryvalue = temp$categoryvalue/total
         # print(temp$categoryvalue )
         list( data = r$d_viz,
               map_name="ecu_provinces",
               title="Promedio de cumplimiento",
               palette_colors = c("#EDF8FB","#006D2C"),
               format_sample_num = "12.4",
               tooltip=tooltip_t,
               y_max=100,
               y_min=0,
               suffix="%",
               map_zoom=FALSE,
               legend_decreasing=TRUE
               
               )
       } 
     }
   })
    
   r_viz <- reactive({
     tryCatch({
       if (is.null(r$active_viz)) return()
       if (is.null(r$v_type)) return()
       # print("map in?")
       #print(r$v_type)
       #print(viz_opts())
       library(hgchmagic)
       do.call(eval(parse(text=r$v_type)),
               viz_opts()
       )
     },
     error = function(cond) {
       return()
     })
   })
   
   output$viz_hgch <- highcharter::renderHighchart({
     if (r$active_viz == "table") return()
     if (r$active_viz == "map") return()
     r_viz()
   })
   
   
   # pal <- leaflet::colorNumeric(
   #   palette = "YlGnBu",
   #   domain = c(0,1)
   # )
   # 
   output$viz_lft <- leaflet::renderLeaflet({
     if (r$active_viz == "table") return()
     if (r$active_viz == "line") return()
     req(r$d_sel_fil)
     #req(r$InsId_band)
     r$active_viz == "map"
     # print((r_viz))
     # print(colnames(r$d_viz))
     # a = r$d_viz
     # print(a)
     r_viz()  %>% #add to package
       htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") 
     
   })
   
   
   
 
   
   
   output$table_dt <- DT::renderDataTable({
     if (r$active_viz != "table") return()
     req(r$d_sel_fil)
     #req(r$InsId_band)
     df <- r$d_sel_fil 
     #print("DT")
     #print(r$InsId_band)
     df$description.role =  toupper(df$description.role)
     df$category = toupper(df$category)
     # if(!is.null(r$InsId_band)){
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
     colnames(df)=lapply(colnames(df),stringr::str_to_sentence)
     df=  df %>% 
       # tidyr:::unnest
       tidyr:::unnest()
     dtable <- DT::datatable(df,
                             rownames = F,
                             #colnames = c("Institución","RUC",#)
                             selection = 'none',
                             options = list(
                               #autoWidth = TRUE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                               #                        lengthChange = F,
                               #                        pageLength = 4,
                               scrollX = T,
                               fixedColumns = TRUE,
                               fixedHeader = TRUE,
                               scrollY = "500px",
                               # initComplete = htmlwidgets::JS(
                               #   "function(settings, json) {",
                               #   "$(this.api().table().header()).css({'text-align': left;});","}"),
                               columnDefs = list(list(className = 'dt-left',targets="_all"))#,
                              # columnDefs = list(list(className = 'upper',targets="_all"))#
                               #                        initComplete = htmlwidgets::JS(
                               #                          "function(settings, json) {",
                               #                          "$(this.api().table().header()).css({'background-color': '#a13e1f', 'color': '#fff'});",
                               #                          "}")
                               #                      )) %>%
                               # DT::formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
                               
                             )) 
     dtable
   })
   
   
   output$viz_print <- renderUI({
     
     if (is.null(r$active_viz)) return()
     
     if (r$active_viz == "table") {
       DT::dataTableOutput(ns("table_dt"), width = 1220)
     } else {
       if (r$active_viz == "line") {
           highcharter::highchartOutput(ns("viz_hgch"), height = 580)
       }
       else{
         leaflet::leafletOutput(ns("viz_lft"), height = 580)
       }
     }
     
   })
   
   
   observe({
     r$downViz <- r_viz()
   })
   
     
  })
}



hgch_line_YeaNum_temp <- function(data, ...) {
  
  if (is.null(data)) stop(" dataset to visualize")
  
  data[[1]] <- homodatum::as_Cat(data[[1]])
  data[[2]] <- homodatum::as_Num(data[[2]])
  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Yea-Num")
  
  d <- l$d
  
  series <- list(list(
    color = d$..colors[1],
    data = purrr::map(1:nrow(d), function(x) {
      list(y = d[[2]][x],
           name = d$a[x],
           label = d$labels[x]
      )
    })
  ))
  
  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'line',
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = l$title$x),
             type = "category") %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_plotOptions(
      series = list(
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )) %>%
    hc_credits(enabled = TRUE, text = l$titles$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))
  
  hc
}
## To be copied in the UI
# mod_load_viz_ui("load_viz_ui_1")

## To be copied in the server
# mod_load_viz_server("load_viz_ui_1")
