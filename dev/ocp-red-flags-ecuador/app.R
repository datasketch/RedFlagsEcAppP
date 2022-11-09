
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)

library(tictoc)

#load("data/results.rda")
#load("data/summaries.rda")
# results <- results |> sample_n(1e6)
# saveRDS(results, "data/results.rds")

tic()
results <- read_rds("data/results.rds")
summary <- read_rds("data/summaries.rds")
toc()



ui <- panelsPage(
  panel(title = "Opciones", width = 250,
        body = div(
          selectizeInput("sel_region","Seleccione Región",opts_region,
                         selected = "colombia"),
          hr(),
          radioButtons("sel_grupo_type", "Tipo de grupo", 
                       c( "Biológico" = "biologico", "Interés de Conservación" = "interes")),
          conditionalPanel("input.sel_grupo_type == 'biologico'",
                           selectizeInput("sel_grupo_biologico","Seleccione grupo",opts_grupo_biologico)
          ),
          conditionalPanel("input.sel_grupo_type == 'interes'",
                           selectizeInput("sel_grupo_interes","Seleccione grupo",opts_grupo_interes)
          ),
          hr(),
          radioButtons("registro_especie", "Tipo registo", c("Todos" = "todos", "Observaciones" = "registro","Especies"="especie")),
          radioButtons("modo", "Modo", c("Todos" = "todos","Continental" = "continental","Marino" = "marinas")),
          radioButtons("tematica", "Temática", c("todas","amenazadas", "cites", "endemicas", "migratorias", "exoticas", "invasoras")),
          verbatimTextOutput("debug"),
          br()
        ),
        footer = ""),
  panel(title = "Información",
        body = div(
          uiOutput("controls"),
          uiOutput("viz"),
          br()
        ),
        footer = "")
)

server <-  function(input, output, session) {
  
  
  output$debug <- renderText({
    #capture.output(str(data()))
    #glimpse(data())
    input$sel_grupo 
  })
  
  
  sel_grupo <- reactive({
    if(input$sel_grupo_type == "biologico"){
      return(input$sel_grupo_biologico)
    } else {
      return(input$sel_grupo_interes)
    }
  })
  
  d_gr <- reactive({
    if(input$sel_grupo_type == "biologico"){
      d <- ds$region_grupo_biologico
      if(sel_grupo() != "todos")
        d <- d |> filter(slug_grupo_biologico == sel_grupo())
    } else {
      d <- ds$region_grupo_interes_conservacion
      if(sel_grupo() != "todos")
        d <- d |> filter(slug_grupo_interes_conservacion == sel_grupo())
    }
    d
  })
  
  d_gr_reg <- reactive({
    d <- d_gr()
    ind_reg <- d |> 
      filter(slug_region == input$sel_region)
    ind_reg
  })
  
  d_gr_subreg <- reactive({
    d <- d_gr()
    subregs <- ds$region |>
      filter(parent == input$sel_region) |> 
      pull(slug)
    ind_subregs <- d |> 
      filter(slug_region %in% subregs)
    ind_subregs
  })
  
  
  vars_meta <- reactive({
    inds <- ds$ind_meta
    if(input$tematica != "todas"){
      inds <- inds |> 
        filter(grepl(input$tematica,tematica))
    }
    if(input$modo != "todos"){
      inds <- inds |> 
        filter(grepl(input$modo,modo))
    }
    if(input$registro_especie != "todos"){
      inds <- inds |> 
        filter(grepl(input$registro_especie,tipo))
    }
    inds |> pull(indicador)
  })
    
  
  data <- reactive({
    #req(d_gr_reg())
    d <- d_gr_reg()
    if(input$region_type == "subregion"){
      d <- d_gr_subreg()
    }
    
    vars <- c("slug", vars_meta())

    d <- d |>
      select(contains(vars))

    d
  })
  
  output$controls <- renderUI({
    out <- list()
    
    out <- list(
      radioButtons("region_type", "Tipo", 
                   c( "Total región"="region", "Subregiones"="subregion"))
    )
    out
  })
  
  output$viz <- renderUI({

    d <- data()
    #d <- l[[input$chart_type]]
    
    renderDataTable({
      d
    })
    
  })
  
  
  
  
}

shinyApp(ui, server)
