# Set options here
options(download.file.method = "libcurl")
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
  library(shiny)
  library(reactlog)
  reactlog::reactlog_enable() 
  options(shiny.reactlog=TRUE)  # Detach all loaded packages and clean your environment
  golem::detach_all_attached()
  # rm(list=ls(all.names = TRUE))
  
  # Document and reload your package
  golem::document_and_reload()
  
    options(shiny.reactlog=TRUE)
  # install.packages("reactlog")
  
  
  library(shiny)
  library(reactlog)
  reactlog::reactlog_enable()
  options(shiny.reactlog=TRUE)
    run_app()

  shiny::reactlogShow()
shiny::reactlogShow()