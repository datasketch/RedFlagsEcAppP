#' Question's buttons
#'
#' @description A util function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

question_buttons <- function(ids = NULL, labels = NULL, tooltips=NULL, ...) {
  if (is.null(ids)) stop("Please enter identifiers for each question")
  if (is.null(labels)) stop("Please enter labels for each question")
  if (is.null(tooltips)) stop("Please enter tooltips for each question")
  
  df <- data.frame(id = ids, questions = labels, tools=tooltips)
  l <- purrr::map(1:nrow(df), function(z){
    # print(df[z,]$id)
    # print(session)
    # htmltools::withTags(
    #   div(class = "BB",
    shiny::actionButton(inputId = df[z,]$id, label = df[z,]$questions, class = "needed") %>%
    # shiny::numericInput(inputId = df[z,]$id, label = df[z,]$questions, value=0) %>%
    #    bsplus::shinyInput_label_embed(
    #   shiny::icon("info") %>%
         # bsplus::bs_embed_popover(title=df[z,]$id,content=df[z,]$tools,placement = "left")
         bsplus::bs_embed_tooltip(id=df[z,]$id,title =df[z,]$tools)
     
      # ))
    # bsplus::bs_embed_tooltip(id=df[z,]$id,title =df[z,]$tools)
    # shinyBS::addTooltip(session=session,id=df[z,]$id,title="Hello! This is a hover pop-up. You'll have to click to see the next one.")
    
  })
  l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
  l[[1]] <- htmltools::HTML(paste0(paste(l[[1]], collapse = '')))
  
  l
}

