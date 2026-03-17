# app.R -- minimal, main-panel-only param runner; saves to here::here("Output", ...)
library(shiny)
library(knitr)
library(rmarkdown)
library(here)

file <- "test.Rmd"  # default Rmd (must exist in app working directory)

# ensure Output directory exists
dir.create(here("Output"), showWarnings = FALSE, recursive = TRUE)

# --- tiny helpers ---
shinyArgs <- function(param, ns) {
  param$inputId <- ns(param$name)
  if (is.null(param$label)) param$label <- param$name
  if (!is.null(param$input) && param$input %in% c("select","radio")) {
    param$selected <- param$value
    param$value <- NULL
  }
  param$name <- NULL; param$input <- NULL
  param
}

getInputFun <- function(inputName) {
  if (is.null(inputName)) return(textInput)
  if (inputName == "radio") return(radioButtons)
  get(paste0(inputName, "Input"))
}

param_ui <- function(params, name, ns) {
  p <- params[[name]]
  do.call(getInputFun(p$input), shinyArgs(p, ns))
}

paramsUI <- function(id, file) {
  params <- knitr::knit_params(readLines(file))
  ns <- NS(id)
  tagList(lapply(names(params), function(n) param_ui(params, n, ns)))
}

getParams <- function(values, meta) {
  res <- lapply(names(values), function(n){
    it <- meta[[n]]$input
    if (!is.null(it) && it == "file") {
      v <- values[[n]]
      if (is.null(v)) meta[[n]]$value else v$datapath
    } else values[[n]]
  })
  names(res) <- names(values); res
}

# --- UI: main panel only, responsive full-width column ---
ui <- fluidPage(
  titlePanel("Minimal Rmd Param Runner"),
  fluidRow(
    column(
      width = 12,
      style = "padding-left:12px; padding-right:12px;", # keep it responsive
      tags$h4("Parameters from test.Rmd"),
      paramsUI("p", file),
      br(),
      actionButton("render", "Render (save to Output/)", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("status")
    )
  )
)

# --- server ---
server <- function(input, output, session) {
  params_meta <- knitr::knit_params(readLines(file))
  params_out <- callModule(function(input, output, session) {
    reactive({ getParams(reactiveValuesToList(input), params_meta) })
  }, "p")
  
  generated <- reactiveVal(NULL)
  
  output$status <- renderText({
    g <- generated()
    if (is.null(g)) "No report yet." else paste0("Last generated report:\n", g)
  })
  
  observeEvent(input$render, {
    params <- params_out()
    
    out_path <- here::here(
      "Output",
      paste0(
        tools::file_path_sans_ext(basename(file)),
        "_",
        params$upper_peak_window,
        ".html"
      )
    )
    
    output$status <- renderText("Rendering...")
    
    tryCatch({
      rmarkdown::render(
        input = file,
        params = params,
        output_file = out_path,
        envir = new.env(parent = globalenv())
      )
      
      generated(out_path)
      output$status <- renderText(paste0("Rendered and saved to:\n", out_path))
      
    }, error = function(e) {
      generated(NULL)
      output$status <- renderText(paste0("Render error:\n", e$message))
    })
  })
}

shinyApp(ui, server)