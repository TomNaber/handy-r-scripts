launch_launcher <- function(inputFile, out_path = NULL) {
  
  pm <- knitr::knit_params(readLines(inputFile))
  
  # --- helpers (unchanged, minimal) ---
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
    if (is.null(inputName)) return(shiny::textInput)
    if (inputName == "radio") return(shiny::radioButtons)
    get(paste0(inputName, "Input"), asNamespace("shiny"))
  }
  
  param_ui <- function(params, name, ns) {
    p <- params[[name]]
    do.call(getInputFun(p$input), shinyArgs(p, ns))
  }
  
  paramsUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(lapply(names(pm), function(n) param_ui(pm, n, ns)))
  }
  
  getParams <- function(values) {
    res <- lapply(names(values), function(n){
      it <- pm[[n]]$input
      if (!is.null(it) && it == "file") {
        v <- values[[n]]
        if (is.null(v)) pm[[n]]$value else v$datapath
      } else values[[n]]
    })
    names(res) <- names(values)
    res
  }
  
  # --- app ---
  app <- shiny::shinyApp(
    
    ui = shiny::fluidPage(
      shiny::titlePanel("Analysis Configuration"),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$h4("Parameters"),
          paramsUI("p"),
          shiny::br(),
          shiny::actionButton("go", "Render", class = "btn-primary"),
          shiny::br(), shiny::br(),
          shiny::verbatimTextOutput("status")
        )
      )
    ),
    
    server = function(input, output, session) {
      
      params_out <- shiny::callModule(function(input, output, session) {
        shiny::reactive({
          getParams(shiny::reactiveValuesToList(input))
        })
      }, "p")
      
      output$status <- shiny::renderText("No report yet.")
      
      shiny::observeEvent(input$go, {
        params <- params_out()
        
        # --- output logic (your clean spec) ---
        if (is.null(out_path) || identical(out_path, "")) {
          out <- here::here(paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        } else if (is.function(out_path)) {
          out <- out_path(params, inputFile)
        } else if (grepl("\\.html$", out_path, ignore.case = TRUE)) {
          if (!dir.exists(dirname(out_path))) dir.create(dirname(out_path), recursive = TRUE)
          out <- out_path
        } else {
          if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
          out <- file.path(out_path, paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        }
        
        output$status <- shiny::renderText("Rendering...")
        
          rmarkdown::render(
            input = inputFile,
            params = params,
            output_file = out,
            envir = new.env(parent = globalenv())
          )
          
          output$status <- shiny::renderText(paste0("Saved to:\n", out))
          shiny::stopApp()
      })
    }
  )
  
  shiny::runApp(
    app,
    launch.browser = TRUE
  )
}