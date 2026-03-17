launch_launcher <- function(inputFile, out_path = NULL) {
  
  pm <- knitr::knit_params(readLines(inputFile))
  
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
  
  getInputFun <- function(x) {
    if (is.null(x)) return(shiny::textInput)
    if (x == "radio") return(shiny::radioButtons)
    get(paste0(x, "Input"), asNamespace("shiny"))
  }
  
  paramsUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(lapply(names(pm), function(n) {
      p <- pm[[n]]
      do.call(getInputFun(p$input), shinyArgs(p, ns))
    }))
  }
  
  getParams <- function(values) {
    setNames(lapply(names(values), function(n) {
      if (!is.null(pm[[n]]$input) && pm[[n]]$input == "file") {
        v <- values[[n]]
        if (is.null(v)) pm[[n]]$value else v$datapath
      } else values[[n]]
    }), names(values))
  }
  
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Analysis Configuration"),
      paramsUI("p"),
      shiny::actionButton("go", "Render"),
      shiny::verbatimTextOutput("status")
    ),
    
    server = function(input, output, session) {
      
      params_out <- shiny::callModule(function(input, output, session) {
        shiny::reactive(getParams(shiny::reactiveValuesToList(input)))
      }, "p")
      
      output$status <- shiny::renderText("No report yet.")
      
      shiny::observeEvent(input$go, {
        params <- params_out()
        
        out <- if (is.null(out_path) || identical(out_path, "")) {
          here::here(paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        } else if (is.function(out_path)) {
          out_path(params, inputFile)
        } else if (grepl("\\.html$", out_path, ignore.case = TRUE)) {
          dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
          out_path
        } else {
          dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
          file.path(out_path, paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        }
        
        output$status <- shiny::renderText("Rendering...")
        
        rmarkdown::render(
          input = inputFile,
          params = params,
          output_file = out,
          envir = new.env(parent = globalenv())
        )
        
        shiny::stopApp()
      })
    }
  )
  
  shiny::runApp(app, launch.browser = TRUE)
}