smart_knitter <- function(inputFile,
                          out_path = NULL,
                          launch_in_browser = FALSE,
                          auto_close = TRUE) {
  
  pm <- knitr::knit_params(readLines(inputFile)) # Create params
  
  shinyArgs <- function(param, ns) {
    param$inputId <- ns(param$name)
    if (is.null(param$label)) # set label to param name if no name present
      param$label <- param$name
    if (!is.null(param$input) &&
      param$input %in% c("select", "radio")) {
      param$selected <- param$value
      param$value <- NULL
    } # rename value to selected for select or radio inputs
    param$name <- NULL
    param$input <- NULL
    param
  } # Set param label types to pass to UI
  
  getInputFun <- function(x) {
    if (is.null(x))
      return(shiny::textInput)
    if (x == "radio")
      return(shiny::radioButtons)
    get(paste0(x, "Input"), asNamespace("shiny"))
  } # Add string of param UI objects depending on input type
  
  paramsUI <- function(id) {
    ns <- shiny::NS(id) # namespace the ID
    shiny::tagList(lapply(names(pm), function(n) {
      p <- pm[[n]]
      do.call(getInputFun(p$input), shinyArgs(p, ns))
    }))
  } # Construct params UI
  
  getParams <- function(values) {
    setNames(lapply(names(values), function(n) {
      if (!is.null(pm[[n]]$input) && pm[[n]]$input == "file") {
        v <- values[[n]]
        if (is.null(v))
          pm[[n]]$value
        else
          v$datapath
      } else
        values[[n]]
    }), names(values))
  } # Get param values
  
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      theme = bslib::bs_theme(version = 5, bootswatch = "zephyr"),  # set theme
      style = "padding: 1em 2em 1em 2em;",  # trbl app padding
      shiny::titlePanel("Analysis Configuration"), # title
      shiny::div(style = "height: 1em;"), # spacer between title and params
      shiny::fluidRow( # row of app
        shiny::column(12, # column with max width
          paramsUI("p"), # parameter UI
          shiny::div( # single row with both the run and exit buttons spaced by a gap
            style = "gap: 0.5em;width: 100%;",
            shiny::actionButton("go", "Run analysis", class = "btn btn-primary"),
            shiny::actionButton("exit", "Exit")
          ),
          shiny::div(style = "height: 1em;"), # spacer between title and params
          shiny::verbatimTextOutput("status") # status bar 
        )
      ), 
    ), # compile all UI elements
    
    server = function(input, output, session) {
      rv <- shiny::reactiveValues(
        running = FALSE,
        job = NULL,
        start_time = NULL,
        done_time = NULL,
        runtime = NULL,
        out = NULL
      ) # set values to dynamically call upon
      
      status_file <- tempfile(fileext = ".txt") # create file with chunk status updates to read from in separate process
      
      params_out <- shiny::callModule(function(input, output, session) {
        shiny::reactive(getParams(shiny::reactiveValuesToList(input)))
      }, "p") # take param values and make them reactive so any input changes the variable
      
      output$status <- shiny::renderText({ # create status object
        shiny::invalidateLater(100, session) # check for status updates every 100 ms
        
        if (isTRUE(rv$running)) { # if rendering, paste chunk in status
          paste(readLines(status_file, warn = FALSE), collapse = "\n")
        } else if (!is.null(rv$done_time) && isTRUE(auto_close)) { # if done and auto_close enabled, paste save info and countdown
          elapsed <- as.numeric(difftime(Sys.time(), rv$done_time))
          
          if (elapsed >= 5) {
            shiny::stopApp()
          } # stop after 5 seconds
          
          paste0(
            "Rendered in ", rv$runtime, " ", attr(rv$runtime, "units"),
            " and saved to:\n", rv$out, "\nApp will close in ",
            as.character(round(5 - elapsed)), " seconds..."
          )
        } else if (!is.null(rv$out)) { # if done and auto_close disabled, paste save info and exit
          paste0(
            "Rendered in ",
            rv$runtime, " ",
            attr(rv$runtime, "units"),
            " and saved to:\n",
            rv$out
          )
        } else {
          "Ready. "
        } # if not rendering, print that there's no report
      })
      
      shiny::observeEvent(input$go, { # create render loop
        rv$running <- TRUE # upon go, set running to true
        rv$start_time <- Sys.time() # upon go, save start time
        rv$done_time <- NULL # intitialize done time
        rv$runtime <- NULL # initialize total runtime
        
        params <- params_out() # store params as params for filename constructor
        
        out <- if (is.null(out_path) || identical(out_path, "")) { # if no filename, save basename
          here::here(paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        } else if (is.function(out_path)) { # if filename is function, evaluate it
          out_path(params, inputFile)
        } else if (grepl("\\.html$", out_path, ignore.case = TRUE)) { # if filename contains .html, save as full path
          dir.create(dirname(out_path),
                     recursive = TRUE,
                     showWarnings = FALSE)
          out_path
        } else { # if filename does not contain .html, evaluate as folder + basename.html
          dir.create(out_path,
                     recursive = TRUE,
                     showWarnings = FALSE)
          file.path(out_path,
                    paste0(tools::file_path_sans_ext(basename(inputFile)), ".html"))
        }
        
        rv$out <- out # store output path
        writeLines("Starting render…", status_file) # signal button press is registered
        
        rv$job <- callr::r_bg( # render on separate process
          function(inputFile, params, out, status_file) {
            knitr::opts_chunk$set(status = TRUE) # broadcast chunk status
            knitr::opts_hooks$set(status = function(options) {
              writeLines(paste0("Rendering chunk: ", options$label, "…"), status_file)
              options
            }) # write current chunk label to a status file and pass through chunk options
            
            rmarkdown::render(
              input = inputFile,
              params = params,
              output_file = out,
              envir = new.env(parent = globalenv())
            ) # render command
          },
          args = list(
            inputFile = inputFile,
            params = params,
            out = out,
            status_file = status_file
          ) # pass render variables to the subprocess
        )
      })
      
      shiny::observe({
        shiny::invalidateLater(1000, session)
        
        if (isTRUE(rv$running) && !rv$job$is_alive()) {
          rv$running <- FALSE
          rv$runtime <- round(difftime(Sys.time(), rv$start_time), 2)
          rv$done_time <- Sys.time()
        }
      }) # check if still rendering every second
      
      shiny::observeEvent(input$exit, {
        shiny::stopApp()
      }) # if render was running but is now done, set rv$running to FALSE, set runtime (for print) and done_time (for auto close counter)
    }
  )
  
  shiny::runApp(
    app,
    launch.browser = if (isTRUE(launch_in_browser))
      TRUE
    else
      rstudioapi::viewer
  ) # launch app in browser if launch_in_browser else in viewer
}