library(shiny)
library(knitr)
library(rmarkdown)
library(here)

file <- here::here("Scripts", "test.Rmd")
dir.create(here("Output"), recursive = TRUE, showWarnings = FALSE)

params_meta <- knitr::knit_params(readLines(file))

ui <- fluidPage(
  titlePanel("Analysis Configuration"),
  lapply(names(params_meta), function(n) {
    textInput(inputId = n, label = n, value = as.character(params_meta[[n]]$value))
  }),
  actionButton("render", "Render (save)"),
  verbatimTextOutput("status")
)

server <- function(input, output, session) {
  output$status <- renderText("No report yet.")
  
  observeEvent(input$render, {
    params <- lapply(names(params_meta), function(n) input[[n]])
    names(params) <- names(params_meta)
    
    out_path <- here::here(
      "Output",
      paste0(tools::file_path_sans_ext(basename(file)),
             "_",
             params$upper_peak_window,
             ".html")
    )
    
    rmarkdown::render(
      input = file,
      params = params,
      output_file = out_path,
      envir = new.env(parent = globalenv())
    )
    
    output$status <- renderText(paste0("Rendered and saved to:\n", out_path))
  })
}

params <- shiny::runApp(
  list(ui = ui, server = server),
  launch.browser = if (requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) rstudioapi::viewer else TRUE
)