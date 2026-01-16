move_output <- function(input, output, figure_folder) {
  html_file <- paste0(sub("\\.Rmd$", "", input), ".html") # current html path
  html_dir  <- paste0(sub("\\.Rmd$", "", input), "_files") # current html _files path
  
  output_relative_dir <- substring(output, nchar(here::here()) + 2L)  # output dir created in .Rmd directory
  wrong_output_dir <- file.path(dirname(input), output_relative_dir) # output dir created in .Rmd directory
  figure_relative_dir <- substring(figure_folder, nchar(here::here()) + 2L) # figure dir relative to project root
  wrong_figure_dir <- file.path(dirname(input), figure_relative_dir) # figure dir created in .Rmd directory
  
  dir.create(output, recursive = TRUE, showWarnings = FALSE) # make new output dir if it doesn't exist
  
  if (file.exists(html_file)) { # check if html is in .Rmd directory
    file.copy(html_file, file.path(output, basename(html_file)), overwrite = TRUE) # copy HTML file
    unlink(html_file, force = TRUE) # Remove copied HTML file
    html_with_rmd <- TRUE
  } else if (file.exists(here::here(basename(html_file)))) { # check if html is in project root
    file.copy(here::here(basename(html_file)),
              file.path(output, basename(html_file)),
              overwrite = TRUE) # copy HTML file
    unlink(here::here(basename(html_file)), force = TRUE) # Remove copied HTML file
    html_with_rmd <- FALSE
  } else {
    stop("No HTML file found in the output folder or project root")
  }
  
  if (html_with_rmd)
  {
    if (dir.exists(html_dir)) {
      unlink(html_dir, recursive = TRUE, force = TRUE) # delete _files folder if it exists
    }
  } else {
    if (dir.exists(here::here(basename(html_dir)))) {
      unlink(html_dir, recursive = TRUE, force = TRUE) # delete _files folder if it exists
    }
  }
  
  if (html_with_rmd && dirname(input) != here::here()) { # only move output folder if html is in the .rmd folder and the .rmd is not in the root
    if (dir.exists(wrong_output_dir)) {
      files <- list.files(wrong_output_dir,
                          full.names = TRUE,
                          recursive = TRUE) # list files in wrong output folder
      relative  <- substring(files, nchar(wrong_output_dir) + 2L) # get relative paths
      destinations <- file.path(output, relative) # determine correct paths (recursively)
      invisible(vapply(unique(dirname(destinations)), dir.create, logical(1), showWarnings = FALSE)) # create folders if they don't exist
      file.copy(files, destinations, overwrite = TRUE) # copy files to correct folders
      unlink(wrong_output_dir,
             recursive = TRUE,
             force = TRUE) # delete wrong output folder
    }
    
    if (dir.exists(wrong_figure_dir)) {
      files <- list.files(wrong_figure_dir,
                          full.names = TRUE,
                          recursive = TRUE) # list files in wrong output folder
      relative  <- substring(files, nchar(wrong_figure_dir) + 2L) # get relative paths
      destinations <- file.path(figure_folder, relative) # determine correct paths (recursively)
      invisible(vapply(unique(dirname(destinations)), dir.create, logical(1), recursive = TRUE, showWarnings = FALSE)) # create folders if they don't exist
      file.copy(files, destinations, overwrite = TRUE) # copy files to correct folders
      unlink(wrong_figure_dir,
             recursive = TRUE,
             force = TRUE) # delete wrong output folder
    }
  }
}