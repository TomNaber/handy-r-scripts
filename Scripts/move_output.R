move_output <- function(input, output) {
  html_file <- {
    dir  <- dirname(input) # take directory of .rmd file
    base <- sub("\\.Rmd$", "", basename(input)) # remove .rmd extension
    base <- gsub(" ", "-", base) # replace spaces by dashes
    file.path(dir, paste0(base, ".html")) # combine dir and .html file path
  }
  
  html_dir  <- {
    dir  <- dirname(input) # take directory of .rmd file
    base <- sub("\\.Rmd$", "", basename(input)) # remove .rmd extension
    base <- gsub(" ", "-", base) # replace spaces by dashes
    file.path(dir, paste0(base, "_files")) # combine dir and .html file path
  }
  
  dir.create(output, recursive = TRUE, showWarnings = FALSE) # make new output dir if it doesn't exist
  
  if (file.exists(html_file)) { # check if html is in .Rmd directory
    file.copy(html_file, file.path(output, basename(html_file)), overwrite = TRUE) # copy HTML file
    unlink(html_file, force = TRUE) # Remove copied HTML file
  } else if (file.exists(here::here(basename(html_file)))) { # check if html is in project root
    file.copy(here::here(basename(html_file)),
              file.path(output, basename(html_file)),
              overwrite = TRUE) # copy HTML file
    unlink(here::here(basename(html_file)), force = TRUE) # Remove copied HTML file
  }
  
  if (dir.exists(html_dir)) {
    unlink(html_dir, recursive = TRUE, force = TRUE) # delete _files folder in .rmd folder if it exists
  } else if (dir.exists(here::here(basename(html_dir)))) {
    unlink(html_dir, recursive = TRUE, force = TRUE) # delete _files folder in project root if it exists
  }
}