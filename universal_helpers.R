save_and_plot <- function(plot,
                          path,
                          device = "agg_png",
                          width,
                          height,
                          res = 600,
                          units = "in", background = "white") {
  file <- knitr::fig_path(suffix = ".png", options = list(fig.path = path))

  agg_png(
    file,
    width = width,
    height = height,
    res = res,
    units = "in",
    background = background
  )
  
  print(plot)
  invisible(dev.off())
  
  if (device == "agg_tiff") {
    file_tiff <- knitr::fig_path(suffix = ".tiff", options = list(fig.path = path))
    agg_tiff(
      file_tiff,
      width = width,
      height = height,
      res = res,
      units = "in",
      background = background
    )
    
    print(plot)
    invisible(dev.off())
  }
  
  knitr::include_graphics(file)
}