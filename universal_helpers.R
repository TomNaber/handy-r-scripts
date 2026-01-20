save_and_plot <- function(plot,
                          path = "",
                          device = c("agg_png", "agg_tiff"),
                          width = 4,
                          height = 4,
                          res = 600,
                          units = "in",
                          background = "white",
                          save = FALSE) {
  #' Plots and saves figure with ragg device
  #'
  #' Creates a dir if not saved in the root.
  #' Creates a blank png, prints the plot, and stores it by closing the device.
  #' If the file is a tiff, creates an additional tiff.
  #' @param plot A data.table to be reformatted.
  #' @param path A path with filename without extension.
  #' @param device Sets filetype to store.
  #' @param width Set plot width.
  #' @param height Set plot height.
  #' @param res Set resolution in pixels per inch.
  #' @param units Set unit of height and width.
  #' @param background Set background, useful for transparency.
  #' @param save Set whether to save the file.
  #' @return A plot.
  
  device <- match.arg(device)
  
  temp_base <- tempfile("_plot_")
  temp_png <- paste0(temp_base, ".png")
  temp_tiff <- paste0(temp_base, ".tiff")
  
  write_image <- function(device, file_path) {
    device(
      file_path,
      width = width,
      height = height,
      res = res,
      units = units,
      background = background
    )
    print(plot)
    invisible(dev.off())
  }
  
  write_image(ragg::agg_png, temp_png)
  
  if (device == "agg_tiff") {
    write_image(ragg::agg_tiff, temp_tiff)
  }
  
  if (save) {
    figure_folder <- dirname(knitr::fig_path(options = list(fig.path = path)))
    
    if (figure_folder != ".") {
      dir.create(figure_folder,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
    
    if (device == "agg_png") {
      target_path = knitr::fig_path(suffix = ".png", options = list(fig.path = path))
      file.copy(temp_png, target_path, overwrite = TRUE)
    } else if (device == "agg_tiff") {
      target_path = knitr::fig_path(suffix = ".tiff", options = list(fig.path = path))
      file.copy(temp_tiff, target_path, overwrite = TRUE)
    }
  }
  
  knitr::include_graphics(temp_png, rel_path = FALSE)
}

prepare_statistics_df <- function(data,
                                  pwc,
                                  response_variable,
                                  dodge_width = 0.9,
                                  group_var_primary,
                                  group_var_secondary,
                                  other_group_vars) {
  group_levels <- seq(1:length(unique(data[[group_var_secondary]])))
  dodge_width <- dodge_width
  
  group_offsets <- setNames(
    seq_along(group_levels) * 0 - mean(seq_along(group_levels)) * 1 + seq_along(group_levels),
    group_levels
  ) * (dodge_width / length(group_levels))
  
  pwc %>%
    mutate(
      group_num = as.numeric(factor(.data[[group_var_primary]])),
      xmin = group_num + group_offsets[[1]],
      xmax = group_num + group_offsets[[2]]
    ) %>%
    left_join(
      data %>%
        group_by(across(all_of(
          c(group_var_primary, other_group_vars, group_var_secondary)
        ))) %>%
        summarise(
          mean_y = mean(.data[[response_variable]], na.rm = TRUE),
          se = sd(.data[[response_variable]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[response_variable]]))),
          n = sum(!is.na(.data[[response_variable]])),
          .groups = "drop"
        ) %>%
        mutate(y = mean_y + qt(0.975, df = n - 1) * se) %>%
        group_by(across(all_of(
          c(group_var_primary, other_group_vars)
        ))) %>%
        slice_max(
          order_by = mean_y,
          n = 1,
          with_ties = FALSE
        ) %>%
        ungroup() %>%
        select(all_of(
          c(group_var_primary, other_group_vars)
        ), y),
      by = c(group_var_primary, other_group_vars)
    ) %>%
    filter(p.value < 0.05)
}
