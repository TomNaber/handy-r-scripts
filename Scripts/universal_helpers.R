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

# Axis config function allows clean arguments and formats in HTML to save a specification sheet with axis info. Intended to be used

define_axis <- function(
    config = NULL,
    axis = c("x", "y"),
    type = c("continuous", "discrete"),
    breaks = NULL,
    limits = NULL,
    name = NULL,
    unit = NULL
) {
  axis <- match.arg(axis)
  type <- match.arg(type)
  
  # add label with formatted unit if unit is present, otherwise pass through label (whether populated or NULL).
  name <- if (!is.null(unit) && !is.null(theme)) {
    paste0(
      name,
      "<br><span style='font-size:",
      config$text_size_primary,
      "pt; color:",
      config$axis_unit_color,
      "'>(",
      unit,
      ")</span>"
    )
  } else if (!is.null(unit)) {
    paste0(
      name,
      "<br><span style='font-size:",
      "8",
      "pt; color:",
      "#4D4D4D",
      "'>(",
      unit,
      ")</span>"
    )
  } else {
    name
  }
  
  list(
    axis = axis,
    type = type,
    name = name,
    breaks = breaks,
    limits = limits
  )
}


# Axis builder function takes x and/or y axes of any type and constructs ggplot scale protoobjects with breaks and a name and a coord_cartesian object with limits.

build_axes <- function(x = NULL, y = NULL) {
  specs <- list(x, y)
  scales <- list()
  xlim <- NULL
  ylim <- NULL
  
  for (spec in specs) {
    # skip loop to next if the axis is NULL
    if (is.null(spec)) next
    
    # switch to correct scale function call depending on specs
    construct_format <- switch(paste(spec$axis, spec$type, sep = "_"),
                               x_continuous = scale_x_continuous,
                               y_continuous = scale_y_continuous,
                               x_discrete   = scale_x_discrete,
                               y_discrete   = scale_y_discrete
    )
    
    # overwrite xlim and ylim from NULL to limits if they are specified
    if (!is.null(spec$limits) && spec$axis == "x") {
      xlim <- spec$limits
    } else if (!is.null(spec$limits) && spec$axis == "y") {
      ylim <- spec$limits
    }
    
    # define arguments to pass to scale
    args <- list()
    if (!is.null(spec$name)) args$name <- spec$name
    if (!is.null(spec$breaks)) args$breaks <- spec$breaks
    if (spec$type == "continuous") args$labels <- scales::label_number(drop0trailing = TRUE)
    
    # construct scale function with arguments and append to list of scales
    scales <- c(scales, do.call(construct_format, args))
  }
  
  # append coord_cartesian object if any limits are provided
  if (!is.null(xlim) || !is.null(ylim)) {
    scales <- c(
      scales,
      coord_cartesian(xlim = xlim, ylim = ylim)
    )
  }
  
  scales
}


# Color builder function ignores NULL items that are passed and constructs colors and fills with shared labels.

build_color_scale <- function(
    colors = NULL,
    fill_colors = NULL,
    labels = NULL,
    breaks = NULL
) {
  color_scales <- list()
  
  if (!is.null(colors) && !is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_color_manual(
        values = colors,
        labels = labels,
        breaks = breaks
      )
    )
  }
  
  if (!is.null(fill_colors) && !is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_fill_manual(
        values = fill_colors,
        labels = labels,
        breaks = breaks
      )
    )
  }
  
  if (!is.null(colors) && is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_color_manual(
        values = colors,
        labels = labels
      )
    )
  }
  
  if (!is.null(fill_colors) && is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_fill_manual(
        values = fill_colors,
        labels = labels
      )
    )
  }
  
  color_scales
}
