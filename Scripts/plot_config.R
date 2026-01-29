# Plot config can be used to avoid having to readjust repetitive plot arguments and easily reuse or adjust plot arguments.
# The universal_config is applied to all plots.
# The second level is a config for the output type to allow for toggling between poster and paper configs within the same script and not having to have separate scripts for posters. This is automatically merged with universal_config when the p_config is built.
# Whole ggplot theme() objects can also be stored.
# It's possible to store both global defaults (that apply to most plots) with overrides for a plot. E.g., p_config$text_themes may contain axis defaults, whereas p_config$fig1$text_themes may contain text themes specific to fig1.

p_config <- list(
  universal_config = list(
    scale_labels = function(x) {
      sapply(x, function(val) {
        if (val == floor(val)) {
          as.character(val)
        } else {
          format(val, nsmall = 0)
        }
      })
    },
    universal_theme = theme(
      legend.title = element_blank(),
      legend.position = "inside",
      legend.justification.inside = c(0, 1),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.key.height = unit(2, "lines")
    )
  ),
  
  paper = list(),
  
  poster = list(
    base_path = here("Output", "Figures", "Poster"),
  )
)
