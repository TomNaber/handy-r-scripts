build_plot_config <- function(output_type = c("paper", "poster"), params) {
  output_type <- match.arg(output_type) # check if output type matches options else return
  
  # Construct list with values and theme options to construct the plot components with
  spec <- list(
    # Set universal theme options
    universal = list(
      theme =
        theme_few() +
        theme(
          legend.title = element_blank(),
          legend.position = "inside",
          legend.justification.inside = c(0, 1),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA),
          legend.key.height = unit(2, "lines"),
          panel.border = element_part_rect(
            colour = "grey30",
            side = "lb",
            linewidth = 0.5
          )
        )
    ),
    
    # Set paper style
    paper = list(
      # Set values to pass through later
      base_path = here::here(
        "Output",
        "Figures",
        if (isTRUE(params$telolength))
          paste0(params$hearing_var, "_telolength")
        else
          params$hearing_var
      ),
      base_text_size = 8,
      signif_size = 14,
      
      # Set values and protoobjects for plot construction
      legend_guide = guides(
        colour = guide_legend(
          override.aes = list(size = 1.25),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm"),
          label.theme = element_markdown(
            "sans",
            size = 9,
            margin = margin(l = 3, t = 1.5, unit = "pt")
          )
        )
      ),
      
      text_theme = theme(
        axis.text.x = element_markdown("sans", size = 8),
        axis.text.y = element_markdown("sans", size = 8)
      ),
      
      title_theme = theme(
        axis.title.x = element_markdown("sans", size = 9, lineheight = 1.1),
        axis.title.y = element_markdown("sans", size = 9, lineheight = 1.1)
      )
    ),
    
    age_rate_x = function() {
      paste0(
        "Age<br><span style='font-size:",
        spec[[output_type]]$base_text_size,
        "pt; color:#4D4D4D'>(Years)</span>"
      )
    },
    
    age_rate_y = function() {
      paste0(
        if (identical(params$hearing_var, "FI_high"))
          "High "
        else
          "Low ",
        "Fletcher Index<br><span style='font-size:",
        spec[[output_type]]$base_text_size,
        "pt; color:#4D4D4D'>(dB HL)</span>"
      )
    },
    
    age_rate_y_der = function() {
      paste0(
        "Rate of HL<br><span style='font-size:",
        spec[[output_type]]$base_text_size,
        "pt; color:#4D4D4D'>(dB HL/Year)</span>"
      )
    },
    
    bioage_x = function() {
      paste0(
        "Age Group<br><span style='font-size:",
        spec[[output_type]]$base_text_size,
        "pt; color:#4D4D4D'>(Years)</span>"
      )
    },
    
    bioage_y = function() {
      paste0(
        "Additional Predictive Capacity<br><span style='font-size:",
        spec[[output_type]]$base_text_size,
        "pt; color:#4D4D4D'>(Δ MSE (dB HL)<sup>2</sup>)</span>"
      )
    },
    
    bioage_color_values = function() {
      if (isTRUE(params$telolength)) {
        c(
          "age" = "#000000",
          "age, hd_log" = "#256DAB",
          "age, kdm" = "#059748",
          "age, phenoage" = "#CB2027",
          "age, telolength" = "#7B3A96"
        )
      } else {
        c(
          "age" = "#000000",
          "age, hd_log" = "#256DAB",
          "age, kdm" = "#059748",
          "age, phenoage" = "#CB2027"
        )
      }
    },
    
    bioage_fill_values = function() {
      if (isTRUE(params$telolength)) {
        c(
          "age" = "#8C8C8C",
          "age, hd_log" = "#88BDE6",
          "age, kdm" = "#90CD97",
          "age, phenoage" = "#F07E6E",
          "age, telolength" = "#B276B2"
        )
      } else {
        c(
          "age" = "#8C8C8C",
          "age, hd_log" = "#88BDE6",
          "age, kdm" = "#90CD97",
          "age, phenoage" = "#F07E6E"
        )
      }
    },
    
    bioage_labels = function() {
      if (isTRUE(params$telolength)) {
        c("Age",
          "Age + HD",
          "Age + KDM",
          "Age + PhenoAge",
          "Age + Telolength")
      } else {
        c("Age", "Age + HD", "Age + KDM", "Age + PhenoAge")
      }
    },
    
    advance_bioage_x = function() {
      paste0(
        "Age Acceleration<br><span style='font-size:",
        spec[[output_type]]$base_text_size,
        "pt; color:#4D4D4D'>(PhenoAge – Age (Years))</span>"
      )
    },
    
    advance_bioage_y = function() {
      paste0(
        "Age Group<br><span style='font-size:",
        spec[[output_type]]$base_text_size,
        "pt; color:#4D4D4D'>(Years)</span>"
      )
    },
    
    biomarkers_y = function() {
      paste0(
        "Additional Predictive Capacity<br><span style='font-size:", spec[[output_type]]$base_text_size, "pt; color:#4D4D4D'>(Δ MSE (dB HL)<sup>2</sup>)</span>"
      )
    }
  )
  
  # Pass through the config containing the plot and passthrough variables
  make_config <- function() {
    # Construct the plot
    add_preset <- function(p,
                           preset = c("bioage", "age_rate", "age_rate_der", "advance_bioage", "biomarkers")) {
      preset <- match.arg(preset) # check if preset matches
      
      # Add protoobjects from the correct spec to append to all plots
      p <- p + spec$universal$theme
      p <- p + spec[[output_type]]$legend_guide
      p <- p + spec[[output_type]]$text_theme
      p <- p + spec[[output_type]]$title_theme
      
      # Add protoobjects that differ between plots
      p <- switch(
        preset,
        
        bioage = {
          p +
            scale_color_manual(
              values = spec$bioage_color_values(),
              labels = spec$bioage_labels()
            ) +
            scale_fill_manual(
              values = spec$bioage_fill_values(),
              labels = spec$bioage_labels()
            ) +
            labs(x = spec$bioage_x(), y = spec$bioage_y())
        },
        
        age_rate = {
          p +
            labs(y = spec$age_rate_y())
        },
        
        age_rate_der = {
          p +
            labs(x = spec$age_rate_x(),
                 y = spec$age_rate_y_der())
        },
        
        advance_bioage = {
          p +
            labs(x = spec$advance_bioage_x(),
                 y = spec$advance_bioage_y())
        },
        
        biomarkers = {
          p + 
            labs(y = spec$biomarkers_y())
        }
      )
      
      p
    }
    
    # Pass through variables to p_config
    list(
      base_path = spec[[output_type]]$base_path,
      base_text_size = spec[[output_type]]$base_text_size,
      signif_size = spec[[output_type]]$signif_size,
      preset = add_preset
    )
  }
  
  make_config()
}