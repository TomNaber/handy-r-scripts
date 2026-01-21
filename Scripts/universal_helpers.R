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
