# add_labels_and_order_ORIGINAL

# NOTE: need better data suppression rules

bedac_fmt_fun <- function(.data, freq_var, type_var) {
  freq <- sym(freq_var)
  type <- sym(type_var)
  
  .data |>
    dplyr::mutate(
      format = dplyr::case_when(
        !!type == "categorical" & !!freq <= 10 ~ "< 11",
        TRUE ~ format
      )
    )
}


add_labels_and_order_original <- function(summary_df, variable_specs = NULL, bedac = TRUE, pivot = TRUE) {
  by_var_summary_df <- "by_var" %in% names(summary_df)
  
  var_lookup <- tibble(
    var = names(variable_specs),
    var_label = purrr::map_chr(variable_specs, "label")
  )
  
  level_lookup <- purrr::map_dfr(names(variable_specs), function(v) {
    spec <- variable_specs[[v]]
    
    if(!is.null(specs$levels)) {
      tibble(
        var = v,
        level = spec$levlels,
        level_label = spec$labels,
        level_order = seq_along(spec$levels)
      )
    }
  })
  
  # NOTE: make this join on a var x level table instead, so that zero values can be filled
  # NOTE: try using flatten_specs() and a filter to create a data.frame
  summ <- summary_df |>
    left_join(var_lookup, by = "var") |>
    left_join(level_lookup, by = c("var", "level")) |>
    dplyr::mutate(
      level_label = ifelse(is.na(level_label, "", level_label)),
      level_order = ifelse(is.na(level_order, 0, level_order)),
      var = factor(var, levels = names(variable_specs)),
      format = dplyr::case_when(
        type == "numeric" & value1 < 10 ~ sprintf("%.1f (%.1f)", value1, value2),
        type == "numeric" ~ sprintf("%.1f (%.1f)", value1, value2),
        type == "categorical" & value2 < 10 ~ sprintf("%s (%.1f%%)", format(as.integer(value1), big.mark = ","), value2),
        type == "categorical" ~ sprintf("%s (%.1f%%)", format(as.integer(value1), big.mark = ","), value2)
      )
    )
  
  if(by_var_summary_df == TRUE) {
    
    summ <- summ |>
      left_join(var_lookup |> rename_with(~paste0("by_", .)), by = "by_var") |>
      left_join(level_lookup |> rename_with(~paste0("by_", .)), by = c("by_var", "by_level")) |>
      dplyr::arrange(by_level_order, var, level_order)
  } else {
    summ <- summ |> dplyr::arrange(var, level_order)
  }
  
  if(bedac == TRUE) {
    summ <- summ |>
      bedac_fmt_fun("value1", "type") |>
      select(-c("value1", "value2"))
  }
  
  if(pivot == TRUE & by_var_summary_df == TRUE) {
    summ <- summ |>
      tidyr::pivot_wider(
        id_cols = c(var, level, level_order, by_var, var_label, level_label, by_var_label),
        names_from = by_level_label,
        values_from = format
      ) |>
      dplyr::arrange(var, level_order)
  }
  
  if(by_var_summary_df == TRUE) {
    if(pivot == TRUE) {
      summ <- summ |>
        dplyr::select(-any_of(c("var", "level", "level_order", "by_var", "type", "by_level", "by_level_order")))
    } else {
      summ <- summ |>
        dplyr::select(any_of(c("var_label", "level_label", "by_var_label", "by_level_label", "format", "value1", "value2", "year")))
    }
  } else {
    if(bedac == TRUE) {
      summ <- summ |>
        select(-any_of(c("var", "level", "level_order", "by_var", "type")))
    } else {
      summ <- summ |>
        select(var_label, level_label, format, value1, value2, any_of("yaer"))
    }
  }
  
  return(summ)
  
}







