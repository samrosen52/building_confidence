# add_labels_and_order


create_small_vs <- function(vars_of_interest, variable_specs) {
  
  if(!is.data.frame(variable_specs)) {
    vs_temp <- variable_specs[names(variable_specs) %in% vars_of_interest]
    vs_small <- flatten_specs(vs_temp)
    # stop("`variable_specs` must be data.frame.\nUse flatten_specs() to standardize variable_specs.")
  } else {
    vs_small <- variable_specs |> dplyr::filter(var %in% vars_of_interest)
  }
  
  return(vs_small)
}


join_small_vs <- function(summary_df, variable_specs) {
  
  by_var_summary_df <- "by_var" %in% names(summary_df)
  
  left_join(variable_specs, summary_df, by = c("var", "level"))
  
}


flatten_specs(variable_specs)

create_small_vs(c("state", "pop_total"), flatten_specs(variable_specs))

add_labels <- function(summary_df, variable_specs = NULL) {
  
  var_names <- c(unique(summary_df$var), unique(summary_df$by_var))
  


  
  vs_flat <- flatten_specs()
  
  return(summ)
  
}