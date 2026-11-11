# get_variable_names


# Creates data.frame using named vectors
# NOTE: check if it can take variable names correctly, like does "tia" have to be in quotes
conditions_vec_to_df <- function(..., override_names = NULL) {
  
  .conditions_vec_lst <- lst(...)
  
  len <- length(.conditions_vec_lst)
  
  if(is.null(override_names)){
    .nm <- names(.conditions_vec_lst)
  } else {
    len_o <- length(override_names)
    if(len_o == len) {
      .nm <- override_names
    } else {
      stop("`override_names` and ... different lengths")
    }
  }
  
  out <- data.frame(
    condition = unlist(.conditions_vec_lst, use.names = FALSE),
    condition_category = rep(.nm, lengths(.conditions_vec_lst))
  )
  
  
  return(out)
  
  
}



# Fetches variable names from whatever data path
get_variable_names <- function(conn, data_path) {
  sql_call <- sprintf("CREATE VIEW data00data AS SELECT * FROM '%s'", data_path)
  
  DBI::dbExecute(conn, sql_call)
  var_cols <- DBI::dbGetQuery(conn, "PRAGMA table_info('data00data')")
  DBI::dbExecute(conn, "DROP VIEW data00data")
  
  return(var_cols)
}


subset_variables <- function(conn, data_path, ..., id_vars = NULL, suffixes) {
  conditions_vec <- paste0("^", c(...), "_", collapse = "|")
  var_cols <- get_variable_names(conn, data_path)
  
  var_cols_out <- var_cols |>
    dplyr::filter(stringr::str_detect(name, conditions_vec) | name %in% id_vars)
  
  if(missing(suffixes)) {
    var_cols_out <- var_cols_out |> pull(name)
  } else {
    .suffixes <- paste0(suffixes, collapse = "|")
    var_cols_out <- var_cols_out |>
      dplyr::filter(strigr::str_detect(name, .suffixes) | name %in% id_vars) |>
      dplyr::pull(name)
  }
  
  return(var_cols_out)
}

view_rows <- function(conn, data_path, rows = 10, view = TRUE) {
  sql_call <- sprintf("CREATE OR REPLACE VIEW data00data AS SELECT * FROM '%s'", data_path)
  DBI::dbExecute(conn, sql_call)
  # NOTE: SEPARATE THESE FUNCTIONS AND maybe don't create view before
  out <- DBI::dbGetQuery(conn, sprintf("SELECT * FROM 'data00data' FETCH FIRST %s ROWS ONLY", rows))
  
  DBI::dbExecute(conn, "DROP VIEW data00data")
  if(view) {
    return(View(out))
  } else {
    return(out)
  }
}
