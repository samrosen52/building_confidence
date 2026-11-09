# auto_summary_ORIGINAL

# NOTE: add an auto_summary_brief to output just the column totals
# NOTE: is there any way to mash it all together instead of calling each thing at once?
# NOTE:change con to conn in these functions



auto_summary_numeric <- function(conn = NULL, table, var, by = NULL) {
  
  if(is.null(by)) {
    return(
      sprintf(
        "(
        SELECT
          '%2$s' AS var,
          NULL AS level,
          AVG(%2$s) AS value1,
          STDDEV('%2$s') AS value2,
          'numeric' AS type
        FROM '%1$s'
        )",
        table, var
      )
    )
  } else {
    return(
      sprintf(
        "(
        SELECT
          '%2$s' AS var,
          NULL AS level,
          '%3$s' AS by_var,
          %3$s AS by_level,
          AVG(%2$s) AS value1,
          STDDEV(%2$s) AS value2,
          'numeric' AS type
        FROM '%1$s'
        GROUP BY %3$s
        )",
        table, var, by
      )
    )
  }
}


# NOTE: check how NULL can be handled better
auto_summary_numeric_weighted <- function(conn = NULL, table, var, by = NULL, weights = NULL) {
  
  if(is.null(by)) {
    return(
      sprintf(
        "(
        SELECT
          '%2$s' AS var,
          NULL AS level,
          SUM(%2$s * %3$s) / SUM(%3$s) AS value1,
          SQRT((SUM(%2$s * %2$s * %3$s) - POWER(SUM(%2$s * %3$s), 2))
            /(SUM(%3$s) - 1)) AS value2,
          'numeric' AS type
        FROM '%1$s'
        WHERE '%2$s' IS NOT NULL
        )",
        table, var, weights
      )
    )
  } else {
    return(
      sprintf(
        "(
        SELECT
          '%2$s' AS var,
          NULL AS level,
          '%4$s' AS by_var,
          %4$s AS by_level,
          SUM(%2$s * %3$s) / SUM(%3$s) AS value1,
          SQRT((SUM(%2$s * %2$s * %3$s) - POWER(SUM(%2$s * %3$s), 2))
            /(SUM(%3$s) - 1)) AS value2,
          'numeric' AS type
        FROM '%1$s'
        GROUP BY %4$s
        )",
        table, var, weights, by
      )
    )
  }
}


auto_summary_categorical <- function(conn = NULL, table, var, by = NULL, dir = "row") {
  
  if(is.null(by)) {
    return(
      sprintf(
        "(
        SELECT
          '%s' AS var,
          %s AS level,
          COUNT(*) AS value1,
          COUNT(*)/SUM(COUNT(*)) OVER()*100.0 AS value2,
          'categorical' AS type
        FROM '%s'
        GROUP BY %s
        )",
        var, var, table, var
      )
    )
  } else if(dir == "row") {
    return(
      sprintf(
        "(
        WITH t2 AS
          (SELECT '%s' AS var, %s AS level, '%s' AS by_var, %s AS by_level, COUNT(*) AS value1
          FROM '%s'
          GROUP BY var, level, by_var, by_level)
        SELECT var, level, by_var, by_level, value1,
          (value1)/(SUM(value1) OVER (PARTITION by var, level))*100.0 AS value2,
          'categorical' AS type
        FROM t2
        )",
        var, var, by, by, table
      )
    )
    # NOTE: find a way to just detect "r" or "c" as `dir`
  } else if(dir == "column" | dir == "col") {
    return(
      sprintf(
        "
        (
        WITH t1 AS
          (SELECT '%s' AS var, %s AS level, '%s' AS by_var, %s AS by_level, COUNT(*) AS value1
          FROM '%s'
          GROUP BY var, level, by_var, by_level)
        SELECT var, level, by_var, by_level, value1,
          (value1)/(SUM(value1) OVER (PARTITION by by_var, by_level))*100.0 AS value2,
          'categorical' AS type
        FROM t1
        )",
        var, var, by, by, table
      )
    )
  }
}


auto_summary_categorical_weighted <- function(conn = NULL, table, var, by = NULL, dir = "row", weights = NULL) {
  
  if(is.null(by)) {
    return(
      sprintf(
        "(
        SELECT
          '%1$s' AS var,
          %1$s AS level,
          COUNT(*) AS value1,
          SUM(%2$s)/SUM(SUM(%2$s)) OVER()*100.0 AS value2,
          'categorical' AS type
        FROM '%3$s'
        GROUP BY %1$s
        )",
        var, weights, table
      )
    )
  } else if(dir == "row") {
    return(
      sprintf(
        "(
        WITH t2 AS
          (SELECT '%1$s' AS var, %1$s AS level, '%4$s' AS by_var, %4$s AS by_level, COUNT(*) AS value1, SUM(%2$s) AS value1_weighted
          FROM '%3$s'
          GROUP BY var, level, by_var, by_level)
        SELECT var, level, by_var, by_level, value1,
          (value1_weighted)/(SUM(value1_weighted) OVER (PARTITION BY var, level))*100.0 AS value2,
          'categorical' AS type
        FROM t2
        )",
        var, weights, table, by
      )
    )
  } else if(dir == "column" | dir == "col") {
    return(
      sprintf(
        "(
        WITH t1 AS
          (SELECT '%1$s' AS var, %1$s AS level, '%4$s' AS by_var, %4$s AS by_level, COUNT(*) AS value1, SUM(%2$s) AS value1_weighted
          FROM '%3$s'
          GROUP BY var, level, by_var, by_level)
        SELECT var, level, by_var, by_level, value1,
          (value1_weighted)/(SUM(value1_weighted) OVER (PARTITION BY by_var, by_level))*100.0 AS value2,
          'categorical' AS type
        FROM t1
        )",
        var, weights, table, by
      )
    )
  }
}

check_con <- function() {
  .con_exists <- DBI::dbIsValid(con)
}


# NOTE: Maybe variable_specs can be used as a table that sets which variables are pulled?
# NOTE: would need to be distinct from the data_dictionary
auto_summary_ORIGINAL <- function(conn = con, data_path, vars, by = NULL, dir = "row", weights = NULL, variable_specs = NULL, force_type = NULL) {
  
  if(is.null(weights)) {
    sql_list <- lapply(vars, function(var) {
      if(is.null(force_type)) {
        var_type <- get_var_type(var, variable_specs = variable_specs)
      } else {
        var_type <- force_type
      }
      
      switch(
        var_type,
        numeric = auto_summary_numeric_weighted(con, table = data_path, var, by, weights = weights),
        categorical = auto_summary_categorical_weighted(con, table = data_path, var, by, dir, weights = weights)
      )
    })
  } else {
    
    weights <- sprintf('"%s"', weights)
    
    sql_list <- lapply(vars, function(var) {
      if(is.null(force_type)) {
        var_type <- get_var_type(var, variable_specs = variable_specs)
      } else {
        var_type <- force_type
      }
      
      switch(
        var_type,
        numeric = auto_summary_numeric(con, table = data_path, var, by),
        categorical = auto_summary_categorical(con, table = data_path, var, by, dir)
      )
    })
    
  }
  
  sql_full <- paste(sql_list, collapse = "\nUNION ALL\n")
  
  if(!missing(by)) {
    if(is.null(weights)) {
      overall <- sprintf(
        "(
        WITH t1 AS
          (
          SELECT 'Overall' AS var, NULL AS level, '%1$s' AS by_var, %1$s AS by_level, COUNT(*) AS value1
          FROM '%2$s'
          GROUP BY by_level
          )
        SELECT var, level, by_var, by_level, value1,
          (value1)/(SUM(value1) OVER (PARTITION BY by_var))*100.0 AS value2,
          'categorical' AS type
        FROM t1
        )
        \nUNION ALL\n",
        by, data_path
      )
    } else {
      overall <- sprintf(
        "(
        WITH t1 AS
          (
          SELECT 'Overall' AS var, NULL AS level, '%1$s' AS by_var, %1$s AS by_level, COUNT(*) AS value1, SUM(%2$s) AS value1_weighted
          FROM '%3$s'
          GROUP BY by_level
          )
        SELECT var, level, by_var, by_level, value1,
          (value1_weighted)/(SUM(value1_weighted) OVER (PARTITION BY by_var))*100.0 AS value2,
          'categorical' AS type
        FROM t1
        )
        \nUNION ALL\n
        ",
        by, weights, data_path
      )
    }
  } else {
    overall <- sprintf(
      "(
      SELECT 'Overall' AS var, NULL AS level, COUNT(*) AS value1, 100.0 AS value2, 'categorical' AS type
      FROM '%s'
      )
      \nUNION ALL\n
      ",
      data_path
    )
  }
  
  sql_full <- paste0(overall, sql_full)
  
  result <- DBI::dbGetQuery(con, sql_full)
  return(result)
  
}



