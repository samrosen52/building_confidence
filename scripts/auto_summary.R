# auto_summary

# NOTE: add an auto_summary_brief to output just the column totals
# NOTE: is there any way to mash it all together instead of calling each thing at once?
# NOTE:change con to conn in these functions



auto_summary_categorical_str <- function(var, by = NULL, dir = "row", weights = NULL) {
  ""
}


"
WITH var_table1 AS
  (
  SELECT 'var' AS var, var AS level, 'by_var' AS by_var, by_var AS by_level, COUNT(*) AS value1, SUM(weights) AS value1_weighted
  FROM table
  GROUP BY var, level, by_var, by_level
  )
  SELECT
"




"
WITH state_table1 AS
  (
  SELECT 'state' AS var, state AS level, 'alzheim_BIN' AS by_var, alzheim_BIN as by_level, COUNT(*) AS value1, SUM(weights) AS value1_weighted
  FROM table
  GROUP BY var, level, by_var, by_level
  ),
  
WITH alzheim_BIN_table1 AS
  (
  SELECT 'alzheim_BIN' AS var, alzheim_BIN AS level, 'state' AS
  )

"

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
       ) ",
  var, var, table, var
)



library(DBI)
library(duckdb)
con <- DBI::dbConnect(duckdb::duckdb())
auto_summary <- function(conn, table, vars, by = NULL, dir = "row", variable_specs = NULL, weights = NULL, overall = TRUE, force_type = NULL) {
  
  if(is.null(force_type)) {
    var_type <- sapply(vars, get_var_type, variable_specs = variable_specs)
  } else {
    var_type <- rep(force_type, length.out = length(vars))
  }
  
  q <- function(x) DBI::dbQuoteIdentifier(conn, x)
  
  # .vars <- DBI::dbQuoteIdentifier(conn, vars)
  
  num_vars <- vars[var_type == "numeric"]
  cat_vars <- vars[var_type == "categorical"]
  
  
  by_expr <- if(is.null(by)) "NULL::VARCHAR" else q(by)
  by_var <- if(is.null(by)) "NULL::VARCHAR" else DBI::dbQuoteString(conn, by)
  w_expr <- if(is.null(weights)) "1.0" else q(weights)
  
  partition <- if(is.null(by)) "PARTITION BY var"
  else if(dir == "row") "PARTITION BY var, level"
  else "PARTITION BY var, by_level"
  
  if(is.null(weights)){
    mean_sql <- "AVG(val)"
    
    # NOTE: Check this:
    sd_sql <- "STDDEV_SAMP(val)"
  } else {
    
    # NOTE: Check this:
    mean_sql <- "SUM(val*__w)"
    
    # NOTE: Check this
    sd_sql <- "SQRT((SUM(val * val * __w) - POWER(SUM(val * __w), 2) / SUM(__w)) /
                (SUM(__w) - SUM(__w * __w) / SUM(__w)))"
  }
  
  pieces <- c()
  
  if(length(num_vars)) {
    cols <- q(num_vars)
    casts <- paste0("CAST(", cols, " AS DOUBLE) AS ", cols, collapse = ", ")
    pieces <- c(pieces, sprintf("(
      SELECT var, NULL::VARCHAR AS level, %s AS by_var, __by AS by_level,
             %s AS value1, %s AS value2, 'numeric' AS type
      FROM (
        UNPIVOT (SELECT %s, %s AS __by, %s AS __w FROM '%s')
        ON %s INTO NAME var VALUE val
      )
      GROUP BY var, __by
    )", by_var, mean_sql, sd_sql, casts, by_expr, w_expr, table,
                                paste(cols, collapse = ", ")))
  }
  
  
  if (length(cat_vars)) {
    cols   <- q(cat_vars)
    casts  <- paste0("CAST(", cols, " AS VARCHAR) AS ", cols, collapse = ", ")
    pieces <- c(pieces, sprintf("(
      WITH long AS (
        UNPIVOT (SELECT %s, %s AS __by, %s AS __w FROM '%s')
        ON %s INTO NAME var VALUE val
      ),
      agg AS (
        SELECT var, val AS level, __by AS by_level,
               COUNT(*) AS value1, SUM(__w) AS w_sum
        FROM long
        GROUP BY var, val, __by
      )
      SELECT var, level, %s AS by_var, by_level,
             CAST(value1 AS DOUBLE) AS value1,
             w_sum / SUM(w_sum) OVER (%s) * 100.0 AS value2,
             'categorical' AS type
      FROM agg
    )", casts, by_expr, w_expr, table, paste(cols, collapse = ", "),
                                by_var, partition))
  }
  
  
  if(overall) {
    pieces <- c(pieces, sprintf("(
      SELECT 'Overall' AS var, NULL::VARCHAR AS level, %s AS by_var,
             __by AS by_level, CAST(COUNT(*) AS DOUBLE) AS value1,
             SUM(__w) / SUM(SUM(__w)) OVER () * 100.0 AS value2,
             'categorical' AS type
      FROM (SELECT %s AS __by, %s AS __w FROM '%s')
      GROUP BY __by
    )", by_var, by_expr, w_expr, table))
  }
  sql <- paste(pieces, collapse = "\nUNION ALL\n")
  # return(sql)
  
  dbGetQuery(conn, sql)
  # vars_q <- DBI::dbQuoteIdentifier(conn, vars)
  # 
  # casts <- paste0("CAST(")
}


test_summ <- auto_summary(conn = con, vars = "alzheim_BIN", by = "state", table = "data/sim_data.parquet", variable_specs = variable_specs)



add_pct_ci <- function(x, dir = "row", conf = 0.95) {
  is_cat <- x$type == "categorical"
  key <- if(all(is.na(x$by_var))) x$var
    else if(dir == "row") paste(x$var, x$level, sep = "|")
    else paste(x$var, x$by_level, sep = "|")
  
  W <- ave(x$w_sum, key, FUN = sum)
  W2 <- ave(x$w2_sum, key, FUN = sum)
  N <- ave(x$n, key, FUN = sum)
  
  
}

for(state in unique(test_summ$by_level)){
  
}





