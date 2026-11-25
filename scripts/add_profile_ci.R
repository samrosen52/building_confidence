# add_profile_ci


get_weighted_sums <- function(con, path, group_vars, outcome, weight = NULL) {
  q   <- function(x) as.character(dbQuoteIdentifier(con, x))
  grp <- paste(q(group_vars), collapse = ", ")
  y   <- q(outcome)
  if (is.null(weight)) {
    w          <- "1"
    where_weight <- ""
  } else {
    w          <- q(weight)
    where_weight <- glue::glue("AND {w} IS NOT NULL")
  }
  src <- as.character(dbQuoteString(con, path))
  
  sql <- glue::glue("
    SELECT {grp} AS var,
      '{y}' as by_var,
      COUNT(*)::DOUBLE              AS n,
      SUM({y})::DOUBLE              AS sum_y,
      SUM({w})::DOUBLE              AS sum_w,
      SUM({y} * {w})::DOUBLE        AS sum_wy,
      SUM({w} * {w})::DOUBLE        AS sum_w2,
      SUM({y} * {w} * {w})::DOUBLE  AS sum_w2y,
      SUM({y} * {y} * {w} * {w})::DOUBLE AS sum_w2y2
    FROM read_parquet({src})
    WHERE {y} IS NOT NULL {where_weight}
    GROUP BY {grp}
  ")
  
  as_tibble(dbGetQuery(con, sql))
}



add_weighted_stats <- function(d) {
  d |>
    mutate(
      p_w   = sum_wy / sum_w,                       # the weighted mean (a proportion if y is 0/1)
      var_p = pmax((sum_w2y2 - 2 * p_w * sum_w2y + p_w^2 * sum_w2) / sum_w^2, 0),
      se_p  = sqrt(var_p),
      n_eff = sum_w^2 / sum_w2,
      deff  = n * sum_w2 / sum_w^2,
      y_eff = p_w * n_eff,
      n_var = if_else(var_p > 0 & p_w > 0 & p_w < 1,
                      p_w * (1 - p_w) / var_p,       # n that matches the design-based variance
                      n_eff)
    )
}


add_profile_ci <- function(d, type = c("prop", "mean"), level = 0.95,
                           match_var = FALSE) {
  type <- match.arg(type)
  
  if (type == "prop") {
    stopifnot(all(d$p_w >= 0 & d$p_w <= 1, na.rm = TRUE))
    n_use <- if (match_var) d$n_var else d$n_eff
    y_use <- d$p_w * n_use
    ci <- vapply(seq_len(nrow(d)),
                 function(i) profile_ci(y_use[i], n_use[i], level),
                 numeric(2))
    d$ci_lo <- ci[1, ]
    d$ci_hi <- ci[2, ]
  } else {
    cut  <- qchisq(level, 1) / 2
    half <- sqrt(d$var_p * d$n_eff * (exp(2 * cut / d$n_eff) - 1))
    d$ci_lo <- d$p_w - half
    d$ci_hi <- d$p_w + half
  }
  d
}

get_weighted_sums(con, 'data/sim_data.parquet', "variable_cat", "alzheim_BIN") |>
  add_weighted_stats() |>
  add_profile_ci("prop")
  # dplyr::select(var, by_var, sum_y, sum_w, p_w, ci_lo, ci_hi)

get_weighted_sums(con, 'data/sim_data.parquet', "alzheim_BIN", "variable_cat") |>
  add_weighted_stats() |>
  add_profile_ci("mean")
  # dplyr::select(var, by_var, sum_w, p_w, ci_lo, ci_hi)

get_weighted_sums(con, 'data/sim_data.parquet', "state", "variable_cat") |>
  add_weighted_stats() |>
  add_profile_ci("prop")
  # dplyr::select(var, by_var, sum_w, p_w, ci_lo, ci_hi)



get_weighted_sums_cat <- function(con, path, group_vars, cat_var, weight = NULL) {
  q   <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  grp <- paste(q(group_vars), collapse = ", ")
  cv  <- q(cat_var)
  src <- as.character(DBI::dbQuoteString(con, path))
  
  if (is.null(weight)) {
    w <- "1"; where_weight <- ""
  } else {
    w <- q(weight); where_weight <- glue::glue("AND {w} IS NOT NULL")
  }
  
  lvls <- DBI::dbGetQuery(con, glue::glue(
    "SELECT DISTINCT {cv} AS lvl FROM read_parquet({src})
     WHERE {cv} IS NOT NULL ORDER BY 1"))$lvl
  
  purrr::map_dfr(lvls, function(l) {
    ind <- glue::glue("(CASE WHEN {cv} = {as.character(DBI::dbQuoteLiteral(con, l))} THEN 1 ELSE 0 END)")
    sql <- glue::glue("
      SELECT {grp},
             COUNT(*)::DOUBLE AS n,
             SUM({ind})::DOUBLE AS sum_y,
             SUM({w})::DOUBLE AS sum_w,
             SUM({ind} * {w})::DOUBLE AS sum_wy,
             SUM({w} * {w})::DOUBLE AS sum_w2,
             SUM({ind} * {w} * {w})::DOUBLE AS sum_w2y,
             SUM({ind} * {w} * {w})::DOUBLE AS sum_w2y2   -- ind^2 = ind
      FROM read_parquet({src})
      WHERE {cv} IS NOT NULL {where_weight}
      GROUP BY {grp}")
    out <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
    out$level <- l
    out
  })
}


get_weighted_sums_cat(con, 'data/sim_data.parquet', "state", "variable_cat") |>
  add_weighted_stats() |>
  add_profile_ci("prop")





# New combined function ---------------------------------------------------

get_table1_sums <- function(con, path, cat_vars = NULL, cont_vars = NULL,
                            by = NULL, weight = NULL, overall = TRUE,
                            percent = c("column", "row")) {
  percent <- match.arg(percent)
  q   <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  src <- as.character(DBI::dbQuoteString(con, path))
  by_name <- if (is.null(by)) "Overall" else by
  
  if (is.null(weight)) { w <- "1"; w_ok <- "" }
  else { w <- q(weight); w_ok <- paste("AND", w, "IS NOT NULL") }
  
  # "by" set = one column per level of `by`; "all" set = Overall column
  by_sets <- list()
  if (!is.null(by)) by_sets[[1]] <- list(
    name = "by", grouped = TRUE,
    expr = paste0("CAST(", q(by), " AS VARCHAR)"),
    filter = paste("AND", q(by), "IS NOT NULL"))
  if (is.null(by) || overall) by_sets[[length(by_sets) + 1]] <- list(
    name = "all", grouped = FALSE, expr = "'Overall'", filter = "")
  
  run <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  
  cat_q <- function(v, s) {
    grp <- paste(c(q(v), if (s$grouped) s$expr), collapse = ", ")
    glue::glue("
      SELECT '{v}' AS var, '{s$name}' AS \"set\",
             CAST({q(v)} AS VARCHAR) AS level, {s$expr} AS by_level,
             COUNT(*)::DOUBLE AS n_cell,
             SUM({w})::DOUBLE AS w_cell,
             SUM({w} * {w})::DOUBLE AS w2_cell
      FROM read_parquet({src})
      WHERE {q(v)} IS NOT NULL {w_ok} {s$filter}
      GROUP BY {grp}")
  }
  
  cont_q <- function(v, s) {
    y   <- q(v)
    grp <- if (s$grouped) paste("GROUP BY", s$expr) else ""
    glue::glue("
      SELECT '{v}' AS var, '{s$name}' AS \"set\",
             CAST(NULL AS VARCHAR) AS level, {s$expr} AS by_level,
             COUNT(*)::DOUBLE AS n,
             SUM({y})::DOUBLE AS sum_y,
             SUM({w})::DOUBLE AS sum_w,
             SUM({w} * {w})::DOUBLE AS sum_w2,
             SUM({y} * {w})::DOUBLE AS sum_wy,
             SUM({y} * {w} * {w})::DOUBLE AS sum_w2y,
             SUM({y} * {y} * {w} * {w})::DOUBLE AS sum_w2y2
      FROM read_parquet({src})
      WHERE {y} IS NOT NULL {w_ok} {s$filter}
      {grp}")
  }
  
  cat_out <- if (length(cat_vars)) {
    dplyr::bind_rows(lapply(cat_vars, function(v)
      dplyr::bind_rows(lapply(by_sets, function(s) run(cat_q(v, s)))))) |>
      dplyr::mutate(den_key = if (percent == "column") by_level
                    else dplyr::if_else(set == "by", level, by_level)) |>
      dplyr::group_by(var, set, den_key) |>
      dplyr::mutate(n = sum(n_cell), sum_w = sum(w_cell), sum_w2 = sum(w2_cell)) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        var, level,
        by_var = dplyr::if_else(set == "by", by_name, "Overall"),
        by_level, type = "cat",
        n, sum_y = n_cell, sum_w, sum_w2,
        sum_wy = w_cell, sum_w2y = w2_cell, sum_w2y2 = w2_cell)  # indicator: y^2 = y
  }
  
  cont_out <- if (length(cont_vars)) {
    dplyr::bind_rows(lapply(cont_vars, function(v)
      dplyr::bind_rows(lapply(by_sets, function(s) run(cont_q(v, s)))))) |>
      dplyr::mutate(by_var = dplyr::if_else(set == "by", by_name, "Overall"),
                    type = "cont") |>
      dplyr::select(var, level, by_var, by_level, type, n, sum_y, sum_w, sum_w2,
                    sum_wy, sum_w2y, sum_w2y2)
  }
  
  dplyr::bind_rows(cat_out, cont_out) |>
    dplyr::arrange(match(var, c(cat_vars, cont_vars)), level,
                   by_level != "Overall", by_level)
}

add_profile_ci <- function(d, level = 0.95, match_var = FALSE) {
  is_prop <- d$type == "cat"
  d$ci_lo <- NA_real_
  d$ci_hi <- NA_real_
  
  if (any(is_prop)) {                       # binomial profile (uniroot)
    p  <- pmin(pmax(d$p_w[is_prop], 0), 1)  # clamp rounding overshoot
    nn <- if (match_var) d$n_var[is_prop] else d$n_eff[is_prop]
    ci <- vapply(seq_along(p),
                 function(i) profile_ci(p[i] * nn[i], nn[i], level),
                 numeric(2))
    d$ci_lo[is_prop] <- ci[1, ]
    d$ci_hi[is_prop] <- ci[2, ]
  }
  
  if (any(!is_prop)) {                      # normal profile, closed form
    i    <- !is_prop
    cut  <- qchisq(level, 1) / 2
    half <- sqrt(d$var_p[i] * d$n_eff[i] * (exp(2 * cut / d$n_eff[i]) - 1))
    d$ci_lo[i] <- d$p_w[i] - half
    d$ci_hi[i] <- d$p_w[i] + half
  }
  d
}

get_table1_sums(con, 'data/sim_data.parquet', cat_vars = "variable_cat", cont_vars = c("alzheim_BIN", "variable_cat"), by = "state", overall = FALSE) |>
  add_weighted_stats() |>
  add_profile_ci() |>
  dplyr::select(var, level, by_var, by_level, type, n, sum_y, sum_w, p_w, ci_lo, ci_hi) |>
  View()





