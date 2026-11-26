# add_profile_ci_for_import
# add way to calculate for mean

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
      SELECT '{v}' AS var, '{s$name}' AS grp_set,
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
      SELECT '{v}' AS var, '{s$name}' AS grp_set,
             CAST(NULL AS VARCHAR) AS level, {s$expr} AS by_level,
             COUNT(*)::DOUBLE AS n,
             SUM({y})::DOUBLE AS sum_y,
             SUM({w})::DOUBLE AS sum_w,
             SUM({w} * {w})::DOUBLE AS sum_w2,
             SUM({y} * {w})::DOUBLE AS sum_wy,
             SUM({y} * {w} * {w})::DOUBLE AS sum_w2y,
             SUM({y} * {y})::DOUBLE AS sum_y2,
             SUM({y} * {y} * {w})::DOUBLE AS sum_wy2,
             SUM({y} * {y} * {w} * {w})::DOUBLE AS sum_w2y2
      FROM read_parquet({src})
      WHERE {y} IS NOT NULL {w_ok} {s$filter}
      {grp}")
  }
  
  cat_out <- if (length(cat_vars)) {
    dplyr::bind_rows(lapply(cat_vars, function(v)
      dplyr::bind_rows(lapply(by_sets, function(s) run(cat_q(v, s)))))) |>
      dplyr::mutate(den_key = if (percent == "column") by_level
                    else dplyr::if_else(grp_set == "by", level, by_level)) |>
      dplyr::group_by(var, grp_set, den_key) |>
      dplyr::mutate(n = sum(n_cell), sum_w = sum(w_cell), sum_w2 = sum(w2_cell)) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        var, level,
        by_var = dplyr::if_else(grp_set == "by", by_name, "Overall"),
        by_level, type = "cat",
        n, sum_y = n_cell, sum_w, sum_w2,
        sum_wy = w_cell, sum_w2y = w2_cell, sum_y2 = NA_real_, sum_wy2 = NA_real_, sum_w2y2 = w2_cell)  # indicator: y^2 = y
  }
  
  cont_out <- if (length(cont_vars)) {
    dplyr::bind_rows(lapply(cont_vars, function(v)
      dplyr::bind_rows(lapply(by_sets, function(s) run(cont_q(v, s)))))) |>
      dplyr::mutate(by_var = dplyr::if_else(grp_set == "by", by_name, "Overall"),
                    type = "cont") |>
      dplyr::select(var, level, by_var, by_level, type, n, sum_y, sum_w, sum_w2,
                    sum_wy, sum_w2y, sum_y2, sum_wy2, sum_w2y2)
  }
  
  dplyr::bind_rows(cat_out, cont_out) |>
    dplyr::arrange(match(var, c(cat_vars, cont_vars)), level,
                   by_level != "Overall", by_level)
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
                      n_eff),
      var_y = pmax((sum_wy2 - 2 * p_w * sum_wy + p_w^2 * sum_w) / sum_w, 0),
      sd_w = sqrt(var_y)
    )
}




profile_ci <- function(y, n, level = 0.95) {
  if (!is.finite(y) || !is.finite(n) || n <= 0) return(c(NA_real_, NA_real_))
  p_hat <- y / n
  ll    <- function(p) y * log(p) + (n - y) * log1p(-p)
  ll_max <- if (p_hat > 0 && p_hat < 1) ll(p_hat) else 0
  cut   <- qchisq(level, 1) / 2
  f     <- function(p) ll_max - ll(p) - cut
  eps   <- 1e-12
  
  lo <- if (p_hat <= 0) 0 else uniroot(f, c(eps, p_hat), tol = 1e-10)$root
  hi <- if (p_hat >= 1) 1 else uniroot(f, c(p_hat, 1 - eps), tol = 1e-10)$root
  c(lo, hi)
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

library(dplyr)

# 1. Goodness-of-fit Rao-Scott test for a categorical distribution (1 set of rows)
#    x_obs : observed/effective counts or weights per category (e.g., sum_wy or y_eff)
#    p0    : hypothesized null probabilities (summing to 1, matching row order)
#    deff_k: design effect for each category (e.g., d$deff or design-based / SRS var ratio)
rao_scott_gof <- function(x_obs, p0, deff_k = NULL) {
  keep <- is.finite(x_obs) & is.finite(p0) & p0 > 0
  x_obs <- x_obs[keep]
  p0 <- p0[keep] / sum(p0[keep])
  k <- length(p0)
  if (k < 2) return(list(stat_rs1 = NA_real_, p_rs1 = NA_real_,
                         stat_rs2 = NA_real_, p_rs2 = NA_real_, df_rs2 = NA_real_))
  
  N_tot <- sum(x_obs)
  exp_counts <- N_tot * p0
  # Design-weighted Pearson chi-square
  x2_w <- sum((x_obs - exp_counts)^2 / exp_counts)
  
  if (is.null(deff_k)) {
    deff_k <- rep(1, k)
  } else {
    deff_k <- pmax(deff_k[keep], 1e-8)
  }
  
  # Under multinomial null, asymptotic eigenvalues of design covariance scaled by SRS
  # are approximated by (1 - p0) * deff_k across the k - 1 free dimensions
  lam <- (1 - p0) * deff_k
  delta_bar <- mean(lam)
  
  # First-order Rao-Scott (df = k - 1)
  df1 <- k - 1
  stat_rs1 <- x2_w / delta_bar
  p_rs1 <- pchisq(stat_rs1, df = df1, lower.tail = FALSE)
  
  # Second-order Rao-Scott (Satterthwaite df adjustment)
  df2 <- (sum(lam)^2) / sum(lam^2)
  stat_rs2 <- x2_w / delta_bar
  p_rs2 <- pchisq(stat_rs2, df = df2, lower.tail = FALSE)
  
  list(
    x2_weighted = x2_w,
    delta_bar   = delta_bar,
    stat_rs1    = stat_rs1,
    df_rs1      = df1,
    p_rs1       = p_rs1,
    stat_rs2    = stat_rs2,
    df_rs2      = df2,
    p_rs2       = p_rs2
  )
}

# 2. Homogeneity / independence across G groups for a binary/proportion outcome
#    Expects a data frame slice for one variable/level across G groups with columns:
#    sum_w, sum_wy, p_w, var_p, n_eff, deff
rao_scott_homogeneity <- function(g_df) {
  g_df <- g_df |> filter(is.finite(sum_w), sum_w > 0, is.finite(p_w))
  G <- nrow(g_df)
  if (G < 2) {
    return(tibble(
      x2_weighted = NA_real_,
      delta_bar   = NA_real_,
      stat_rs1    = NA_real_,
      p_rs1       = NA_real_,
      stat_rs2    = NA_real_,
      df_rs2      = NA_real_,
      p_rs2       = NA_real_
    ))
  }
  
  # Pooled weighted proportion across groups
  p_pool <- sum(g_df$sum_wy) / sum(g_df$sum_w)
  p_pool_c <- min(max(p_pool, 1e-12), 1 - 1e-12)
  
  # Effective sample size per group (fallback to sum_w if n_eff absent)
  n_g <- if ("n_eff" %in% names(g_df)) g_df$n_eff else g_df$sum_w
  
  # Pearson chi-square comparing group proportions to pooled proportion
  # Using effective counts n_g * p_w and expected n_g * p_pool
  obs_y <- n_g * g_df$p_w
  exp_y <- n_g * p_pool_c
  obs_n <- n_g * (1 - g_df$p_w)
  exp_n <- n_g * (1 - p_pool_c)
  
  x2_w <- sum((obs_y - exp_y)^2 / exp_y + (obs_n - exp_n)^2 / exp_n)
  
  # Group-level design effect relative to SRS variance p_pool*(1-p_pool)/n_g
  srs_var_g <- p_pool_c * (1 - p_pool_c) / pmax(n_g, 1)
  deff_g <- if ("var_p" %in% names(g_df)) {
    pmax(g_df$var_p / pmax(srs_var_g, 1e-12), 1e-8)
  } else if ("deff" %in% names(g_df)) {
    pmax(g_df$deff, 1e-8)
  } else {
    rep(1, G)
  }
  
  # Weight eigenvalues by group size share (1 - n_g / sum(n_g)) * deff_g
  w_g <- 1 - n_g / sum(n_g)
  lam <- pmax(w_g, 1e-8) * deff_g
  delta_bar <- mean(lam)
  
  df1 <- G - 1
  stat_rs1 <- x2_w / delta_bar
  p_rs1 <- pchisq(stat_rs1, df = df1, lower.tail = FALSE)
  
  df2 <- (sum(lam)^2) / sum(lam^2)
  stat_rs2 <- x2_w / delta_bar
  p_rs2 <- pchisq(stat_rs2, df = df2, lower.tail = FALSE)
  
  tibble(
    x2_weighted = x2_w,
    delta_bar   = delta_bar,
    stat_rs1    = stat_rs1,
    p_rs1       = p_rs1,
    stat_rs2    = stat_rs2,
    df_rs2      = df2,
    p_rs2       = p_rs2
  )
}

# 3. Helper to attach Rao-Scott p-values to a summary table `d`
#    Groups by `group_vars` (e.g., c("var_name", "category")) across `by_var` (e.g., "group")
add_rao_scott_p <- function(d, group_vars, by_var) {
  cat_rows <- d |> filter(type == "cat")
  other_rows <- d |> filter(type != "cat" | is.na(type))
  
  rs_tbl <- cat_rows |>
    group_by(across(all_of(group_vars))) |>
    group_modify(~ rao_scott_homogeneity(.x)) |>
    ungroup()
  
  d |>
    left_join(rs_tbl, by = group_vars) |>
    mutate(
      # Continuous/non-categorical rows get NA for Rao-Scott categorical p-values
      across(c(x2_weighted, delta_bar, stat_rs1, p_rs1, stat_rs2, df_rs2, p_rs2),
             ~ if_else(type == "cat", .x, NA_real_))
    )
}



# 1. Design-adjusted one-sample t-test against a hypothesized mean mu0
#    Uses existing p_w and se_p (or sqrt(var_p)) and effective df
weighted_mean_t_test <- function(p_w, var_p, n_eff, mu0 = 0) {
  se_p <- sqrt(pmax(var_p, 0))
  df_eff <- pmax(n_eff - 1, 1)
  
  if (!is.finite(p_w) || !is.finite(se_p) || se_p <= 0) {
    return(tibble(
      diff     = NA_real_,
      se_w     = se_p,
      t_stat   = NA_real_,
      df_eff   = df_eff,
      p_value  = NA_real_
    ))
  }
  
  t_stat <- (p_w - mu0) / se_p
  p_val  <- 2 * pt(abs(t_stat), df = df_eff, lower.tail = FALSE)
  
  tibble(
    diff    = p_w - mu0,
    se_w    = se_p,
    t_stat  = t_stat,
    df_eff  = df_eff,
    p_value = p_val
  )
}

# 2. Design-adjusted multi-group comparison of means (Welch / Satterthwaite F-test)
#    Expects a slice for one variable across G groups with columns: p_w, var_p, n_eff
weighted_means_anova <- function(g_df) {
  g_df <- g_df |>
    filter(is.finite(p_w), is.finite(var_p), var_p > 0)
  
  G <- nrow(g_df)
  if (G < 2) {
    return(tibble(
      mu_pooled = NA_real_,
      q_stat    = NA_real_,
      f_stat    = NA_real_,
      df1       = NA_real_,
      df2_satt  = NA_real_,
      p_f_satt  = NA_real_,
      p_chi2    = NA_real_
    ))
  }
  
  # Precision (inverse-variance) weights for combining group means under H0
  inv_var <- 1 / g_df$var_p
  w_sum   <- sum(inv_var)
  mu_pool <- sum(inv_var * g_df$p_w) / w_sum
  
  # Between-group Wald / Cochran Q statistic: sum((mu_g - mu_pool)^2 / var_g)
  q_stat <- sum(inv_var * (g_df$p_w - mu_pool)^2)
  
  # Welch-Satterthwaite denominator & degrees of freedom for unequal variances/deffs
  df1 <- G - 1
  h_g <- inv_var / w_sum
  denom_satt <- sum(((1 - h_g)^2) / pmax(g_df$n_eff - 1, 1))
  df2_satt <- if (denom_satt > 0) df1 / (3 * denom_satt) else Inf
  
  # Welch's F-like scaling: accounts for variance heterogeneity across groups
  # (equivalent to Cochran Q / df1 when variances are equal)
  lambda_term <- (2 * (G - 2) / (G^2 - 1)) * sum(((1 - h_g)^2) / pmax(g_df$n_eff - 1, 1))
  f_stat <- q_stat / (df1 * (1 + lambda_term))
  
  p_f_satt <- if (is.finite(df2_satt) && df2_satt > 0) {
    pf(f_stat, df1 = df1, df2 = df2_satt, lower.tail = FALSE)
  } else {
    pchisq(q_stat, df = df1, lower.tail = FALSE)
  }
  p_chi2 <- pchisq(q_stat, df = df1, lower.tail = FALSE)
  
  tibble(
    mu_pooled = mu_pool,
    q_stat    = q_stat,
    f_stat    = f_stat,
    df1       = df1,
    df2_satt  = df2_satt,
    p_f_satt  = p_f_satt,
    p_chi2    = p_chi2
  )
}

# 3. Helper to attach mean-comparison p-values to non-categorical rows (`type != "cat"`)
add_mean_p <- function(d, group_vars, mu0 = NULL) {
  mean_rows <- d |> filter(type != "cat" | is.na(type))
  
  if (!is.null(mu0)) {
    out <- mean_rows |>
      rowwise() |>
      mutate(res = list(weighted_mean_t_test(p_w, var_p, n_eff, mu0 = mu0))) |>
      ungroup() |>
      tidyr::unnest(res)
    return(out)
  }
  
  anova_tbl <- mean_rows |>
    group_by(across(all_of(group_vars))) |>
    group_modify(~ weighted_means_anova(.x)) |>
    ungroup()
  
  d |>
    left_join(anova_tbl, by = group_vars) |>
    mutate(
      across(c(mu_pooled, q_stat, f_stat, df1, df2_satt, p_f_satt, p_chi2),
             ~ if_else(type != "cat" | is.na(type), .x, NA_real_))
    )
}

test_out <- get_table1_sums(con, 'data/sim_data.parquet', cont_vars = "variable_cat", by = "alzheim_BIN", overall = FALSE, percent = "column") |>
  add_weighted_stats() |>
  add_profile_ci()
  # dplyr::select(var, level, by_var, by_level, type, n, sum_y, sum_w, p_w, ci_lo, ci_hi) 

test_out |>
  add_mean_p(group_vars = "var") |>
  View()


get_table1_sums(con, 'data/sim_data.parquet', cat_vars = "variable_cat", overall = FALSE, percent = "column") |>
  add_weighted_stats() |> View()
sd(loaded_data$variable_cat)




