
library(duckdb)
library(DBI)
con <- dbConnect(duckdb())

library(binom)
get_variable_names(con, 'data/sim_data.parquet')
test <- dbGetQuery(
  con,
  "SELECT state, variable_cat,
    SUM(alzheim_BIN) AS alzheim_SUM,
    SUM(alzheim_BIN*weights) AS alzheim_SUM_weighted,
    SUM(weights) AS n_weighted,
    COUNT(*) AS n_total,
    SUM(alzheim_BIN)/COUNT(*) AS prop,
    SUM(alzheim_BIN*weights)/SUM(weights) AS prop_weighted
  FROM 'data/sim_data.parquet'
  GROUP BY state, variable_cat
  ORDER BY state, variable_cat"
)
test

binom.confint(test$alzheim_SUM[1], test$n_total[1]-test$alzheim_SUM[1], methods = "profile")
binom.confint(test$alzheim_SUM, test$n_total-test$alzheim_SUM, methods = "profile")
binom.confint(test$alzheim_SUM_weighted, test$n_weighted-test$alzheim_SUM_weighted, methods = "profile")

?binom.confint



get_weighted_sums <- function(con, path, group_vars, outcome, weight = NULL) {
  q   <- function(x) as.character(dbQuoteIdentifier(con, x))
  grp <- paste(q(group_vars), collapse = ", ")
  y   <- q(outcome)
  if (is.null(weight)) {
    w          <- "1"
    where_weight <- ""
  } else {
    w          <- q(weight)
    where_weight <- glue("AND {w} IS NOT NULL")
  }
  src <- as.character(dbQuoteString(con, path))
  
  sql <- glue::glue("
    SELECT {grp},
      COUNT(*)::DOUBLE              AS n,
      SUM({y})::DOUBLE              AS sum_y,
      SUM({w})::DOUBLE              AS sum_w,
      SUM({y} * {w})::DOUBLE        AS sum_wy,
      SUM({w} * {w})::DOUBLE        AS sum_w2,
      SUM({y} * {w} * {w})::DOUBLE  AS sum_w2y
    FROM read_parquet({src})
    WHERE {y} IS NOT NULL {where_weight}
    GROUP BY {grp}
  ")
  
  as_tibble(dbGetQuery(con, sql))
}

add_weighted_stats <- function(d) {
  d |>
    mutate(
      p_w   = sum_wy / sum_w,
      var_p = ((1 - 2 * p_w) * sum_w2y + p_w^2 * sum_w2) / sum_w^2,
      se_p  = sqrt(var_p),
      n_eff = sum_w^2 / sum_w2,        # Kish effective sample size
      deff  = n * sum_w2 / sum_w^2,    # design effect from weighting
      y_eff = p_w * n_eff              # effective count for binomial CIs
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

add_profile_ci <- function(d, level = 0.95) {
  ci <- mapply(profile_ci, d$y_eff, d$n_eff, level = level)
  d$ci_low  <- ci[1, ]
  d$ci_high <- ci[2, ]
  d
}


14*log(0.05) + (130-0.05)*log1p(-0.05)
lrt_p <- function(y, n, p0 = 0.05) {
  if (!is.finite(y) || !is.finite(n) || n <= 0) return(NA_real_)
  ll     <- function(p) y * log(p) + (n - y) * log1p(-p)
  p_hat  <- y / n
  ll_max <- if (p_hat > 0 && p_hat < 1) ll(p_hat) else 0
  G      <- 2 * (ll_max - ll(p0))
  pchisq(max(G, 0), df = 1, lower.tail = FALSE)
}

add_lrt_p <- function(d, p0 = 0.05) {
  d$p_value <- mapply(lrt_p, d$y_eff, d$n_eff, p0 = p0)
  d
}

res <- get_weighted_sums(
  con,
  path       = "data/sim_data.parquet",
  group_vars = c("state", "variable_cat"),
  outcome    = "alzheim_BIN"
) |>
  add_weighted_stats() |>
  add_profile_ci(level = 0.95) |>
  add_lrt_p()

View(res)


profile_ci <- function(x, n, conf = 0.95) {
  crit <- qchisq(conf, 1) / 2
  ll <- function(p) ifelse(x == 0, 0, x * log(p)) +
    ifelse(n - x == 0, 0, (n - x) * log1p(-p))
  if (!is.finite(n) || n <= 0) return(c(NA_real_, NA_real_))
  phat <- x / n
  f <- function(p) ll(phat) - ll(p) - crit
  lo <- if (phat <= 0) 0 else uniroot(f, c(1e-12, phat))$root
  hi <- if (phat >= 1) 1 else uniroot(f, c(phat, 1 - 1e-12))$root
  c(lo, hi)
}


profile_ci(test$alzheim_SUM[1], test$n_total[1])

prop_ci <- function(yes, n) {
  
  p_hat <- yes/n
  
  model <- glm(cbind(yes, n-yes) ~ 1, family = binomial())
  
  model_ci <- plogis(confint(model, level = 0.95))
  
  return(model_ci)
  
}



add_pct_ci <- function(x, dir = "row", conf = 0.95) {
  # denominator group, matching the earlier SQL partitions
  key <- if (all(is.na(x$by_var))) x$var
  else if (dir == "row") interaction(x$var, x$level, drop = TRUE)
  else interaction(x$var, x$by_level, drop = TRUE)
  is_cat <- x$type == "categorical"
  
  den_w  <- ave(x$w_sum,  key, FUN = sum)
  den_w2 <- ave(x$w2_sum, key, FUN = sum)
  n_eff  <- den_w^2 / den_w2          # Kish effective n (= n if unweighted)
  
  x$pct <- ifelse(is_cat, x$w_sum / den_w * 100, NA_real_)
  
  ci <- t(mapply(profile_ci, x = x$w_sum / den_w * n_eff, n = n_eff,
                 MoreArgs = list(conf = conf)))
  x$lower <- ifelse(is_cat, ci[, 1] * 100, NA_real_)
  x$upper <- ifelse(is_cat, ci[, 2] * 100, NA_real_)
  
  # numeric rows: t-based CI for the mean
  n_num   <- x$w_sum^2 / x$w2_sum
  half    <- qt(1 - (1 - conf) / 2, n_num - 1) * x$sd / sqrt(n_num)
  x$lower <- ifelse(is_cat, x$lower, x$mean - half)
  x$upper <- ifelse(is_cat, x$upper, x$mean + half)
  x
}



prop_ci(test$alzheim_SUM[1], test$n_total[1])
test_glm <- glm(cbind(test$alzheim_SUM, test$n_total-test$alzheim_SUM) ~ 1, family = binomial())

test_ci <- plogis(confint(test_glm, level = 0.95))














