# binomial_ci
# converting values to percentages



# Example long data -------------------------------------------------------

# Data from Dhana K, Beck T, Desai P, Wilson RS, Evans DA, Rajan KB. Prevalence of Alzheimer's disease dementia in the 50 US states and 3142 counties: A population estimate using the 2020 bridged-race postcensal from the National Center for Health Statistics. Alzheimers Dement. 2023 Oct;19(10):4388-4395. doi: 10.1002/alz.13081. Epub 2023 Jul 17. PMID: 37458371; PMCID: PMC10593099.

ex_data <- data.frame(
  state = c("CT", "ME", "MA", "NH", "RI", "VT"),
  pop_over_65 = c(646.0, 293.8, 1198.4, 263.1, 192.5, 128.6)*1000,
  alz_prev_over_65 = c(11.9, 10.1, 11.3, 10.1, 11.4, 9.9),
  pop_total = c(3688496, 1414874, 7154084, 1415342, 1114521, 644663)
)

ex_data$alz_cases_over_65 <- round(ex_data$alz_prev_over_65/100*ex_data$pop_over_65)
ex_data$alz_prev_total <- ex_data$alz_cases_over_65/ex_data$pop_total
ex_data$non_alz_cases <- ex_data$pop_total - ex_data$alz_cases_over_65

total_n <- sum(ex_data$pop_total)
total_alz_cases <- sum(ex_data$alz_cases_over_65)
total_non_alz_cases <- sum(ex_data$non_alz_cases)

xmodel <- glm(cbind(total_alz_cases, total_non_alz_cases) ~ 1, family = binomial())
plogis(confint(xmodel))
?confint
exp(coef(xmodel))

get_risk_ci <- function(yes, no) {
  model <- glm(cbind(yes, no) ~ 1, family = binomial())
  plogis(confint(model))
}

get_risk_ci(ex_data$alz_cases_over_65[1], ex_data$non_alz_cases[1])

# test this:
profile_ci <- function(yes, n, level = 0.95) {
  p_hat <- yes / n
  crit <- qchisq(level, 1) / 2
  ll <- function(p) dbinom(yes, n, p, log = TRUE)
  f <- function(p) ll(p_hat) - ll(p) - crit
  lower <- if (yes == 0) 0 else uniroot(f, c(1e-10, p_hat))$root
  upper <- if (yes == n) 1 else uniroot(f, c(p_hat, 1 - 1e-10))$root
  c(estimate = p_hat, lower = lower, upper = upper)
}


ex_data$pop_total[1]/total_n

glm(cbind(ex_data$pop_total[1], total_n-ex_data$pop_total[1]) ~ 1, family = binomial()) |>
  confint() |>
  plogis()

get_risk_ci(ex_data$pop_total[1], total_n-ex_data$pop_total[1])

t(sapply(ex_data$pop_total, function(x) get_risk_ci(x, total_n - x)))


t(sapply(ex_data$pop_total, profile_ci, total_n))




glm(cbind(successes, total_n-successes) ~ 1, family = binomial()) |>
  confint() |>
  plogis()

binom.test(ex_data$pop_total[1], total_n)$conf.int
  



glm(ex_data$pop_total[1], family = binomial())
  confint() |>
  plogis()





ex_univariate <- data.frame(
  var = "Alzheimer's disease",
  level = c(0, 1),
  value1 = c(100, 900)
)

ex_categorical <- data.frame(
  var = "State",
  level = c("CT", "ME", "MA", "NH", "RI", "VT"),
  value1 = c(3688496, 1414874, 7154084, 1415342, 1114521, 644663)
)


ex_bivariate <- data.frame(
  var = "State",
  level = c("CT", "ME", "MA", "NH", "RI", "VT"),
  by_var = "",
  
)













