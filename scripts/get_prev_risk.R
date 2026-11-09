# File from work
library(dplyr)


test <- auto_summary(con, demo_path, "cohortgrp_a", force_type = "categorical") |>
  select(level, value1) |>
  filter(!is.na(level))

total_n <- sum(test$value1)
poisson_model <- glm(value1 ~ 0 + factor(level), data = test, family = poisson())

poisson_model

log_count_ci <- confint(poisson_model, level = 0.95)
prop_ci <- exp(log_count_ci)/total_n
rownames(prop_ci) <- test$level
print(prop_ci)

library(dplyr)
test_data <- arrow::read_parquet(demo_path) |>
  mutate(
    cohortgrp_a = case_when(
      
      
    ),
    cohortgrp_a = factor(cohortgrp_a, levels = c("DS", "AS", "ID"))
  )

library(nnet)
multinom_model <- multinom(cohortgrp_a ~ 1, data = test_data)
prof_intervals <- confint(multinom_model, level = 0.95)
prop_intervals <- plogis(prof_intervals) # this might be wrong

print(prop_intervals)
print(prop_ci)
auto_summary(con, demo_path, cohortgrp_a, force_type = "categorical")

1-prop_intervals

counts_df <- data.frame(
  cohortgrp_a = c("DS", "AS", "ID"),
  Count = c(149362, 1122365, 605950)
)

total_n <- sum(coutns_df$Count)

get_multinom_profile_ci <- function(category_count, total_n) {
  successes <- category_count
  failures <- total_n - category_count
  
  model <- glm(cbind(successes, failures) ~ 1, family = binomial())
  
  log_odds_ci <- confint(model, level = 0.95)
  
  plogis(log_odds_ci)
}

results <- t(sapply(counts_df$Count, get_multinom_profile_ci, total_n = total_n))
rownames(results) <- counts_df$cohortgrp_a
colnames(results) <- c("lower_ci", "upper_ci")

final_table <- cbind(proportion = counts_df$Count/total_n, results)
print(final_table)

counts_df_outcome <- data.frame(
  cohortgrp_a = c("DS", "AS", "ID"),
  Outcome_Yes = c(n/2, n/5, n/5),
  Outcome_No = c(n.2, n/5*4, n/5*4)
)

total_cases <- sum(counts_df_outcome$Outcome_Yes)



get_likelihood_among_cases <- function(category_cases, total_cases) {
  successes <- category_cases
  failures <- total_cases - category_cases
  
  model <- glm(cbind(successes, failures) ~ 1, family = binomial())
  
  plogis(confint(model, level = 0.95))
}


results_B <- t(sapply(counts_df_outcome$Outcome_Yes, get_likelihood_among_cases, total_cases))
rownames(results_B) <- counts_df_outcome$cohortgrp_a
colnames(results_B) <- c("ci_lower", "ci_upper")

final_B <- cbind(proportion_yes = counts_df_outcome$Outcome_Yes/total_cases, results_B)
print(final_B)

total_non_cases <- sum(counts_df_outcome$Outcome_No)
results_B_no <- t(sapply(counts_df_outcome$Outcome_No, get_likelihood_among_cases, total_non_cases))
row_names(results_B_no) <- counts_df_outcome$cohortgrp_a
colnames(results_B_no) <- c("ci_lower", "ci_upper")

final_B_no <- cbind(proportion_no = counts_df_outcome$Outcome_No/total_non_cases, results_B_no)
print(final_B_no)



get_risk_by_group <- function(yes, no) {
  model <- glm(cbind(yes, no) ~ 1, family = binomial())
  
  plogis(confint(model, level = 0.95))
}

get_risk_by_group(224473, 879892)

# NOTE: why mapply()?
results_A <- t(mapply(get_risk_by_group, counts_df_outcome$Outcome_Yes, counts_df_outcome$Outcome_No))
rownames(results_A) <- counts_df_outcome$cohortgrp_a
colnames(results_A) <- c("ci_lower", "ci_upper")

totals <- counts_df_outcome$Outcome_Yes + counts_df_outcome$Outcome_No
final_A <- cbind(prev = counts_df_outcome$Outcome_Yes/totals, results_A)
print(final_A)

chisq.test()
counts_df_outcome

test_model <- glm(cbind(224473, 897892) ~ 1, family = binomial())
summary(test_model)

get_p_value <- function(yes, no) {
  model <- glm(cbind(yes, no) ~ 1, family = binomial())
  
  fit_null <- glm(cbind(yes, no) ~ 0 + offset(rep(0, lenght(yes))), family = binomial())
  anova(fit_null, model, test = "Chisq")
}








