# generate_data


# Data from Dhana K, Beck T, Desai P, Wilson RS, Evans DA, Rajan KB. Prevalence of Alzheimer's disease dementia in the 50 US states and 3142 counties: A population estimate using the 2020 bridged-race postcensal from the National Center for Health Statistics. Alzheimers Dement. 2023 Oct;19(10):4388-4395. doi: 10.1002/alz.13081. Epub 2023 Jul 17. PMID: 37458371; PMCID: PMC10593099.

state_data <- data.frame(
  state = c("CT", "ME", "MA", "NH", "RI", "VT"),
  pop_over_65 = c(646.0, 293.8, 1198.4, 263.1, 192.5, 128.6)*1000,
  alz_prev_over_65 = c(11.9, 10.1, 11.3, 10.1, 11.4, 9.9)/100,
  pop_total = c(3688496, 1414874, 7154084, 1415342, 1114521, 644663)
)



# Creating simulated dataset ----------------------------------------------

n_per_state <- 1000

set.seed(4)

sim_df <- do.call(
  rbind,
  lapply(
    1:nrow(state_data),
    function(i) {
      st <- state_data$state[i]
      prev <- state_data$alz_prev_over_65[i]
      
      data.frame(
        id = (1:n_per_state) + (i-1)*n_per_state,
        state = st,
        alzheim_BIN = rbinom(n = n_per_state, size = 1, prob = prev)
      )
    })
)



# Aggregating data to correct setup ---------------------------------------

sim_data_agg <- sim_df |>
  count(state, alzheim_BIN) |>
  rename("level"="state", "by_level"="alzheim_BIN", "value1"="n") |>
  mutate(var="state", by_var="alzheim_BIN") |>
  select(var, level, by_var, by_level, value1)





arrow::write_parquet(sim_df, "data/sim_data.parquet")











