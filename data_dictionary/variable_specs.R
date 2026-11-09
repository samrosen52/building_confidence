# variable_specs

variable_specs <- list(
  "state" = list(
    label = "State",
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c("CT", "ME", "MA", "NH", "RI", "VT"),
    ref = "CT",
    type = "categorical"
  ),
  
  alzheim_BIN = list(
    label = "Alzheimer's diagnosis",
    levels = c(0, 1),
    labels = c("No", "Yes"),
    ref = "Yes",
    type = "categorical"
  ),
  
  pop_total = list(
    label = "Population",
    type = "numeric"
  )
)

