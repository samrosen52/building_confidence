# get_var_type

get_var_type_from_name <- function(var, variable_specs = NULL) {
  
  var_upper <- toupper(var)
  suffix <- sub(".*_([A-Z]+)$", "\\1", var_upper)
  
  type <- switch(
    suffix,
    BIN = "categorical",
    PROP = "numeric",
    SUM = "numeric",
    MEAN = "numeric",
    MIN = "numeric",
    MAX = "numeric",
    CAT = "categorical",
    COMBO = "categorical",
    COLLAPSED = "categorical",
    FLAG = "categorical",
    FL = "categorical",
    CD = "categorical",
    DATE = "categorical",
    DT = "categorical",
    ID = "categorical", # NOTE maybe exclude
    NUM = "numeric",
    TOTAL = "numeric",
    # CUMSUM = "categorical",
    DAYS = "categorical",
    OCCURRENCE = "categorical",
    OCC = "categorical",
    EVER = "categorical",
    # A = "categorical",
    # IMPUTED = "categorical",
    ANY = "categorical",
    # YEAR = "categorical",
    # YEARS = "categorical",
    # MONTH = "categorical",
    # MONTHS = "numeric",
    COUNT = "numeric",
    NULL
  )
  
  if(is.null(type) & !is.null(variable_specs)) {
    stop(sprintf(
      "Variable '%s' not found in '%s' and suffix '%s' is not recognized.",
      var, variable_specs, suffix
    ))
  } else if(is.null(type)) {
    stop(sprintf(
      "Variable '%s' not found and '%s' is not recognized.",
      var, suffix
    ))
  }
  
  return(type)
  
}



get_var_type <- function(var, variable_specs = NULL) {
  
  if(!is.null(variable_specs) & var %in% names(variable_specs)) {
    type <- variable_specs[[var]]$type
  } else {
    type <- get_var_type_from_name(var, variable_specs)
  }
  
  if(is.null(type)) {
    stop(sprintf("No type defined for variable '%s'.", var))
  }
  
  if(!type %in% c("numeric", "categorical")) {
    stop(sprintf("Invalid type '%s' for variable '%s'.", type, var))
  }
  
  return(type)
}



