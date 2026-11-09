# flatten_specs

flatten_specs <- function(variable_specs) {
  rows <- lapply(names(variable_specs), function(v) {
    s <- variable_specs[[v]]
    n <- length(s$labels)
    data.frame(
      variable = v,
      label = if(is.null(s$label)) NA_character_ else s$label,
      levels = if(n > 0) s$levels[seq_len(n)] else NA_real_,
      labels = if(n > 0) s$labels else NA_character_,
      short_label = if(is.null (s$short_label)) s$label else s$short_label,
      ref = if(is.null(s$ref)) NA_character_ else s$ref,
      type = if(is.null(s$type)) NA_character_ else s$type,
      # category = if(is.null(s$category)) NA_character_ else s$category,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


# unflatten_specs

unflatten_specs <- function(df) {
  vars <- unique(df$variable)
  
  specs <- lapply(vars, function(v) {
    d <- df[df$variable == v, , drop = FALSE]
    
    list(
      label = d$label[1],
      levels = d$levels,
      labels = d$labels,
      short_label = d$short_label[1],
      ref = d$ref[1],
      type = d$type[1]
      # category = d$category[1]
    )
  })
  
  setNames(specs, vars)
}


flat1 <- flatten_specs(variable_specs)

unflat1 <- unflatten_specs(flat1)

flat2 <- flatten_specs(unflat1)

unflat2 <- unflatten_specs(flat2)

all.equal(flat1, flat2)
all.equal(unflat1, unflat2)

rm(flat1)
rm(unflat1)
rm(flat2)
rm(unflat2)


gc()