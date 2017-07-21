
# crude way to convert | to + in formulas

cond2sum <- function(formula) {
  e <- environment(formula)
  res <- as.formula(gsub("\\|", "+", format(formula)))
  environment(res) <- e
  res
}

