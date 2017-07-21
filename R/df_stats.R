#' @importFrom mosaicCore mosaic_formula_q mosaic_formula
#' @importFrom tidyr gather
NA

#' Calculate basic statistics on a quantitative variable
#'
#' Creates a data frame of statistics calculated on one variable, possibly for each
#' group formed by combinations of additional variables.
#' The resulting data frame has one column
#' for each of the statistics requested as well as columns for any grouping variables.
#'
#' @param formula A formula indicating which variables are to be used. See details.
#' @param data A data frame or list containing the variables.
#' @param ... Functions used to compute the statistics.  If this is empty,
#'   a default set of summary statistics is used.  Functions used must accept
#'   a vector of values and return either a (possibly named) single value,
#'   a (possibly named) vector of values, or a data frame with one row.
#'   Note: names may not be among the names of the named arguments of \code{df_stats}().
#' @param drop A logical indicating whether combinations of the grouping
#'   variables that do not occur in \code{data} should be dropped from the
#'   result.
#' @param fargs Arguments passed to the functions in \code{...}.
#' @param long_names A logical indciting whether the default names should include the name
#'   of the variable being summarized as well as the summarizing function name in the default
#'   case when names are not derived from the names of the returned object or
#'   an argument name.
#' @param nice_names A logical indicating whether \code{\link{make.names}()} should be
#'   used to force names of the returned data frame to by syntactically valid.
#' @param format One of \code{"long"} or \code{"wide"} indicating the desired shape of the
#'   returned data frame.
#' @param sep A charachter string to separate components of names.  Set to \code{""} if
#'   you don't want separation.
#' @importFrom stats quantile
#'
#' @details
#' Use a one-sided formula to compute summary statistics for the left hand side
#' expression over the entire data.
#' Use a two-sided formula to compute summary statistics for the left hand expression
#' for each combination of levels of the expressions ocurring on the right hand side.
#' This is most useful when the left hand side is quantitative and each expression
#' on the right hand side has relatively few unique values.  A function like
#' \code{\link[mosaic]{ntiles}()} is often useful to create a few groups of roughly equal size
#' determined by ranges of a quantitative variable.  See the examples.
#'
#' Note that unlike \code{dplyr::\link[dplyr]{summarise}()}, `df_stats()` ignores
#' any grouping defined in \code{data} if \code{data} is a grouped \code{tibble}.
#'
#' Names of columns in the resulting data frame are determined as follows.  For named
#' arguments in \code{...}, the argument name is used.  For unnamed arguments, if the
#' statistic function returns a result with names, those names are used.  Else, a name is
#' computed from the expression in \code{...} and the name of the variable being summarized.
#' For functions that produce multiple
#' outputs without names, consecutive integers are appended to the names.
#' See the examples.
#' @return A data frame.
#'
#' @examples
#' df_stats( ~ hp, data = mtcars)
#' df_stats( ~ hp, data = mtcars, mean, median)
#' df_stats( hp ~ cyl, data = mtcars, mean, median, range)
#' df_stats( hp ~ cyl, data = mtcars, mean, median, range, format = "long")
#' df_stats( hp ~ cyl + gear, data = mtcars, mean, median, range)
#' df_stats( hp ~ cyl | gear, data = mtcars, mean, median, range)
#' df_stats( hp ~ cyl, groups = gear, data = mtcars, mean, median, range)
#' # magrittr style piping is also supported
#' mtcars %>% df_stats(hp ~ cyl)
#' gf_violin(hp ~ cyl, data = mtcars, group = ~ cyl) %>%
#'   gf_point(mean_hp ~ cyl, data = df_stats(hp ~ cyl, data = mtcars, mean))
#'
#' @export
#' @importFrom rlang eval_tidy exprs expr
#' @importFrom stats model.frame aggregate
#'
df_stats <- function(formula, data, ..., drop = TRUE, fargs = list(),
                     sep = "_",
                     format = c("wide", "long"), groups = NULL,
                     long_names = TRUE, nice_names = FALSE) {
  # dots <- lazyeval::lazy_dots(...)
  dots <- rlang::exprs(...)
  format <- match.arg(format)

  if (length(dots) < 1) {
    dots <- list(rlang::expr(ggf_favstats))
    names(dots) <- ""
  }

  if (inherits(formula, "data.frame") && inherits(data, "formula")) {
    # switched at birth. Likely because input is piped in
    tmp <- data
    data <- formula
    formula <- tmp
  }
  if ( ! inherits(formula, "formula")) stop("first arg must be a formula")
  if ( ! inherits(data, "data.frame")) stop("second arg must be a data.frame")

  formula <- cond2sum(mosaicCore::mosaic_formula_q(formula, groups = groups))

  MF <- model.frame(formula, data)

  one_group <- FALSE
  if (ncol(MF) == 1) {
    one_group <- TRUE
    if ("group" %in% names(MF)) {
      MF[, "..group.."] <- 1
    } else {
      MF[, "group"] <- 1
    }
  }

  res <-
    lapply(dots, function(f)
      aggregate(MF[, 1], by = MF[, -1, drop = FALSE],
                FUN = function(x) do.call(rlang::eval_tidy(f), c(list(x), fargs)),
                drop = drop
      )
    )
  d <- ncol(MF) - 1
  groups <- res[[1]][, 1:d, drop = FALSE]

  res_names <- lapply(res, function(x) colnames(as.matrix(x$x)))
  arg_names <- names(res)

  # res has an odd format where res[[i]]$x is a list-based matrix
  # here we convert these to data frames of vectors
  res <- lapply(res, function(x) data.frame(lapply(data.frame(x$x), unlist)))
  ncols <- sapply(res, ncol)

  fun_names <- sapply(dots, function(x) deparse(x))
  if (long_names) {
    fun_names <- paste0(fun_names, sep, deparse(formula[[2]]))
  }
  fun_names <- ifelse(sapply(res_names, is.null), fun_names, "")

  # # Use numbers or "" if there are no names.
  alt_res_names <- lapply(ncols, function(nc) if (nc > 1) format(1:nc) else "")

  res_names <-
    mapply(
      function(x, y) { if (is.null(x)) y else x },
      res_names, alt_res_names
    )
  # print(res_names)

  final_names <-
    mapply(
      paste0,
      ifelse(arg_names == "", fun_names, arg_names),
      sep,
      res_names) %>%
    unlist()

  # remove unneccessary seperators
  final_names <- gsub(paste0(sep, sep), sep, final_names)
  final_names <- gsub(paste0(sep, "$"), "", final_names)
  final_names <- gsub(paste0("^", sep), "", final_names)

  res <- do.call(cbind, c(list(groups), res))
  names(res) <- c(names(res)[1:d], unlist(final_names))
  if (nice_names) names(res) <- base::make.names(names(res), unique = TRUE)
  if (one_group) {
    res <- res[, -1]
  }
  row.names(res) <- NULL

  # return the appropriate format
  if (format == "long") {
    res %>% tidyr::gather(stat, value, -(1:d))
  } else {
    res
  }
}

ggf_favstats <- function (x, ..., na.rm = TRUE, type = 7)
{
  if (!is.null(dim(x)) && min(dim(x)) != 1)
    warning("Not respecting matrix dimensions.  Hope that's OK.")
  # x <- as.vector(x)
  if (! is.numeric(x)) {
    warning("Auto-converting ", class(x), " to numeric.")
    x <- as.numeric(x)
    if (!is.numeric(x)) stop("Auto-conversion to numeric failed.")
  }

  qq <- if (na.rm)
    stats::quantile(x, na.rm = na.rm, type = type)
  else
    rep(NA, 5)
  val <- data.frame(
    min=qq[1],
    Q1 = qq[2],
    median = qq[3],
    Q3 = qq[4],
    max = qq[5],
    mean = base::mean(x, na.rm = na.rm),
    sd = stats::sd(x, na.rm = na.rm),
    n = base::sum(! is.na(x)),
    missing = base::sum( is.na(x) )
  )
  rownames(val) <- ""
  return(val)
}

