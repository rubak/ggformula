# This allows us to use these functions without attaching the packages they come from.

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @rdname makeFun
#' @importFrom mosaicCore makeFun df_stats na.warn
#' @inherit mosaicCore::makeFun
#' @export
makeFun <- mosaicCore::makeFun

#' Calculate statistics on a variable
#'
#' @rdname df_stats
#' @inherit mosaicCore::df_stats
#' @inheritParams mosaicCore::df_stats
#' @export
df_stats <- mosaicCore::df_stats

#' @rdname na.warn
#' @inherit mosaicCore::na.warn
#' @export
na.warn <- mosaicCore::na.warn
