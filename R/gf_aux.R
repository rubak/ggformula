
#' Non-layer functions for gf plots
#'
#' These functions modify things like labels, limits, scales, etc. for plots
#' ggplot2 plots. They are wrappers around functions in ggplot2 that allow for
#' chaining syntax.
#'
#' @param object a gg object
#' @param ... additional arguments passed through to the similarly named function in
#' @param theme a ggplot2 theme function like \code{\link{theme_minimal}}.
#' ggplot2
#' @return a modified gg object
#'
#' @rdname gf_aux
#' @export
gf_labs <- function(object, ...) {
  object + ggplot2::labs(...)
}

#' @rdname gf_aux
#' @export
gf_lims <- function(object, ...) {
  object + ggplot2::lims(...)
}

#' @rdname gf_aux
#' @export
gf_theme <- function(object, ..., theme = theme_minimal) {
  object + do.call(theme, list(...))
}

#' @rdname gf_aux
#' @export
gf_facet_wrap <- function(object, ...) {
  object + ggplot2::facet_wrap(...)
}

#' @rdname gf_aux
#' @export
gf_facet_grid <- function(object, ...) {
  object + ggplot2::facet_grid(...)
}