#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{size}, \code{shape}, \code{fill}, \code{group}, \code{stroke}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_point}()}
#' @export
#' @examples
#' gf_point(show.help = TRUE)
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars, verbose = TRUE)
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars) %>%
#'   gf_abline(~ color:"red" + slope:-0.10 + intercept:35)
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars) %>%
#'   gf_abline(color = "red", slope = -0.10, intercept = 35)
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars) %>%
#'   gf_abline(color = "red", slope = -0.10, intercept = 33:36) %>%
#'   gf_hline(color = "navy", yintercept = c(20, 25)) %>%
#'   gf_vline(color = "brown", xintercept = c(200, 300))
#' # faceting -- two ways
#' gf_point(mpg ~ hp, data = mtcars) %>%
#'   gf_facet_wrap(~ am)
#' gf_point(mpg ~ hp + group:cyl | am, data = mtcars)
#' gf_point(mpg ~ hp + group:cyl | ~ am, data = mtcars)
#' gf_point(mpg ~ hp + group:cyl | am ~ ., data = mtcars)
#'
#' gf_text(Sepal.Length ~ Sepal.Width + label:Species + color:Species , data = iris)
#'
#' # Chaining in the data
#' mtcars %>% gf_point(mpg ~ wt)
#'

gf_point <-
  gf_factory(
    type = "point",
    extras = alist(alpha = ,  color = , size = , shape = , fill = , group = , stroke = )
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{size}, \code{shape}, \code{fill}, \code{group}, \code{stroke}, \code{width}, \code{height}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_jitter}()}

#' @export
gf_jitter <-
  gf_factory(
    type = "jitter",
    extras = alist(alpha = ,  color = , size = , shape = , fill = , group = , stroke = ,
                   width =, height = )
    )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}, \code{lineend}, \code{linejoin}, \code{linemitre}, \code{arrow}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_line}()}
#' @export
gf_line <-
  gf_factory(
    type = "line",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = ,
                   lineend = , linejoin = , linemitre = , arrow = )
    )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{lineend}, \code{linejoin}, \code{linemitre}, \code{arrow}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_path}()}
#' @export
gf_path <-
  gf_factory(
    type = "path",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   lineend = "butt", linejoin = "round", linemitre = 1, arrow = NULL)
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{method}, \code{formula}, \code{se}, \code{method.args}, \code{n}, \code{span}, \code{fullrange}, \code{level}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_smooth}()}
#' @export
gf_smooth <-
  gf_factory(
    type = "smooth",
    extras = alist(method = "auto", formula = y ~ x, se = TRUE, method.args = ,
                   n = 80 , span = 0.75 , fullrange = FALSE, level = 0.95)
    )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{weight}, \code{df}, \code{spar}, \code{tol}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_spline}()}
#' @export
gf_spline <-
  gf_factory(
    type = "spline",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   weight = , df = , spar = , tol = )
    )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}, \code{hjust}, \code{vjust}, \code{interpolate}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_raster}()}
#' @export
gf_raster <-
  gf_factory(
    type = "raster",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = ,
                   hjust = 0.5, vjust = 0.5, interpolate = FALSE)
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{weight}, \code{lineend}, \code{linejoin}, \code{linemitre}, \code{quantiles}, \code{formula}, \code{method}, \code{method.args}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_quantile}()}
#' @export
gf_quantile <-
  gf_factory(
    type = "quantile",
    extras = alist(alpha = , color = , group = , linetype = , size = , weight =,
                   lineend = "butt", linejoin = "round", linemitre = 1, quantiles = ,
                   formula = , method = ,  method.args =  )
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{contour}, \code{n}, \code{h}, \code{lineend}, \code{linejoin}, \code{linemitre}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_density_2d}()}
#' @export
gf_density_2d <-
  gf_factory(
    type = "density_2d",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   contour = TRUE, n = 100 , h = NULL , lineend = "butt", linejoin = "round",
                   linemitre = 1 )
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{contour}, \code{n}, \code{h}, \code{lineend}, \code{linejoin}, \code{linemitre}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_density2d}()}
#' @export
gf_density2d <-
  gf_factory(
    type = "density2d",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   contour = TRUE, n = 100 , h = NULL , lineend = "butt", linejoin = "round",
                   linemitre = 1 )
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{bins}, \code{binwidth}, \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_hex}()}
#' @export
gf_hex <-
  gf_factory(
    type = "hex",
    extras = alist(bins = , binwidth = , alpha = , color = , fill = , group = , size = )
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{shape}, \code{size}, \code{weight}, \code{coef}, \code{outlier.color}, \code{outlier.fill}, \code{outlier.shape}, \code{outlier.size}, \code{outlier.stroke}, \code{outlier.alpha}, \code{notch}, \code{notchwidth}, \code{varwidth}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_boxplot}()}
#' @export
gf_boxplot <-
  gf_factory(
    type = "boxplot",
    extras = alist(
      alpha = , color = , fill = , group = , linetype = , shape = , size = ,
      weight =, coef = ,
      outlier.color = NULL, outlier.fill = NULL,
      outlier.shape = 19, outlier.size = 1.5, outlier.stroke = 0.5,
      outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5, varwidth = FALSE)
  )

#' Formula interface to gplot2
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{label}, \code{alpha}, \code{angle}, \code{color}, \code{family}, \code{fontface}, \code{group}, \code{hjust}, \code{lineheight}, \code{size}, \code{vjust}, \code{parse}, \code{nudge_x}, \code{nudge_y}, \code{check_overlap}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_text}()}
#' @export
gf_text <-
  gf_factory(
    type = "text",
    extras = alist(
      label =, alpha = , angle = , color = , family = , fontface = , group = , hjust = ,
      lineheight = , size = , vjust = , parse = FALSE, nudge_x = 0, nudge_y = 0,
      check_overlap = FALSE
      )
  )

#' Formula interface to geom_label()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{label}, \code{alpha}, \code{angle}, \code{color}, \code{family}, \code{fontface}, \code{group}, \code{hjust}, \code{lineheight}, \code{size}, \code{vjust}, \code{parse}, \code{nudge_x}, \code{nudge_y}, \code{lparse}, \code{nudge_x}, \code{nudge_y}, \code{label.padding}, \code{label.r}, \code{label.size}, \code{check_overlap}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_label}()}
#' @export
gf_label <-
  gf_factory(
    type = "label",
    extras = alist(
      label =, alpha = , angle = , color = , family = , fontface = , group = , hjust = ,
      lineheight = , size = , vjust = ,
      parse = , nudge_x = , nudge_y = ,
      lparse = FALSE, nudge_x = 0, nudge_y = 0,
      label.padding = unit(0.25, "lines"), label.r = unit(0.15, "lines"),
      label.size = 0.25, check_overlap = FALSE)
  )

#' Formula interface to geom_area()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_area}()}
#' @export
gf_area <-
  gf_factory(
    type = "area",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
    )

#' Formula interface to geom_violin()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}, \code{}, \code{draw_quatiles}, \code{trim}, \code{scale}, \code{bw}, \code{adjust}, \code{kernel}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_violin}()}
#' @export
gf_violin <-
  gf_factory(
    type = "violin",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = , weight,
      draw_quatiles = NULL, trim = TRUE, scale = "area", bw = , adjust = , kernel = )
  )

#' Formula interface to geom_spoke()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{angle}, \code{radius}, \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#' @section Note \code{angle} and \code{radius} must be set or mapped.
#' @seealso \code{\link{geom_spoke}()}
#' @export
gf_spoke <-
  gf_factory(
    type = "spoke",
    extras = alist(
      angle = , radius = ,
      alpha = , color = , group = , linetype = , size = ),
    note = "Note: angle and radius must be set or mapped."
  )


#' Formula interface to geom_step()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{direction}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_step}()}
#' @export
gf_step <-
  gf_factory(
    type = "step",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   direction = "hv" )
    )

#' Formula interface to geom_tile()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_tile}()}
#' @export
gf_tile <-
  gf_factory(
    type = "tile",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
  )

#' Formula interface to geom_count()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{shape}, \code{size}, \code{stroke}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_count}()}
#' @export
gf_count <-
  gf_factory(
    type = "count",
    extras = alist(
      alpha = , color = , fill = , group = , shape = , size = , stroke =
    )
  )

#' Formula interface to geom_col()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_col}()}
#' @export
gf_col <-
  gf_factory(
    type = "col",
    extras = alist(
      alpha = , color = , fill = , group = , linetype = , size =
    )
  )

#' Formula interface to geom_blank()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_blank}()}
#' @export
gf_frame <-
  gf_factory(type = "blank")

' Formula interface to geom_histogram()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_histogram}()}
#' @export
gf_histogram2 <-
  gf_factory(
    type = "histogram",
    aes_form = y ~ x,
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
    )




#' Formula interface to geom_histogram()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_histogram}()}, \code{\link{gf_histogram2}()}
#' @export
#' @examples
#' gf_histogram(~ Sepal.Length | Species, data = iris, binwidth = 0.25)
#' gf_dens(show.help = TRUE)
#' gf_density(~ Sepal.Length + color:Species, data = iris)
#' gf_dens(~ Sepal.Length + color:Species, data = iris)
#' gf_freqpoly(~ Sepal.Length + color:Species, data = iris)
#' gf_dotplot(~ Sepal.Length + fill:Species, data = iris)
#' # Chaining in the data
#' iris %>% gf_dens(~ Sepal.Length + color:Species)
gf_histogram <-
  gf_factory(
    type = "histogram", aes_form = ~x,
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
  )

#' Formula interface to geom_density()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}, \code{weight}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_density}()}
#' @export
gf_density <-
  gf_factory(
    type = "density", aes_form = ~ x,
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = , weight = )
  )

# modified version of density plot without line along bottom and sides
#' Formula interface to geom_line() and stat_density()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{stat}, \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}, \code{weight}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_line}()}
#' @export
gf_dens <-
  gf_factory(
    type = "line",
    aes_form = ~ x,
    extras = alist(stat = "density", alpha = , color = , fill = , group = , linetype = , size = , weight = )
  )

#' Formula interface to geom_dotplot()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{binwidth}, \code{binaxis}, \code{method}, \code{binpositions}, \code{stackdir}, \code{stackratio}, \code{dotsize}, \code{stackgroups}, \code{origin}, \code{right}, \code{width}, \code{drop}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_dotplot}()}
#' @export
gf_dotplot <-
  gf_factory(
    type = "dotplot",
    aes_form = ~x,
    extras = alist(
      alpha = , color = , fill =, group = ,
      binwidth = NULL, binaxis = "x", method = "dotdensity",
      binpositions = "bygroup", stackdir = "up", stackratio = 1,
      dotsize = 1, stackgroups = FALSE, origin = NULL, right = TRUE,
      width = 0.9, drop = FALSE)
  )

#' Formula interface to geom_bar()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}, \code{width}, \code{binwidth}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_bar}()}
#' @export
gf_bar <-
  gf_factory(
    type = "bar", aes_form = ~ x,
    extras = alist(
      alpha = , color = , fill = , group = , linetype = , size = ,
      width = NULL, binwidth = NULL )
  )

#' Formula interface to geom_freqpoly()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{binwidth}, \code{bins}, \code{center}, \code{boundary}, \code{}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_freqpoly}()}
#' @export
gf_freqpoly <-
  gf_factory(
    type = "freqpoly", aes_form = ~ x,
    extras = alist(
      alpha = , color = , group = , linetype = , size =,
      binwidth =, bins = , center = , boundary = ,
    )
  )

#' Formula interface to geom_qq()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~sample.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{group}, \code{x}, \code{y}, \code{distribution}, \code{dparams}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_qq}()}
#' @export
gf_qq <-
  gf_factory(
    type = "qq", aes_form = ~ sample,
    extras = alist(group = , x = , y =, distribution = stats::qnorm , dparams = list())
  )

#' Formula interface to geom_rug()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ~x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{sides}, \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_rug}()}
#' @export
gf_rug <-
  gf_factory(
    type = "rug", aes_form = ~ x,
    extras = alist(sides = "bl", alpha = , color = , group = , linetype = , size = )
    )

#' Formula interface to geom_raster()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape fill ~ x + y.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_raster}()}

#' @export
gf_raster2 <-
  gf_factory(type = "raster", aes_form = fill ~ x + y)

#' Formula interface to geom_contour()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape z ~ x + y.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_contour}()}
#' @export
gf_contour <-
  gf_factory(type = "contour", aes_form = z ~ x + y)

#' Formula interface to geom_ribbon()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ymin + ymax ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_ribbon}()}
#' @export
#' @examples
#' gf_ribbon(show.help = TRUE)
#' if (require(weatherData) & require(dplyr)) {
#' Temps <- NewYork2013 %>% mutate(city = "NYC") %>%
#' bind_rows(Mumbai2013 %>% mutate(city = "Mumbai")) %>%
#' bind_rows(London2013 %>% mutate(city = "London")) %>%
#'   mutate(date = lubridate::date(Time),
#'          month = lubridate::month(Time)) %>%
#'   group_by(city, date) %>%
#'   summarise(
#'     hi = max(Temperature, na.rm = TRUE),
#'     lo = min(Temperature, na.rm = TRUE),
#'     mid = (hi + lo)/2
#'   )
#'
#' gf_ribbon(lo + hi ~ date + fill:city, data = Temps, alpha = 0.4) %>%
#'    gf_theme(theme = theme_minimal)
#' gf_linerange(lo + hi + color:mid ~ date | city ~ ., data = Temps) %>%
#'   gf_refine(scale_colour_gradientn(colors = rev(rainbow(5))))
#' gf_ribbon(lo + hi ~ date | city ~ ., data = Temps)
#' # Chaining in the data
#' Temps %>% gf_ribbon(lo + hi ~ date, alpha = 0.4) %>%
#'   gf_facet_grid(city ~ .)
#' }

gf_ribbon <-
  gf_factory(
    type = "ribbon", aes_form = ymin + ymax ~ x,
    extras = list(alpha = 0.3))

#' Formula interface to geom_curve()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y + yend ~ x + xend.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{curvature}, \code{angle}, \code{ncp}, \code{arrow}, \code{lineend}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_curve}()}
#' @export
gf_curve <-
  gf_factory(
    type = "curve", aes_form = y + yend ~ x + xend,
    extras = alist(
      alpha = , color = , group = , linetype = , size = ,
      curvature = 0.5, angle = 90, ncp = 5, arrow = NULL, lineend = "butt")
  )

#' Formula interface to geom_segment()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y + yend ~ x + xend.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{arrow}, \code{lineend}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_segment}()}
#' @export
gf_segment <-
  gf_factory(
    type = "segment", aes_form = y + yend ~ x + xend,
    extras = alist(
      alpha = , color = , group = , linetype = , size = ,
      arrow = NULL, lineend = "butt"
      )
  )

#' Formula interface to geom_linerange()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ymin + ymax ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_linerange}()}
#' @export
gf_linerange <-
  gf_factory(
    type = "linerange", aes_form = ymin + ymax ~ x,
    extras = alist( alpha = , color = , group = , linetype = , size = )
  )

#' Formula interface to geom_pointrange()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y + ymin + ymax ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{fatten}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_pointrange}()}
#' @export
gf_pointrange <-
  gf_factory(
    type = "pointrange",
    aes_form = y + ymin + ymax ~ x,
    extras = alist(
      alpha = , color = , group = , linetype = , size = ,
      fatten = 2 )
  )

#' Formula interface to geom_crossbar()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y + ymin + ymax ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}, \code{fatten}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_crossbar}()}
#' @export
gf_crossbar <-
  gf_factory(
    type = "crossbar",
    aes_form = y + ymin + ymax ~ x,
    extras = alist(
      alpha = , color = , group = , linetype = , size = , fatten = 2.5
    )
  )

#' Formula interface to geom_errorbar()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ymin + ymax ~ x.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_errorbar}()}
#' @export
gf_errorbar <-
  gf_factory(
    type = "errorbar",
    aes_form = ymin + ymax ~ x,
    extras = alist(
      alpha = , color = , group = , linetype = , size =
      )
    )

#' Formula interface to geom_errorbarh()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape y ~ x + xmin + xmax.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_errorbarh}()}
#' @export
gf_errorbarh <-
  gf_factory(
    type = "errorbarh",
    aes_form = y ~ x + xmin + xmax,
    extras = alist(
      alpha = , color = , group = , linetype = , size =
      )
    )

#' Formula interface to geom_rect()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape ymin + ymax ~ xmin + xmax.
#'   Faceting can be acheived by including \code{|} in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{alpha}, \code{color}, \code{fill}, \code{group}, \code{linetype}, \code{size}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_rect}()}
#' @export
gf_rect <-
  gf_factory(
    type = "rect",
    aes_form = ymin + ymax ~ xmin + xmax,
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
  )


#' Formula interface to geom_abline()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'

#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{slope}, \code{intercept}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_abline}()}
#' @export
gf_abline <-
  gf_factory(
    type = "abline", aes_form = NULL,
    extras = alist( slope =, intercept = )
  )

#' Formula interface to geom_hline()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'

#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{yintercept}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_hline}()}
#' @export
gf_hline <-
  gf_factory(
    type = "hline", aes_form = NULL,
    extras = alist(yintercept = )
  )

#' Formula interface to geom_vline()
#'
#' \pkg{ggformula} functions provide a formula interface to \code{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \code{gformula} argument).
#' Additional formula terms of the form \code{+ attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{+ attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \code{attribute = value} or
#' mapped using arguments of the form \code{attribute = ~ expression}.
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'

#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \code{attribute = value},
#'   (b) ggplot2 aesthetics to be mapped with \code{attribute = ~expression}, or
#'   (c) attributes of the layer as a whole, which are set with \code{attribute = value}.
#'   Available attributes include
#'   \code{xintercept}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param show.help If \code{TRUE}, display some minimal help.
#'
#' @seealso \code{\link{geom_vline}()}
#' @export
gf_vline <-
  gf_factory(
    type = "vline", aes_form = NULL,
    extras = alist(xintercept = )
    )

#' @rdname gf_functions0
#' @export
gf_function <- function(object, fun, ...) {
  object + stat_function(fun = fun, ...)
}

#' @rdname gf_functions0
#' @export
gf_fun <- function(object, formula, ...) {
  fun <- function(x, ...) mosaic::makeFun(formula)(x, ...)
  object + stat_function(fun = fun, ...)
}


#' gf_ functions with no formula part
#'
#' These functions provide a formula interface to \code{ggplot2} and
#' various geoms. The formula interface is similar to the one used
#' for \pkg{lattice} plots, but more expressive, and consistent with
#' its use in modeling functions like \code{\link{lm}()}.  These functions
#' can be used to create a complete plot, or they can be chained together
#' using the pipe operator from \pkg{magrittr} to create multi-layer plots.
#' The functions generate a \code{ggplot2} command string which can be
#' displayed by setting \code{verbose = TRUE} as an argument.
#'
#' @seealso \code{\link{gf_histogram}()},
#' \code{\link{gf_point}()},
#' \code{\link{gf_pointrange}()},
#' \code{\link{gf_refine}()}, and the other functions documented with these functions.
#'
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#' @param data A data frame with the variables to be plotted
#' @param gformula mostly ignored,
#' but the environment of this object determines
#' where the \pkg{ggplot2} code is evaluated.
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param coef A numeric vector of length at least 2, treated as intercept and slope.
#' Additional components, if any, are ignored (with a warning).
#' @param model An object with a method for \code{coef()} that returns a
#' numeric vector, the first two elements of which are intercept and slope.
#' This is equivalent to \code{coef = coef(model)}.
#' @param ... Other arguments such as \code{position="dodge"}.
#' @param show.help If \code{TRUE}, display some minimal help.  In particular,
#' the help will show (a) which geom from \pkg{ggplot2} is used,
#' (b) how aesthetics are assigned based on \code{formula}, and (c)
#' any default values of arguments to the geom.
#'
#' @examples
#' mtcars.model <- lm(mpg ~ wt, data = mtcars)
#' gf_point(mpg ~ wt, data = mtcars) %>%
#'   gf_coefline(model = mtcars.model, alpha = 0.6) %>%
#'   gf_hline(yintercept = 20, color = "red", alpha = 0.4) %>%
#'   gf_vline(xintercept = 3, color = "navy", alpha = 0.4)
#' # Chaining in the data
#' mtcars %>% gf_point(mpg ~ wt, data = mtcars) %>%
#'   gf_coefline(model = mtcars.model, alpha = 0.6, col = "red")



#' @rdname gf_functions0
#' @export
gf_coefline <- function(object = NULL, formula = NULL, coef = NULL, model = NULL, ...) {
  if (is.null(coef) + is.null(model) != 1) stop("must specify exactly one of coef or model")
  if (is.null(coef)) coef <- coef(model)
  if (length(coef) > 2) warning("Ignoring all but first two values of coef.")
  if (length(coef) < 2) stop("coef must be of length at least 2.")
  gf_abline(object = object, formula = formula,
            intercept = coef[1], slope = coef[2], ...)
}

