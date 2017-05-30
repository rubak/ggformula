#' Bivariate gf_ plotting functions
#'
#' These functions provide a formula interface to \code{ggplot2} and
#' various geoms. For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and mosaic notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' Formulas must specify the \code{y} and \code{x} aesthetics in the form \code{y ~ x}.
#' Additional terms of the form \code{attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}.
#' Attributes can also be set by including optional arguments of the form
#' \code{attribute = value}.
#'
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{\link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{formula}.  This
#' will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#'
#' @seealso  \code{\link{gf_histogram}()}, \code{\link{gf_abline}()}, \code{\link{gf_pointrange}()}, \code{\link{gf_refine}()},
#' and the other functions documented with these functions.
#'
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#' @param data A data frame with the variables to be plotted
#' @param gformula A formula describing the x and y variables and other aesthetics in
#' a form like \code{y ~ x + color:"red" + shape:sex + alpha:0.5}.
#' The environment of \code{gformula} determines
#' where the \pkg{ggplot2} code is evaluated.
#' See details.
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"}.
#' @param show.help If \code{TRUE}, display some minimal help.  In particular,
#' the help will show (a) which geom from \pkg{ggplot2} is used,
#' (b) how aesthetics are assigned based on \code{formula}, and (c)
#' any default values of arguments to the geom.
#'
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
#' @rdname gf_functions
#' @export
gf_point <- gf_factory(type = "point")

#' @rdname gf_functions
#' @export
gf_jitter <- gf_factory(type = "jitter")

#' @rdname gf_functions
#' @export
gf_line <- gf_factory(type = "line")

#' @rdname gf_functions
#' @export
gf_path <- gf_factory(type = "path")

#' @rdname gf_functions
#' @export
gf_smooth <- gf_factory(type = "smooth")

#' @rdname gf_functions
#' @export
gf_spline <- gf_factory(type = "spline")

#' @rdname gf_functions
#' @export
gf_raster <- gf_factory(type = "raster")

#' @rdname gf_functions
#' @export
gf_quantile <- gf_factory(type = "quantile")

#' @rdname gf_functions
#' @export
gf_density_2d <- gf_factory(type = "density_2d")

#' @rdname gf_functions
#' @export
gf_density2d <- gf_factory(type = "density2d")

#' @rdname gf_functions
#' @export
gf_hex <- gf_factory(type = "hex")

#' @rdname gf_functions
#' @export
gf_boxplot <- gf_factory(type = "boxplot")

#' @rdname gf_functions
#' @export
gf_text <- gf_factory(type = "text")

#' @rdname gf_functions
#' @export
gf_label <- gf_factory(type = "label")

#' @rdname gf_functions
#' @export
gf_area <- gf_factory(type = "area")

#' @rdname gf_functions
#' @export
gf_violin <- gf_factory(type = "violin")

#' @rdname gf_functions
#' @export
gf_spoke <- gf_factory(type = "spoke")

#' @rdname gf_functions
#' @export
gf_step <- gf_factory(type = "step")

#' @rdname gf_functions
#' @export
gf_tile <- gf_factory(type = "tile")

#' @rdname gf_functions
#' @export
gf_count <- gf_factory(type = "count")

#' @rdname gf_functions
#' @export
gf_col <- gf_factory(type = "col")

#' @rdname gf_functions
#' @export
gf_frame <- gf_factory(type = "blank")

#' @rdname gf_functions
#' @export
gf_histogram2 <- gf_factory(type = "histogram", aes_form = y ~ x)



#' Univariate gf_ plotting functions
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
#' Formulas must specify the \code{x} aesthetic in the form \code{~x}.
#' Additional terms of the form \code{attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}.
#' Attributes can also be set by including optional arguments of the form
#' \code{attribute = value}.
#'
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{formula}.  This
#' will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#'
#' @seealso \code{\link{gf_point}()}, \code{\link{gf_abline}()}, \code{\link{gf_pointrange}()}, \code{\link{gf_refine}()}, and the other functions documented with these functions.
#'
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#' @param data A data frame with the variables to be plotted
#' @param gformula A formula describing the x variable and other aesthetics in
#' a form like \code{ ~ x + color:red + fill:gray50 + alpha:0.5}.
#' The environment of \code{gformula} determines
#' where the \pkg{ggplot2} code is evaluated. See details.
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"}.
#' @param show.help If \code{TRUE}, display some minimal help.  In particular,
#' the help will show (a) which geom from \pkg{ggplot2} is used,
#' (b) how aesthetics are assigned based on \code{gformula}, and (c)
#' any default values of arguments to the geom.
#'
#' @examples
#' gf_dens(show.help = TRUE)
#' gf_histogram(~ Sepal.Length | Species, data = iris, binwidth = 0.25)
#' gf_density(~ Sepal.Length + color:Species, data = iris)
#' gf_dens(~ Sepal.Length + color:Species, data = iris)
#' gf_freqpoly(~ Sepal.Length + color:Species, data = iris)
#' gf_dotplot(~ Sepal.Length + fill:Species, data = iris)
#' # Chaining in the data
#' iris %>% gf_dens(~ Sepal.Length + color:Species)

#' @rdname gf_functions1
#' @export
gf_histogram <- gf_factory(type = "histogram", aes_form = ~x)

#' @rdname gf_functions1
#' @export
gf_density <- gf_factory(type = "density", aes_form = ~ x)

# modified version of density plot without line along bottom and sides
#' @rdname gf_functions1
#' @export
gf_dens <- gf_factory(type = "line", extras = list(stat = "density"),
                      aes_form = ~ x)

#' @rdname gf_functions1
#' @export
gf_dotplot <- gf_factory(type = "dotplot", aes_form = ~x)

#' @rdname gf_functions1
#' @export
gf_bar <- gf_factory(type = "bar", aes_form = ~ x)

#' @rdname gf_functions1
#' @export
gf_freqpoly <- gf_factory(type = "freqpoly", aes_form = ~ x)

#' @rdname gf_functions1
#' @export
gf_qq <- gf_factory(type = "qq", aes_form = ~ sample)

#' @rdname gf_functions1
#' @export
gf_rug <- gf_factory(type = "rug", aes_form = ~ x)

#' Multivariate gf_ plotting functions
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
#' Formulas must specify the required aesthetics of the underlying
#' \pkg{ggplot2} function.  Use, for example,  \code{gf_ribbon(show.help = TRUE)}
#' to see the formula specification required (along with any other default values
#' passed to the geom).
#' Additional terms of the form \code{attribute::value} map \code{attribute}
#' to \code{value}.
#' Additional terms of the form \code{attribute:value} will map \code{attribute}
#' to \code{value} if \code{value} is the name of a variable in \code{data}, else
#' \code{attribute} will be set to the constant \code{value}.
#' Attributes can also be set by including optional arguments of the form
#' \code{attribute = value}.
#'
#' In formulas of the form \code{A | B}, \code{B} will be used to form facets using
#' \code{link{facet_wrap}()} or \code{\link{facet_grid}()}.
#' This provides an alternative to
#' \code{\link{gf_facet_wrap}()} and
#' \code{\link{gf_facet_grid}()} that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.  This
#' will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#'
#' @seealso \code{\link{gf_histogram}()}, \code{\link{gf_point}()}, \code{\link{gf_abline}()}, \code{\link{gf_refine}()},
#' and the other functions documented with these functions.
#'
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#' @param data A data frame with the variables to be plotted
#' @param gformula A formula describing the manditory aesthetics and possibly other
#' aesthetics in a form like \code{ y + ymin + ymax ~ x + color:red + fill:gray50 + alpha:0.5}.
#' The environment of \code{gformula} determines
#' where the \pkg{ggplot2} code is evaluated. See details.
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"}.
#' @param show.help If \code{TRUE}, display some minimal help.  In particular,
#' the help will show (a) which geom from \pkg{ggplot2} is used,
#' (b) how aesthetics are assigned based on \code{formula}, and (c)
#' any default values of arguments to the geom.
#'
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

#' @rdname gf_functions3
#' @export
gf_raster2 <- gf_factory(type = "raster", aes_form = fill ~ x + y)

#' @rdname gf_functions3
#' @export
gf_contour <- gf_factory(type = "contour", aes_form = z ~ x + y)

#' @rdname gf_functions3
#' @export
gf_ribbon <- gf_factory(type = "ribbon", aes_form = ymin + ymax ~ x,
                        extras = list(alpha = 0.3))

#' @rdname gf_functions3
#' @export
gf_curve <- gf_factory(type = "curve", aes_form = y + yend ~ x + xend)

#' @rdname gf_functions3
#' @export
gf_segment <- gf_factory(type = "segment", aes_form = y + yend ~ x + xend)

#' @rdname gf_functions3
#' @export
gf_linerange <- gf_factory(type = "linerange", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_pointrange <- gf_factory(type = "pointrange", aes_form = y + ymin + ymax ~ x,
                            extras = list(fatten = 2))

#' @rdname gf_functions3
#' @export
gf_crossbar <- gf_factory(type = "crossbar", aes_form = y + ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_errorbar <- gf_factory(type = "errorbar", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_errorbarh <- gf_factory(type = "errorbarh", aes_form = y ~ x + xmin + xmax)

#' @rdname gf_functions3
#' @export
gf_rect <- gf_factory(type = "rect", aes_form = ymin + ymax ~ xmin + xmax)


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
gf_abline <- gf_factory(type = "abline", aes_form = NULL)

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

#' @rdname gf_functions0
#' @export
gf_hline <- gf_factory(type = "hline", aes_form = NULL)

#' @rdname gf_functions0
#' @export
gf_vline <- gf_factory(type = "vline", aes_form = NULL)

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


