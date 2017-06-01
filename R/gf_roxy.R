gf_roxy <- function(
  type,
  aes_form = y ~ x,
  extras = alist(),
  note = NULL
) {
  do.call(
    glue::glue,
    c(
      list(
        .sep = "\n",
        type = type,
        aes_form = aes_form,
        extras = extras,
        note = note),
      "\n",
      "#' Formula interface to geom_{type}()",
      "#' ",
      "#' \\pkg{{ggformula}} functions provide a formula interface to \\code{{ggplot2}} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \\pkg{{mosaic}} notation.
#' The functions generate a \\code{{ggplot}} command string which can be displayed by
#' setting \\code{{verbose = TRUE}} as an argument.
#'
#' Positional aesthetics are typically specified using a formula (see the \\code{{gformula}} argument).
#' Additional formula terms of the form \\code{{+ attribute::value}} map \\code{{attribute}}
#' to \\code{{value}}.
#' Additional terms of the form \\code{{+ attribute:value}} will map \\code{{attribute}}
#' to \\code{{value}} if \\code{{value}} is the name of a variable in \\code{{data}}, else
#' \\code{{attribute}} will be set to the constant \\code{{value}}. Alternatively (and preferably)
#' attributes can be set can be set using arguments of the form \\code{{attribute = value}} or
#' mapped using arguments of the form \\code{{attribute = ~ expression}}.",
      "#' In formulas of the form \\code{{A | B}}, \\code{{B}} will be used to form facets using
#' \\code{{\\link{{facet_wrap}}()}} or \\code{{\\link{{facet_grid}}()}}.
#' This provides an alternative to
#' \\code{{\\link{{gf_facet_wrap}}()}} and
#' \\code{{\\link{{gf_facet_grid}}()}} that is terser and may feel more familiar to users
#' of \\pkg{{lattice}}.
#'
#' Evaluation of the \\pkg{{ggplot2}} code occurs in the environment of \\code{{gformula}}.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.",
      "#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#' ",
      if (is.null(aes_form)) "" else "#' @param gformula A formula with shape {format(aes_form)}.
#'   Faceting can be acheived by including \\code{{|}} in the formula.",
      "#' @param data A data frame with the variables to be plotted.",
      "#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with \\code{{attribute = value}},
#'   (b) ggplot2 aesthetics to be mapped with \\code{{attribute = ~expression}}, or
#'   (c) attributes of the layer as a whole, which are set with \\code{{attribute = value}}.
#'   Available attributes include ",
      paste("#'  ", paste("\\code{{",names(extras), "}}", sep = "", collapse = ", ")),
      "#' @param add If \\code{{TRUE}} then construct just the layer with no frame.  The result
#'   can be added to an existing frame.",
      "#' @param verbose If \\code{{TRUE}} print the ggplot2 command in the console.",
      "#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.",
      "#' @param show.help If \\code{{TRUE}}, display some minimal help.",
      if (is.null(note)) "#'" else "#' @section Note {note}",
      "#' @seealso \\code{{\\link{{geom_{type}}}()}}",
      " "
    )
  )
}

  gf_roxy(
    type = "point",
    extras = alist(alpha = ,  color = , size = , shape = , fill = , group = , stroke = )
  )

  gf_roxy(
    type = "jitter",
    extras = alist(alpha = ,  color = , size = , shape = , fill = , group = , stroke = ,
                   width =, height = )
  )

  gf_roxy(
    type = "line",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = ,
                   lineend = , linejoin = , linemitre = , arrow = )
  )
