# ggformula 0.4.0

  * Separated `ggformula` from `statisticalModeling`.
  * Added support for many more geoms.
  * Improved parsing of formulas.
  * `data` may now be an expression (like `data = KidsFeet %>% filter(sex == "G")`)
  * Added support for geoms that have different required aesthetics
  * Added support for on-the-fly computations in formulas.
  * Use `::` to indicate mapping aesthetics.  (`:` will autodetect, but only if the value
is the name of a variable in the data set.)
  * Added wrappers `gf_lims()`, `gf_labs()`, `gf_theme()`, `gf_facet_grid()`, `gf_facet_wrap()`
  * Added `gf_refine()` which can be used to pass by chaining anything that would have been "added" in ``ggplot2`
  * Expanded and improved vignette describing use of the package.
  * Added two tutorials.
  * Added "quick help" for plotting functions.
  * Added `gf_lm()`, which is `gf_smooth()` with `method = "lm"`
  * Added `gf_dens()` which is `gf_line()` with `stat = "density"`.



