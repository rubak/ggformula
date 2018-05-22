## Test environments

* local OS X install 
  * R version 3.5.0 (2018-04-23)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS High Sierra 10.13.4

* win-builder via devtools::build_win()

## R CMD check results (local)

R CMD check results

0 errors | 0 warnings | 0 notes


## Downstream dependencies

Checked with `devtools::revdep_check()`.  

Error in `mosaicCore` and warning in `mosaic` should go away once `mosaic`, `mosaicCore` 
and `ggformula` are all updated.  All three check cleaning against each other in the
versions being submitted.



