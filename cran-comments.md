## Test environments

* local OS X install 
  * R version 3.5.1 (2018-07-02)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS High Sierra 10.13.6

* win-builder via devtools::build_win()

## R CMD check results (local)

R CMD check results

0 errors | 0 warnings | 0 notes


## Downstream dependencies

Checked with `devtools::revdep_check()`.  

Checked fastR2     : 0 errors | 0 warnings | 0 notes
Checked mosaic     : 0 errors | 0 warnings | 0 notes
Checked mosaicCore : 0 errors | 0 warnings | 0 notes
Checked mosaicData : 0 errors | 0 warnings | 0 notes
Checked mosaicModel: 0 errors | 0 warnings | 0 notes
Checked supernova  : 0 errors | 0 warnings | 0 notes

## Comments

This is a resubmission.  I've removed the (unnecessary) import of dplyr (it is now in
suggests).  The error found in the mosaic package in your previous checks is unrelated
to ggformula and I can't reproduce it locally or in win builder.

I hope that means we are good to go.


