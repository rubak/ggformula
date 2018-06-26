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

Checked fastR2     : 0 errors | 1 warning  | 0 notes
Checked mosaic     : 0 errors | 0 warnings | 1 note 
Checked mosaicCore : 0 errors | 0 warnings | 0 notes
Checked mosaicData : 0 errors | 0 warnings | 0 notes
Checked mosaicModel: 0 errors | 0 warnings | 0 notes
Checked supernova  : 0 errors | 0 warnings | 0 notes

An updated fastR2 will be heading to CRAN in the next couple days. (The reported
issues has to do with ggplot2 now exporting a stat() when mosaic already has had
a stat() function for some time.  The new mosaic::stat() is ggplot2::stat() aware.)

The mosaic note is about the size of the package which is riding right at the 5
Mb limit, sometimes just under, sometimes just over.  An updated mosaic will be
coming to CRAN very soon (before fastR2).

