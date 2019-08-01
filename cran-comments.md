## Test environments
* local OS X install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* Version 2.1
 - Fixed bug by adding .onLoad hook to setup initial options
 - Added bibliography directly and removed .bib as it was causing pandoc-citeproc problems with README.Rmd and vignette.
 - Corrected spelling error in vignette
* This is version 2.0.
 - Contains checks on enrichR website to ensure it is online.
