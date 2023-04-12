# enrichR v3.2
 - Fixed further problems with errors when internet connection was not available
 - Automated bibliography using pandoc and .bib files
 - Added optional file writing for printEnrich().
 - Changed all getOption("stringsAsFactors") to getOption("stringsAsFactors", FALSE)
# enrichR v3.1
 - Changed to gracefully detect internet connection
# enrichR v3.0
 - Updated enrichR.base.address
  - Included additional enrichr sites including FlyEnrichr, WormEnrichr, YeastEnrichR and FishEnrichR (Thanks to Alexander Blume)
 - Ability to generate a plot from the enrichment results (Thanks to I-Hsuan Lin)
# enrichR v2.1
 - Fixed bug by adding .onLoad hook to setup initial options
 - Added bibliography directly and removed .bib as it was causing pandoc-citeproc problems with README.Rmd and vignette.
 - Corrected spelling error in vignette
# enrichR v2.0.
 - Contains checks on enrichR website to ensure it is online.
