An R interface to the Enrichr database
================
Wajid Jawaid
2019-08-01

<!-- README.md is generated from README.Rmd. Please edit that file -->



[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/enrichR)](https://cran.r-project.org/package=enrichR) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![CRAN mirror downloads](http://cranlogs.r-pkg.org/badges/enrichR)](https://cran.r-project.org/package=enrichR/)

Installation
============

**enrichR** can be installed from Github or soon from CRAN.

Github
------

``` {.r}
library(devtools)
install_github("wjawaid/enrichR")
```

CRAN
----

The package can be downloaded from CRAN using:

``` {.r}
install.packages("enrichR")
```

Usage example
=============

**enrichR** provides an interface to the Enrichr database [@kuleshov\_enrichr:\_2016] hosted at <http://amp.pharm.mssm.edu/Enrichr>.

First find the list of all available databases from Enrichr.

``` {.r}
library(enrichR)
dbs <- listEnrichrDbs()
```

``` {.r}
head(dbs)
```

||geneCoverage|genesPerTerm|libraryName|link|numTerms|
|---|-----------:|-----------:|:----------|:---|-------:|
|1|13362|275|Genome\_Browser\_PWMs|<http://hgdownload.cse.ucsc.edu/goldenPath/hg18/database/>|615|
|2|27884|1284|TRANSFAC\_and\_JASPAR\_PWMs|<http://jaspar.genereg.net/html/DOWNLOAD/>|326|
|3|6002|77|Transcription\_Factor\_PPIs||290|
|5|47107|509|Drug\_Perturbations\_from\_GEO\_2014|<http://www.ncbi.nlm.nih.gov/geo/>|701|
|6|21493|3713|ENCODE\_TF\_ChIP-seq\_2014|<http://genome.ucsc.edu/ENCODE/downloads.html>|498|

View and select your favourite databases. Then query enrichr, in this case I have used genes associated with embryonic haematopoiesis.

``` {.r}
dbs <- c("GO_Molecular_Function_2015", "GO_Cellular_Component_2015", "GO_Biological_Process_2015")
enriched <- enrichr(c("Runx1", "Gfi1", "Gfi1b", "Spi1", "Gata1", "Kdr"), dbs)
#> Uploading data to Enrichr... Done.
#>   Querying GO_Molecular_Function_2015... Done.
#>   Querying GO_Cellular_Component_2015... Done.
#>   Querying GO_Biological_Process_2015... Done.
#> Parsing results... Done.
```

Now view the results table.

``` {.r}
enriched[["GO_Biological_Process_2015"]]
```

You can give many genes.

``` {.r}
data(genes790)
length(genes790)
head(enrichr(genes790, c('LINCS_L1000_Chem_Pert_up'))[[1]])
```

|Term|Overlap|P.value|Adjusted.P.value|Old.P.value|Old.Adjusted.P.value|Odds.Ratio|Combined.Score|Genes|
|:---|:------|------:|---------------:|----------:|-------------------:|---------:|-------------:|:----|
|embryonic hemopoiesis (GO\_0035162)|3/24|0.0e+00|0.0001573|0|0|416.66667|7213.540|KDR;GATA1;RUNX1|
|regulation of myeloid cell differentiation (GO\_0045637)|4/156|1.0e-07|0.0001370|0|0|85.47009|1432.245|GFI1B;SPI1;GATA1;RUNX1|
|regulation of erythrocyte differentiation (GO\_0045646)|3/36|1.0e-07|0.0001847|0|0|277.77778|4459.220|GFI1B;SPI1;GATA1|
|positive regulation of myeloid cell differentiation (GO\_0045639)|3/74|1.0e-06|0.0012521|0|0|135.13514|1871.822|GFI1B;GATA1;RUNX1|
|hemopoiesis (GO\_0030097)|3/95|2.1e-06|0.0021339|0|0|105.26316|1378.448|KDR;GATA1;RUNX1|
|hematopoietic progenitor cell differentiation (GO\_0002244)|3/106|2.9e-06|0.0024754|0|0|94.33962|1204.196|SPI1;GATA1;RUNX1|

References
==========

Kuleshov, Maxim V., Matthew R. Jones, Andrew D. Rouillard, Nicolas F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, et al. 2016. “Enrichr: A Comprehensive Gene Set Enrichment Analysis Web Server 2016 Update.” Nucleic Acids Res 44 (Web Server issue): W90–W97. <https://doi.org/10.1093/nar/gkw377>.
