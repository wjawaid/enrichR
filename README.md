An R interface to the Enrichr database
================
Wajid Jawaid
2017-04-02

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/enrichR)](https://cran.r-project.org/package=enrichR)

Installation
============

**enrichR** can be installed from Github or soon from CRAN.

Github
------

``` r
library(devtools)
install_github("wjawaid/enrichR")
```

CRAN
----

The package can be downloaded from CRAN using:

``` r
install.packages("enrichR")
```

Usage example
=============

**enrichR** provides an interface to the Enrichr database (Kuleshov et al. 2016) hosted at <http://amp.pharm.mssm.edu/Enrichr>.

First find the list of all available databases from Enrichr.

``` r
library(enrichR)
dbs <- listEnrichrDbs()
```

``` r
head(dbs)
```

|     | libraryName                          |  numTerms|  geneCoverage|  genesPerTerm| link                                                       |
|-----|:-------------------------------------|---------:|-------------:|-------------:|:-----------------------------------------------------------|
| 1   | Genome\_Browser\_PWMs                |       615|         13362|           275| <http://hgdownload.cse.ucsc.edu/goldenPath/hg18/database/> |
| 2   | TRANSFAC\_and\_JASPAR\_PWMs          |       326|         27884|          1284| <http://jaspar.genereg.net/html/DOWNLOAD/>                 |
| 3   | Transcription\_Factor\_PPIs          |       290|          6002|            77|                                                            |
| 5   | Drug\_Perturbations\_from\_GEO\_2014 |       701|         47107|           509| <http://www.ncbi.nlm.nih.gov/geo/>                         |
| 6   | ENCODE\_TF\_ChIP-seq\_2014           |       498|         21493|          3713| <http://genome.ucsc.edu/ENCODE/downloads.html>             |

View and select your favourite databases. Then query enrichr, in this case I have used genes associated with embryonic haematopoiesis.

``` r
dbs <- c("GO_Molecular_Function_2015", "GO_Cellular_Component_2015", "GO_Biological_Process_2015")
enriched <- enrichr(c("Runx1", "Gfi1", "Gfi1b", "Spi1", "Gata1", "Kdr"), dbs)
#> Uploading data to Enrichr... Done.
#>   Querying GO_Molecular_Function_2015... Done.
#>   Querying GO_Cellular_Component_2015... Done.
#>   Querying GO_Biological_Process_2015... Done.
#> Parsing results... Done.
```

Now view the results table.

``` r
enriched[["GO_Biological_Process_2015"]]
```

| Term                                                             | Overlap |   P.value|  Adjusted.P.value|  Old.P.value|  Old.Adjusted.P.value|    Z.score|  Combined.Score| Genes                  |
|:-----------------------------------------------------------------|:--------|---------:|-----------------:|------------:|---------------------:|----------:|---------------:|:-----------------------|
| embryonic hemopoiesis (GO\_0035162)                              | 3/24    |  0.00e+00|         0.0000083|     1.00e-07|             0.0000355|  -2.869798|        33.56095| KDR;GATA1;RUNX1        |
| regulation of erythrocyte differentiation (GO\_0045646)          | 3/36    |  1.00e-07|         0.0000112|     4.00e-07|             0.0000395|  -2.503155|        28.52580| GFI1B;SPI1;GATA1       |
| regulation of myeloid cell differentiation (GO\_0045637)         | 4/156   |  1.00e-07|         0.0000083|     2.00e-07|             0.0000355|  -2.325462|        27.19519| GFI1B;SPI1;GATA1;RUNX1 |
| positive regulation of granulocyte differentiation (GO\_0030854) | 2/10    |  3.40e-06|         0.0001522|     9.70e-06|             0.0004382|  -2.728754|        23.98657| GFI1B;RUNX1            |
| regulation of granulocyte differentiation (GO\_0030852)          | 2/17    |  1.02e-05|         0.0002681|     2.51e-05|             0.0006716|  -2.824466|        23.22908| GFI1B;RUNX1            |
| granulocyte differentiation (GO\_0030851)                        | 2/15    |  7.90e-06|         0.0002484|     2.00e-05|             0.0006314|  -2.779246|        23.06870| SPI1;GATA1             |

References
==========

Kuleshov, Maxim V., Matthew R. Jones, Andrew D. Rouillard, Nicolas F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, et al. 2016. “Enrichr: A Comprehensive Gene Set Enrichment Analysis Web Server 2016 Update.” *Nucleic Acids Res* 44 (Web Server issue): W90–W97. doi:[10.1093/nar/gkw377](https://doi.org/10.1093/nar/gkw377).
