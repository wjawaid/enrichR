## Author: Wajid Jawaid
## Date: 17 February 2017
## enrichR package: sends data to http://http://amp.pharm.mssm.edu/Enrichr/ for gene enrichment
## in multiple databases.

##' Look up available databases on Enrichr
##'
##' Look up available databases on Enrichr
##' @title Look up available databases on Enrichr
##' @return dataframe of available Enrichr databases
##' @author Wajid Jawaid
##' @importFrom httr GET POST
##' @importFrom rjson fromJSON
##' @export
listEnrichrDbs <- function() {
    dfSAF <- options()$stringsAsFactors
    options(stringsAsFactors = FALSE)
    dbs <- GET(url="http://amp.pharm.mssm.edu/Enrichr/datasetStatistics")$content
    dbs <- intToUtf8(dbs)
    dbs <- fromJSON(dbs)
    dbs <- lapply(dbs$statistics, function(x) do.call(cbind.data.frame, x))
    dbs <- do.call(rbind.data.frame, dbs)
    options(stringsAsFactors = dfSAF)
    dbs
}

##' Gene enrichment using Enrichr
##'
##' Gene enrichment using Enrichr
##' @title Gene enrichment using Enrichr
##' @param genes Character vector of gene names or dataframe of gene names in
##' in first column and a score between 0 and 1 in the other.
##' @param databases Character vector of databases to search.
##' See http://amp.pharm.mssm.edu/Enrichr/ for available databases.
##' @return Returns a data frame of enrichment terms, p-values, ...
##' @author Wajid Jawaid
##' @importFrom httr GET POST
##' @importFrom rjson fromJSON
##' @importFrom utils read.table
##' @export
enrichr <- function(genes, databases = NULL) {
    ## if (is.null(databases)) {
    ##     dbs <- c("ChEA 2015", "Epigenomics Roadmap HM ChIP-seq",
    ##      "ENCODE and ChEA Consensus TFs from ChIP-X",
    ##      "TF-LOF Expression from GEO", "ENCODE Histone Modifications 2015",
    ##      "Transcription Factor PPIs", "KEGG 2016", "WikiPathways 2016", "CORUM",
    ##      "SILAC Phosphoproteomics", "Humancyc 2016", "NCI-Nature 2016", "Panther 2016",
    ##      "GO Biological Process 2015", "GO Cellular Component 2015",
    ##      "GO Molecular Function 2015",
    ##      "MGI Mammalian Phenotype Level 3", "MGI Mammalian Phenotype Level 4",
    ##      "Human Phenotype Ontology", "OMIM Disease", "OMIM Expanded",
    ##      "Mouse Gene Atlas", "Human Gene Atlas", "Cancer Cell Line Encyclopedia",
    ##      "ESCAPE")
    ##     databases <- gsub(" ", "_", dbs)
    ## }
    cat("Uploading data to Enrichr... ")
    if (is.vector(genes)) {
        temp <- POST(url="http://amp.pharm.mssm.edu/Enrichr/enrich",
                     body=list(list=paste(genes, collapse="\n")))
    } else if (is.data.frame(genes)) {
        temp <- POST(url="http://amp.pharm.mssm.edu/Enrichr/enrich",
                     body=list(list=paste(paste(genes[,1], genes[,2], sep=","),
                                          collapse="\n")))
    } else {
        warning("genes must be a vector of gene names or a dataframe with genes and score.")
    }
    GET(url="http://amp.pharm.mssm.edu/Enrichr/share")
    cat("Done.\n")
    dbs <- as.list(databases)
    dfSAF <- options()$stringsAsFactors
    options(stringsAsFactors = FALSE)
    result <- lapply(dbs, function(x) {
        cat("  Querying ", x, "... ", sep="")
        r <- GET(url="http://amp.pharm.mssm.edu/Enrichr/export",
                 query=list(file="API", backgroundType=x))
        r <- intToUtf8(r$content)
        tc <- textConnection(r)
        r <- read.table(tc, sep = "\t", header = TRUE, quote = "")
        close(tc)
        cat("Done.\n")
        return(r)
    })
    options(stringsAsFactors = dfSAF)
    cat("Parsing results... ")
    names(result) <- dbs
    cat("Done.\n")
    return(result)
}

##' Print Enrichr output.
##'
##' Print Enrichr output to text file.
##' @title Print Enrichr output to text file.
##' @param data Output from Enrichr function.
##' @param file Name of output file.
##' @param sep Default TAB. How to separate fields.
##' @param columns Columns from each entry of data.
##' 1-"Index", 2-"Name", 3-"Adjusted_P-value", 4-"Z-score"         
##' 5-"Combined_Score", 6-"Genes", 7-"Overlap_P-value" 
##' @return Produces file.
##' @author Wajid Jawaid
##' @export
printEnrich <- function (data, file, sep = "\t", columns = c(2,3,6)) {
    enrich <- file(file, "w")
    for (i in 1:length(data)) {
        writeLines(names(data)[i], enrich)
        n <- nrow(data[[i]])
        if (n > 10) n <- 10
        if (n > 0) {
            writeLines(paste(apply(data[[i]][1:n, columns, drop=FALSE], 1,
                                   function(x) paste(x, collapse = sep)),
                             collapse = "\n"), enrich)
        writeLines("\n", enrich)
        }
    }
    close(enrich)
}
