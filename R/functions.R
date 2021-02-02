## Author: Wajid Jawaid
## Date: 1 August 2019
## enrichR package: sends data to https://maayanlab.cloud/Enrichr/ for gene enrichment
## in multiple databases.

##' onLoad hook to setup package options
##'
##' onLoad hook to setup package options and to check connection to website
##' @title onLoad hook to setup package options
##' @param libname (Required). Library name
##' @param pkgname (Required). Package name
##' @return NULL
##' @author Wajid Jawaid \email{wj241@alumni.cam.ac.uk}
.onAttach <- function(libname, pkgname) {
    options(enrichR.base.address = "https://maayanlab.cloud/Enrichr/")
    options(enrichR.live = TRUE)
    packageStartupMessage("Welcome to enrichR\nChecking connection ... ", appendLF = TRUE)
    options(modEnrichR.use = TRUE)
    options(enrichR.sites.base.address = "https://maayanlab.cloud/")
    options(enrichR.sites = c("Enrichr", "FlyEnrichr", "WormEnrichr", "YeastEnrichr", "FishEnrichr"))
    if (getOption("modEnrichR.use")) {
        listEnrichrSites()
    } else {
        getEnrichr(url=paste0(getOption("enrichR.base.address"), "datasetStatistics"))
        packageStartupMessage("Enrichr ... ", appendLF = FALSE)
        if (getOption("enrichR.live")) packageStartupMessage("Connection is Live!")
    }
}


##' Helper function
##'
##' Helper function for GET
##' @title Helper function for GET
##' @param url (Required). URL address requested
##' @param ... (Optional). Additional parameters to pass to GET
##' @return same as GET
##' @author Wajid Jawaid \email{wj241@alumni.cam.ac.uk}
##' @importFrom httr GET
getEnrichr <- function(url, ...) {
    tryCatch({
        options(enrichR.live = TRUE)
        x <- GET(url=url, ...)
    },
    error = function(err_msg) {
        message("EnrichR website not responding")
        options(enrichR.live = FALSE)
    },
    finally = function() {
       invisible(x) 
    })
}

##' List modEnrichr Websites
##'
##' List Enrichr Websites
##' @title List Enrichr Websites
##' @return print Enrichr Website status
##' @author Alexander Blume
##' @param ... (Optional  Additional parameters)
##' @export
listEnrichrSites <- function(...) {
    for (site in getOption("enrichR.sites")) {
        getEnrichr(url = paste0(getOption("enrichR.sites.base.address"), site, "/", "datasetStatistics"))
        packageStartupMessage(paste0(site, " ... "), appendLF = FALSE)
        if (paste0(getOption("enrichR.sites.base.address"), site, "/")  == getOption("enrichR.base.address")) {
            if (getOption("enrichR.live")) packageStartupMessage("Connection is Live!")
        } else 
            if (getOption("enrichR.live")) packageStartupMessage("Connection is available!")
        
    }
}

##' Set Enrichr Website
##'
##' Set Enrichr Website
##' @title Set Enrichr Website
##' @param site site requested
##' @return Changes Enrichr Website connection
##' @author Alexander Blume
##' @export
setEnrichrSite <- function(site) {
    site <- gsub(getOption("enrichR.sites.base.address"), "", site)
    matched <- grep(paste0("^",site), 
                    getOption("enrichR.sites"),
                    ignore.case = TRUE,value = FALSE)
    if( length(matched) == 0 ) {
        message("Given website does not match available sites: ", site)
        message("Choose from:\n",
                paste("-",getOption("enrichR.sites"),"\n"))
    } else if (length(matched) > 1) {
        message("Given website matches multiple options: ", site)
        message(paste("-", getOption("enrichR.sites")[matched],"\n"),)        
    } else {
        site <- getOption("enrichR.sites")[matched]
        options(enrichR.base.address = paste0(getOption("enrichR.sites.base.address"),site,"/"))
        message("Connection changed to ",paste0(getOption("enrichR.sites.base.address"),site,"/"))
        getEnrichr(url = paste0(getOption("enrichR.base.address"),"datasetStatistics"))
        if (getOption("enrichR.live")) message("Connection is Live!")
    }
}

##' Look up available databases on Enrichr
##'
##' Look up available databases on Enrichr
##' @title Look up available databases on Enrichr
##' @return A data.frame of available Enrichr databases
##' @author Wajid Jawaid \email{wj241@alumni.cam.ac.uk}
##' @importFrom httr GET POST
##' @importFrom rjson fromJSON
##' @export
##' @examples
##' dbs <- listEnrichrDbs()
listEnrichrDbs <- function() {
    dfSAF <- getOption("stringsAsFactors")
    options(stringsAsFactors = FALSE)
    dbs <- getEnrichr(url = paste0(getOption("enrichR.base.address"), "datasetStatistics"))
    if (!getOption("enrichR.live")) return()
    ## if (length(dbs) == 1) {
    ##     if (dbs == "FAIL") {
    ##         return()
    ##     }
    ## }
    dbs <- dbs$content
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
##' @param genes (Required). Character vector of gene names or data.frame of gene names in
##' in first column and a score between 0 and 1 in the other.
##' @param databases (Required). Character vector of databases to search.
##' See https://maayanlab.cloud/Enrichr/ for available databases.
##' @return Returns a list of data.frame of enrichment terms, p-values, ...
##' @author Wajid Jawaid \email{wj241@alumni.cam.ac.uk}
##' @importFrom httr GET POST
##' @importFrom rjson fromJSON
##' @importFrom utils read.table
##' @export
##' @examples
##' dbs <- listEnrichrDbs()
##' dbs <- c("GO_Molecular_Function_2018", "GO_Cellular_Component_2018", 
##'          "GO_Biological_Process_2018")
##' enriched <- enrichr(c("Runx1", "Gfi1", "Gfi1b", "Spi1", "Gata1", "Kdr"), dbs)
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
    if (length(genes) < 1) {
        message("No genes have been given")
        return()
    }
    x_text <- getEnrichr(url = getOption("enrichR.base.address"))
    if (!getOption("enrichR.live")) return()
    if (is.null(databases)) {
        message("No databases have been provided")
        return()
    }
    cat("Uploading data to Enrichr... ")
    if (is.vector(genes) & ! all(genes == "") & length(genes) != 0) {
        temp <- POST(url=paste0(getOption("enrichR.base.address"), "enrich"),
                     body=list(list=paste(genes, collapse="\n")))
    } else if (is.data.frame(genes)) {
        temp <- POST(url=paste0(getOption("enrichR.base.address"), "enrich"),
                     body=list(list=paste(paste(genes[,1], genes[,2], sep=","),
                                          collapse="\n")))
    } else {
        warning("genes must be a non-empty vector of gene names or a data.frame with genes and score.")
    }
    getEnrichr(url=paste0(getOption("enrichR.base.address"), "share"))
    cat("Done.\n")
    dbs <- as.list(databases)
    dfSAF <- getOption("stringsAsFactors")
    options(stringsAsFactors = FALSE)
    result <- lapply(dbs, function(x) {
        cat("  Querying ", x, "... ", sep="")
        r <- getEnrichr(url=paste0(getOption("enrichR.base.address"), "export"),
                        query=list(file="API", backgroundType=x))
        if (!getOption("enrichR.live")) return()
        r <- gsub("&#39;", "'", intToUtf8(r$content))
        tc <- textConnection(r)
        r <- read.table(tc, sep = "\t", header = TRUE, quote = "", comment.char="")
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

## Given a Enrichr output, order and subset criteria, returns a data frame accordingly
.enrichment_prep_df <- function(df, showTerms, orderBy) {

    if(is.null(showTerms)) {
        showTerms = nrow(df)
    } else if(!is.numeric(showTerms)) {
        stop(paste0("showTerms '", showTerms, "' is invalid."))
    }

    Annotated <- as.numeric(sub("^\\d+/", "", as.character(df$Overlap)))
    Significant <- as.numeric(sub("/\\d+$", "", as.character(df$Overlap)))

    # Build data frame
    df <- cbind(df, data.frame(Annotated = Annotated, Significant = Significant,
                               stringsAsFactors = FALSE))

    # Order data frame (P.value or Combined.Score)
    if(orderBy == "Combined.Score") {
        idx <- order(df$Combined.Score, decreasing = TRUE)
    } else {
        idx <- order(df$P.value, decreasing = FALSE)
    }
    df <- df[idx,]

    # Subset to selected number of terms
    if(showTerms <= nrow(df)) {
        df <- df[1:showTerms,]
    }

    return(df)
}

##' Print Enrichr output.
##'
##' Print Enrichr output to text file.
##' @title printEnrich
##' @param data (Required). Output from Enrichr function.
##' @param prefix (Optional). Prefix of output file. Default is \code{"enrichr"}.
##' @param showTerms (Optional). Number of terms to show. 
##' Default is \code{NULL} to print all terms.
##' @param columns (Optional). Columns from each entry of data. 
##' Default is \code{c(1:9)} to print all columns.
##' 1-"Term", 2-"Overlap", 3-"P.value", 4-"Adjusted.P.value" 5-"Old.P.value", 
##' 6-"Old.Adjusted.P.value" 7-"Odds.Ratio" 8-"Combined.Score" 9-"Combined.Score"
##' @return NULL
##' @author Wajid Jawaid \email{wj241@alumni.cam.ac.uk}
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @importFrom utils write.table
##' @export
##' @examples
##' if (getOption("enrichR.live")) {
##'   enrichRLive <- TRUE
##'   dbs <- listEnrichrDbs()
##'   if(is.null(dbs)) enrichRLive <- FALSE
##'   dbs <- c("GO_Molecular_Function_2018", "GO_Cellular_Component_2018", 
##'            "GO_Biological_Process_2018")
##'   enriched <- enrichr(c("Runx1", "Gfi1", "Gfi1b", "Spi1", "Gata1", "Kdr"), dbs)
##'   if (enrichRLive) printEnrich(enriched)
##' }
printEnrich <- function(data, prefix = "enrichr", showTerms = NULL, columns = c(1:9)) {
    if (!is.list(data)) stop("data is malformed must be a list")
    if(!is.numeric(columns)) {
        stop(paste0("columns '", columns, "' is invalid."))
    }

    for (i in 1:length(data)) {
        dbname <- names(data)[i]
        df <- data[[i]]
        if (!is.data.frame(df)) stop("data is malformed - it must consist of data frames")

        df <- .enrichment_prep_df(df, showTerms, orderBy = "P.value")
        df <- df[, !colnames(df) %in% c("Annotated", "Significant")]

        if(any(columns > ncol(df))) {
            stop("Undefined columns selected")
        }
    }
}

##' Visualise a Enrichr output as barplot
##'
##' Print Enrichr output to text file.
##' @title plotEnrich
##' @param df (Required). A single data.frame from a list of Enrichr output.
##' @param showTerms (Optional). Number of terms to show. Default is \code{20}.
##' @param numChar (Optional). A single integer. Default is \code{40}.
##' Indicates the number characters to keep in the term description.
##' @param y (Optional). A character string. Default is \code{"Count"}.
##' Indicates the variable that should be mapped to the y-axis.
##' It can be either \code{"Count"} or \code{"Ratio"}.
##' @param orderBy (Optional). A character string. Default is \code{"P.value"}.
##' Indicates how to order the Enrichr results before subsetting to keep top \code{N} terms.
##' It can be either \code{"P.value"} or \code{"Combined.Score"}.
##' @param xlab (Optional). A character string. Default is \code{NULL}.
##' Indicates the x-axis label.
##' @param ylab (Optional). A character string. Default is \code{NULL}.
##' Indicates the y-axis label.
##' @param title (Optional). A character string. Default is \code{NULL}
##' Indicates the main title for the graphic.
##' @return A \code{\link{ggplot}}2 plot object
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @seealso
##' \code{\link[ggplot2]{ggplot}}
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_string
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 coord_flip
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 scale_fill_continuous
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 guide_colorbar
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 margin
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 ggtitle
##' @export
##' @examples
##' if (getOption("enrichR.live")) {
##'   dbs <- listEnrichrDbs()
##'   enrichRLive <- TRUE
##'   if (is.null(dbs)) enrichRLive <- FALSE
##'   dbs <- c("GO_Molecular_Function_2018", "GO_Cellular_Component_2018", 
##'            "GO_Biological_Process_2018")
##'   enriched <- enrichr(c("Runx1", "Gfi1", "Gfi1b", "Spi1", "Gata1", "Kdr"), dbs)
##'   # Plot top 20 GO-BP results ordered by P-value
##'   if (enrichRLive) {
##'     plotEnrich(enriched[[3]], showTerms = 20, numChar = 50, y = "Count",
##'                orderBy = "P.value")
##'   }
##' }
plotEnrich <- function(df, showTerms = 20, numChar = 40, y = "Count", orderBy = "P.value",
                       xlab = NULL, ylab = NULL, title = NULL) {
    if(!is.data.frame(df)) stop("df is malformed - must be a data frame")
    if(!is.numeric(numChar)) {
        stop(paste0("numChar '", numChar, "' is invalid."))
    }

    df <- .enrichment_prep_df(df, showTerms, orderBy)

    # Create trimmed name (as seen in topGO)
    shortName <- paste(substr(df$Term, 1, numChar),
                       ifelse(nchar(df$Term) > numChar, '...', ''), sep = '')
    df$shortName = shortName
    df$shortName <- factor(df$shortName, levels = rev(unique(df$shortName)))
    df$Ratio <- df$Significant/df$Annotated

    # Define fill variable (P.value or Combined.Score)
    if(orderBy == "Combined.Score") {
        fill <- "Combined.Score"
    } else {
        fill <- "P.value"
    }

    # Define y variable (Count or Ratio)
    if(y != "Ratio") {
        y <- "Significant"
    }

    # Define variable mapping
    map <- aes_string(x = "shortName", y = y, fill = fill)

    # Define labels
    if(is.null(xlab)) {
        xlab <- "Enriched terms"
    }

    if(is.null(ylab)) {
        if(y == "Ratio") {
            ylab <- "Gene ratio"
        } else {
            ylab <- "Gene count"
        }
    }

    if(is.null(title)) {
        title <- "Enrichment analysis by Enrichr"
    }

    # Make the ggplot
    p <- ggplot(df, map) + geom_bar(stat = "identity") + coord_flip() + theme_bw()

    if(orderBy == "Combined.Score") {
        p <- p + scale_fill_continuous(low = "blue", high = "red") +
                guides(fill = guide_colorbar(title = "Combined Score", reverse = FALSE))
    } else {
        p <- p + scale_fill_continuous(low = "red", high = "blue") +
                guides(fill = guide_colorbar(title = "P value", reverse = TRUE))
    }

    # Adjust theme components
    p <- p + theme(axis.text.x = element_text(colour = "black", vjust = 1),
                   axis.text.y = element_text(colour = "black", hjust = 1),
                   axis.title = element_text(color = "black", margin = margin(10, 5, 0, 0)),
                   axis.title.y = element_text(angle = 90))

    p <- p + xlab(xlab) + ylab(ylab) + ggtitle(title)

    return(p)
}

