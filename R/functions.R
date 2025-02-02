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
##' @author Wajid Jawaid \email{wajid.jawaid@gmail.com}
##' @importFrom curl has_internet
.onAttach <- function(libname, pkgname) {
    options(enrichR.sites.base.address = "https://maayanlab.cloud/")
    options(enrichR.base.address = paste0(getOption("enrichR.sites.base.address"), "Enrichr/"))
    options(speedrichr.base.address = paste0(getOption("enrichR.sites.base.address"), "speedrichr/api/"))
    options(enrichR.live = TRUE)
    options(enrichR.quiet = FALSE)
    packageStartupMessage("Welcome to enrichR\nChecking connections ... ", appendLF = TRUE)
    options(modEnrichR.use = TRUE)
    options(enrichR.sites = c("Enrichr", "FlyEnrichr", "WormEnrichr", "YeastEnrichr", "FishEnrichr", "OxEnrichr"))

    opts <- .proxyOpts()
    if (has_internet() || !is.null(opts)) {
        if (getOption("modEnrichR.use")) {
            listEnrichrSites()
        } else {
            getEnrichr(url=paste0(getOption("enrichR.base.address"), "datasetStatistics"))
            packageStartupMessage("Enrichr ... ", appendLF = FALSE)
            msg <- if(is.null(opts)) "Connection is Live!" else "Connection is Live! Using proxy."
            if (getOption("enrichR.live")) packageStartupMessage(msg)
        }
    } else {
        packageStartupMessage("No internet connection could be found. Set 'RCurlOptions' if proxy is needed")
        options(enrichR.live = FALSE)
    }
}


##' Internal function to check RCurlOptions
##'
##' Internal function to check RCurlOptions
##' @title Internal function to check RCurlOptions
##' @return Named vector
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
.proxyOpts <- function() {
    opts <- getOption("RCurlOptions")
    if(is.null(opts) || is.null(opts[["proxy"]])) {
        return(NULL)
    } else {
        url <- opts[["proxy"]]
        port <- opts[["proxyport"]]
        username <- opts[["proxyusername"]]
        password <- opts[["proxypassword"]]
        auth <- if(!is.null(opts[["proxyauth"]])) opts[["proxyauth"]] else "basic"
        return(c(url = url, port = port, username = username, password = password, auth = auth))
    }
}

##' Helper function
##'
##' Helper function for HTTP methods GET and POST
##' @title Helper function for HTTP methods GET and POST
##' @param method (Required). HTTP method. Default is \code{"GET"}
##' @param url (Required). URL address requested
##' @param ... (Optional). Additional parameters to pass to GET
##' @return same as GET
##' @author Wajid Jawaid \email{wajid.jawaid@gmail.com}
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @importFrom httr GET
##' @importFrom httr POST
##' @importFrom httr status_code
##' @importFrom httr http_status
##' @importFrom httr use_proxy
getEnrichr <- function(method = "GET", url, ...) {
    if(!method %in% c("GET","POST")) stop("Support GET and POST only.")
    options(enrichR.live = FALSE)
    tryCatch({
        opts <- .proxyOpts()
        if(is.null(opts)) {
            x <- if(method == "GET") GET(url = url, ...) else POST(url = url, ...)
	} else {
            proxy_config <- use_proxy(url = opts['url'], port = as.numeric(opts['port']),
                                      username = opts['username'], password = opts['password'],
                                      auth = opts['auth'])
            x <- if(method == "GET") GET(url = url, proxy_config, ...) else POST(url = url, proxy_config, ...)
	}
        code <- status_code(x)
        if(code != 200) {
            # Error with status code
            message(http_status(code)$message)
        } else {
            # OK/success
            options(enrichR.live = TRUE)
            invisible(x)
        }
    },
    # Warning message
    warning = function(warn) {
        message(warn); message("") # force newline
    },
    # Error without status code
    error = function(err) {
        message(err); message("") # force newline
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
##' @export
listEnrichrSites <- function() {
    opts <- .proxyOpts()
    msg <- if(is.null(opts)) "Connection is Live!" else "Connection is Live! Using proxy."
    for (site in getOption("enrichR.sites")) {
        getEnrichr(url = paste0(getOption("enrichR.sites.base.address"), site, "/", "datasetStatistics"))
        packageStartupMessage(paste0(site, " ... "), appendLF = FALSE)
        if (paste0(getOption("enrichR.sites.base.address"), site, "/")  == getOption("enrichR.base.address")) {
            if (getOption("enrichR.live")) packageStartupMessage(msg)
        } else 
            if (getOption("enrichR.live")) packageStartupMessage(msg)
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
    opts <- .proxyOpts()
    msg <- if(is.null(opts)) "Connection is Live!" else "Connection is Live! Using proxy."
    site <- gsub(getOption("enrichR.sites.base.address"), "", site)
    matched <- grep(paste0("^",site),
                    getOption("enrichR.sites"),
                    ignore.case = TRUE, value = FALSE)
    if( length(matched) == 0 ) {
        message("Given website does not match available sites: ", site)
        message("Choose from:\n",
                paste("-",getOption("enrichR.sites"), "\n"))
    } else if (length(matched) > 1) {
        message("Given website matches multiple options: ", site)
        message(paste("-", getOption("enrichR.sites")[matched], "\n"),)
    } else {
        site <- getOption("enrichR.sites")[matched]
        options(enrichR.base.address = paste0(getOption("enrichR.sites.base.address"), site,"/"))
        message("Connection changed to ",paste0(getOption("enrichR.sites.base.address"), site,"/"))
        getEnrichr(url = paste0(getOption("enrichR.base.address"), "datasetStatistics"))
        if (getOption("enrichR.live")) message(msg)
    }
}

##' Look up available databases on Enrichr
##'
##' Look up available databases on Enrichr
##' @title Look up available databases on Enrichr
##' @return A data.frame of available Enrichr databases
##' @author Wajid Jawaid \email{wajid.jawaid@gmail.com}
##' @importFrom httr GET POST
##' @importFrom rjson fromJSON
##' @export
##' @examples
##' dbs <- listEnrichrDbs()
listEnrichrDbs <- function() {
    dfSAF <- getOption("stringsAsFactors", FALSE)
    options(stringsAsFactors = FALSE)
    dbs <- getEnrichr(url = paste0(getOption("enrichR.base.address"), "datasetStatistics"))
    if (!getOption("enrichR.live")) return()
    dbs <- intToUtf8(dbs$content)
    dbs <- fromJSON(dbs)
    dbs <- lapply(dbs$statistics, function(x) do.call(cbind.data.frame, x))
    dbs <- do.call(rbind.data.frame, dbs)
    options(stringsAsFactors = dfSAF)
    dbs
}


##' Given an input, check format and return a character vector
##'
##' In standard analysis without background, crisp (symbols only) and
##' fuzzy (with scores) gene sets are acceptable
##' In analysis with background, only crisp gene sets are acceptable
##' @title FormatGenes
##' @param x Vector or dataframe of genes with or without score
##' @param type Depends on type of gene input
##' @return Character vector
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
.formatGenes <- function(x, type = c("standard","background")) {
    err_msg <- "Genes must be a character vector of Entrez gene symbols or a data.frame with gene symbols and/or score"
    type <- match.arg(type)

    if(is.data.frame(x)) {
        if(ncol(x) == 0) stop(err_msg)
        if(!is.character(x[,1])) stop(err_msg)
        if(ncol(x) == 1 | type == "background") { # 1-column input or background genes
            input <- x[,1]
	} else {
            if(is.numeric(x[,2]) & !all(x[,2] <= 1)) stop("The score (degree of membership) should be between 0 and 1")
            input <- paste(x[,1], x[,2], sep = ",")
	}
    } else if(is.character(x)) {
        if(length(x) == 0) stop(err_msg)
        input <- x
    } else {
        stop(err_msg)
    }

    input <- unique(input[nzchar(input)])
    if(length(input) == 0) stop(err_msg) else paste(input, collapse = "\n")
}


##' Download and parse GMT files from Enrichr
##'
##' Download and parse GMT files from Enrichr
##' @title Download and parse GMT files from Enrichr
##' @param db library
##' @return List object
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @importFrom utils download.file
.read_gmt <- function(db) {
    base.address <- getOption("enrichR.base.address")
    url <- paste0(base.address, "geneSetLibrary?mode=text&libraryName=", db)
    tf <- tempfile(pattern = db, fileext = ".gmt")
    cat("   - Download GMT file... ")
    tryCatch(download.file(url, tf, mode = "w", quiet = TRUE), 
	     warning = function(warn) { message(warn); message("") },
	     error = function(err) { message(err); message("") })
    lines <- readLines(tf)
    # Formatting from backr.R
    gmt <- list()
    for(line in lines) {
        line = gsub("\"", "", trimws(line))
        sp = unlist(strsplit(line, "\t"))
        sp[3:length(sp)] = gsub(",.*$", "", sp[3:length(sp)])
        gmt[[sp[1]]] = sort(unique(sp[3:length(sp)]))
    }
    unlink(tf)
    cat("Done.\n")
    return(gmt)
}


##' Upload gene list using Speedrichr API
##'
##' Upload gene list using Speedrichr API
##' @title Upload gene list using Speedrichr API
##' @param genes Input genes
##' @return R object that corresponds to the JSON object
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @importFrom rjson fromJSON
##' @importFrom httr config
.add_list <- function(genes) {
    base.address <- getOption("speedrichr.base.address")
    url <- paste0(base.address, "addList")
    payload <- list(list = .formatGenes(genes, "background"), description = NA)
    cat(" - Your gene set... ")
    response <- getEnrichr(url = url, body = payload, method = "POST", handle = NULL,
			   config = config(http_version = 1))
    # Handle odd errors that escape tryCatch
    if(is.null(response)) {
        stop("Error uploading gene list. Please try again later.")
    }
    if (!getOption("enrichR.quiet")) cat("Done.\n")
    data <- fromJSON(rawToChar(response$content))
    return(data)
}

##' Upload background list using Speedrichr API
##'
##' Upload background list using Speedrichr API
##' @title Upload background list using Speedrichr API
##' @param genes gene list
##' @return R object from JSON
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @importFrom rjson fromJSON
##' @importFrom httr config
.add_background <- function(genes) {
    base.address <- getOption("speedrichr.base.address")
    url <- paste0(base.address, "addbackground")
    payload <- list(background = .formatGenes(genes, "background"))
    cat(" - Your background... ")
    response <- getEnrichr(url = url, body = payload, method = "POST", handle = NULL,
			   config = config(http_version = 1))
    # Handle odd errors that escape tryCatch
    if(is.null(response)) {
        stop("Error uploading background list. Please try again later.")
    }
    if (!getOption("enrichR.quiet")) cat("Done.\n")
    data <- fromJSON(rawToChar(response$content))
    return(data)
}

##' Get enrichment result using Speedrichr API
##'
##' Get enrichment result using Speedrichr API
##' @title Get enrichment result using Speedrichr API
##' @param uId user List ID
##' @param bId background ID
##' @param db background Type
##' @return R object from JSON
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
.get_backgroundenrich <- function(uId, bId, db) {
    base.address <- getOption("speedrichr.base.address")
    url <- paste0(base.address, "backgroundenrich")
    payload <- list(userListId = uId,
                    backgroundid = bId,
                    backgroundType = db)
    cat(paste0(" - ", db, "... "))
    response <- getEnrichr(url = url, body = payload, method = "POST", handle = NULL)
    # Handle odd errors that escape tryCatch
    if(is.null(response)) {
        stop("Error running enrichment analysis. Please try again later.")
    }

    if (!getOption("enrichR.quiet")) cat("Done.\n")
    # Substitute Infinity with "Inf"
    json <- gsub("Infinity,", "\"Inf\",", rawToChar(response$content))
    json <- gsub("NaN,", "\"NaN\",", json)
    data <- fromJSON(json)
    return(data)
}

##' Gene enrichment using Enrichr
##'
##' Gene enrichment using Enrichr, also, you can now try adding a background.
##' @title Gene enrichment using Enrichr
##' @param genes (Required). Character vector of Entrez gene symbols as input. A data.frame
##' of gene symbols in first column is also acceptable, optionally a score denoting the
##' degree of membership between 0 and 1 in the second column.
##' @param databases (Required). Character vector of databases to search.
##' See https://maayanlab.cloud/Enrichr/ for available databases.
##' @param sleepTime (Optional) Time to wait (in seconds) between sending requests to the server to prevent the same results being returned as the previous request. Default is 1.
##' @param background (Optional). Character vector of Entrez gene symbols to be used as
##' background. A data.frame of gene symbols in first column is also acceptable. 
##' Default is \code{"NULL"}. Enrichment analysis with background genes is only available 
##' on the main site (Enrichr). Also, it is using a different API service (Speedrichr), 
##' hence it is a little slower to complete and return the results.
##' @param include_overlap (Optional). Download database in GMT format to include 'Overlap'
##' in the resulting data.frame when analysing with a background. Default is \code{"FALSE"}.
##' @return Returns a list of data.frame of enrichment terms, p-values, ...
##' @author Wajid Jawaid \email{wajid.jawaid@gmail.com}
##' @importFrom httr GET POST
##' @importFrom rjson fromJSON
##' @importFrom utils read.table
##' @export
##' @examples
##' # data(input) # Load example input genes
##' # data(background) # Load example background genes
##' # dbs <- c("GO_Molecular_Function_2023", "GO_Cellular_Component_2023",
##' #          "GO_Biological_Process_2023")
##' # if (getOption("enrichR.live")) {
##' #   enriched1 <- enrichr(input, dbs)
##' #   print(head(enriched1[[1]]))
##'
##' #   # Include background
##' #   enriched2 <- enrichr(input, dbs, background = background)
##' #   print(head(enriched2[[1]]))
##'
##' #   # Include background and add 'Overlap' info
##' #   enriched3 <- enrichr(input, dbs, background = background, include_overlap = TRUE)
##' #   print(head(enriched3[[1]]))
##' # }
enrichr <- function(genes, databases = NULL, background = NULL, include_overlap = FALSE,
                    sleepTime = 1) {
    if(length(genes) < 1) {
        stop("No genes have been given")
    }
    base.address <- getOption("enrichR.base.address")
    getEnrichr(url = base.address)
    if (!getOption("enrichR.live")) stop("Enrichr website is unreachable")
    if (is.null(databases)) {
        stop("No databases have been provided")
    }
    Sys.sleep(sleepTime)
    if(!is.null(background)) {
        if(basename(base.address) != "Enrichr") {
            warning("Enrichment analysis with background genes is only supported on the \\
                     main site\nSwitching to 'Enrichr'")
            setEnrichrSite("Enrichr")
        } else if(!is.vector(background) | all(background == "") | length(background) == 0)
            stop("'background' is invalid")
    }
    dfSAF <- getOption("stringsAsFactors", FALSE)
    options(stringsAsFactors = FALSE)
    if(is.null(background)) {
        if (!getOption("enrichR.quiet")) cat("Uploading data to Enrichr... ")
                                        # POST data to server, response not required
        getEnrichr(url = paste0(base.address, "enrich"),
                   body = list(list = .formatGenes(genes, "standard")), method = "POST")
        if (!getOption("enrichR.quiet")) cat("Done.\n")
        dbs <- as.list(databases)
        result <- lapply(dbs, function(x) {
            if (!getOption("enrichR.quiet")) cat("  Querying ", x, "... ", sep="")
            r <- getEnrichr(url = paste0(base.address, "export"),
                            query = list(file = "API", backgroundType = x))
            if (!getOption("enrichR.live")) stop("Enrichr website is unreachable")
            r <- gsub("&#39;", "'", intToUtf8(r$content))
            tc <- textConnection(r)
            r <- read.table(tc, sep = "\t", header = TRUE, quote = "", comment.char = "")
            close(tc)
            if (!getOption("enrichR.quiet")) cat("Done.\n")
            # Term, Overlap, P.value, Adjusted.P.value, Old.P.value, Old.Adjusted.P.value,
            # Odds.Ratio, Combined.Score, Genes
            return(r)
        })
    } else {
        if (!getOption("enrichR.quiet")) cat("Uploading data to Speedrichr...\n")
        uId <- .add_list(genes)$userListId
	bId <- .add_background(background)$backgroundid
	cat("Getting enrichment results...\n")
        result <- list()
        for(db in databases) {
            res <- .get_backgroundenrich(uId, bId, db)

	    r <- as.data.frame(do.call(rbind, (lapply(res[[db]], function(i) {
                i[[6]] <- paste(i[[6]], collapse = ";"); unlist(i) }))))
            # Rank, Term, P.value, Odds.Ratio, Combined.Score, Genes, Adjusted.P.value,
            # Old.P.value, Old.Adjusted.P.value
            colnames(r) <- c("Rank","Term","P.value","Odds.Ratio","Combined.Score","Genes",
                             "Adjusted.P.value","Old.P.value","Old.Adjusted.P.value")
	    r <- r[,c("Term","Rank","P.value","Adjusted.P.value","Old.P.value",
                      "Old.Adjusted.P.value","Odds.Ratio","Combined.Score","Genes")]

            if(isTRUE(include_overlap)) {
                gmt <- .read_gmt(db)
                # Count number of annotated genes in each term
                n_gmt <- sapply(gmt, length)

                # If all terms are found in GMT file, replace 'Rank' column with 'Overlap'
                if(length(intersect(r$Term, names(n_gmt))) == length(r$Term)) {
                    annotated <- as.numeric(n_gmt[r$Term])
                    significant <- nchar(r$Genes) - nchar(gsub(";", "", r$Genes))+1
                    r$Rank <- paste0(significant, "/", annotated)
                    colnames(r)[2] <- "Overlap"
		}
            }

	    r$P.value <- as.numeric(r$P.value)
	    r$Adjusted.P.value <- as.numeric(r$Adjusted.P.value)
	    r$Odds.Ratio <- as.numeric(r$Odds.Ratio)
	    r$Combined.Score <- as.numeric(r$Combined.Score)
	    r$Old.P.value <- as.integer(r$Old.P.value)
	    r$Old.Adjusted.P.value <- as.integer(r$Old.Adjusted.P.value)
	    result[[db]] <- r
	}
    }
    options(stringsAsFactors = dfSAF)
    if (!getOption("enrichR.quiet")) cat("Parsing results... ")
    names(result) <- databases
    if (!getOption("enrichR.quiet")) cat("Done.\n")
    return(result)
}


##' Given a Enrichr output, order and subset criteria, returns a data frame accordingly
##'
##' Given a Enrichr output, order and subset criteria, returns a data frame accordingly
##' @title Given a Enrichr output, order and subset criteria, returns a data frame accordingly
##' @param df Enrichr output
##' @param showTerms Number of terms to show. Default 20.
##' @param orderBy Column for ordering. Default "P.value"
##' @return Data frame
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
.enrichment_prep_df <- function(df, showTerms = 20, orderBy = "P.value") {

    if(is.null(showTerms)) {
        showTerms = nrow(df)
    } else if(!is.numeric(showTerms)) {
        stop(paste0("showTerms '", showTerms, "' is invalid."))
    }

    if("Overlap" %in% colnames(df)) {
        Annotated <- as.numeric(sub("^\\d+/", "", as.character(df$Overlap)))
        Significant <- as.numeric(sub("/\\d+$", "", as.character(df$Overlap)))

        # Build data frame
        df <- cbind(df, data.frame(Annotated = Annotated, Significant = Significant,
                                   stringsAsFactors = FALSE))
    } else {
        # Count significant genes
        df$Significant <- nchar(df$Genes) - nchar(gsub(";", "", df$Genes))+1
    }

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

##' Print Enrichr results
##'
##' Print Enrichr results from the selected gene-set libraries to individual text files 
##' or a Excel spreadsheet.
##' @title printEnrich
##' @param data (Required). Output list object from the \code{"enrichr"} function.
##' @param prefix (Optional). Prefix of output file. Default is \code{"enrichr"}.
##' @param showTerms (Optional). Number of terms to show.
##' Default is \code{NULL} to print all terms.
##' @param columns (Optional). Columns from each entry of data.
##' Default is \code{c(1:9)} to print all columns.
##' * Results without background: 1-"Term", 2-"Overlap", 3-"P.value", 
##' 4-"Adjusted.P.value", 5-"Old.P.value", 6-"Old.Adjusted.P.value", 
##' 7-"Odds.Ratio", 8-"Combined.Score", 9-"Combined.Score".
##' * In results with background, the second column is "Rank" if terms are not identical 
##' with those annotated in the Enrichr GMT files
##' @param outFile (Optional). Output file format, choose from "txt" and "excel". 
##' Default is "txt".
##' @return NULL
##' @author Wajid Jawaid \email{wajid.jawaid@gmail.com}
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @importFrom utils write.table
##' @importFrom WriteXLS WriteXLS
##' @export
##' @examples
##' # data(input) # Load example input genes
##' # if (getOption("enrichR.live")) {
##' #   enrichRLive <- TRUE
##' #   dbs <- listEnrichrDbs()
##' #   if(is.null(dbs)) enrichRLive <- FALSE
##' #   dbs <- c("GO_Molecular_Function_2023", "GO_Cellular_Component_2023",
##' #            "GO_Biological_Process_2023")
##' #   enriched <- enrichr(input, dbs)
##' #   print(head(enriched[[1]]))
##' #   # if (enrichRLive) printEnrich(enriched, outFile = "excel")
##' # }
printEnrich <- function(data, prefix = "enrichr", showTerms = NULL, columns = c(1:9),
                        outFile = c("txt","excel")) {
    if (!is.list(data)) stop("data is malformed must be a list")
    if (!is.integer(columns)) {
        stop(sprintf("'columns=' (%s) is invalid, must be a positive integer vector", 
                     paste(shQuote(columns), collapse = ",")))
    }

    outFile <- match.arg(outFile)
    if (outFile == "excel") {
        filename <- paste0(prefix, ".xlsx")
        output_excel_df <- vector("list", length(data))
        sheetnames <- names(data)
    } else {
        filenames <- paste0(prefix, "_", names(data), ".txt")
    }

    for (i in 1:length(data)) {
        dbname <- names(data)[i]
        df <- data[[i]]
        if (!is.data.frame(df)) stop("data is malformed - it must consist of data frames")

        df <- .enrichment_prep_df(df, showTerms, orderBy = "P.value")
        df <- df[, !colnames(df) %in% c("Annotated", "Significant")]

        if(any(columns > ncol(df))) {
            stop("Undefined columns selected")
        } else {
            df <- df[,columns]
        }

        if (outFile == "excel") {
            output_excel_df[[i]] <- df
            if (i == length(data)) {
                WriteXLS(output_excel_df, ExcelFileName = filename, SheetNames = sheetnames)
            }
        } else {
            write.table(df, file = filenames[i], sep = "\t", quote = F, row.names = F, col.names = T)
        }
    }
    message(sprintf("Output results in '%s' format", outFile))
}

##' Visualise a Enrichr output as barplot
##'
##' Visualise Enrichr result from a selected gene-set library as barplot.
##' @title plotEnrich
##' @param df (Required). A single data.frame from a list of Enrichr output.
##' @param showTerms (Optional). Number of terms to show. Default is \code{20}.
##' @param numChar (Optional). A single integer. Default is \code{40}.
##' Indicates the number characters to keep in the term description.
##' @param y (Optional). A character string. Default is \code{"Count"}.
##' Indicates the variable that should be mapped to the y-axis.
##' It can be either \code{"Count"} or \code{"Ratio"}. 
##' Results that includes background genes in the analysis can only show \code{"Count"}.
##' @param orderBy (Optional). A character string. Default is \code{"P.value"}.
##' Indicates how to order the Enrichr results before subsetting to keep top \code{N} terms.
##' It can be one of these:
##' \itemize{
##' \item \code{"P.value"}
##' \item \code{"Adjusted.P.value"} (or \code{"FDR"})
##' \item \code{"Combined.Score"} (or \code{"Score"})
##' }
##' @param xlab (Optional). A character string. Default is \code{NULL}.
##' Indicates the x-axis label.
##' @param ylab (Optional). A character string. Default is \code{NULL}.
##' Indicates the y-axis label.
##' @param title (Optional). A character string. Default is \code{NULL}
##' Indicates the main title for the graphic.
##' @return A \code{\link[ggplot2]{ggplot}} plot object
##' @author I-Hsuan Lin \email{i-hsuan.lin@manchester.ac.uk}
##' @seealso
##' \code{\link[ggplot2]{ggplot}}
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_string
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 coord_flip
##' @importFrom ggplot2 theme_bw
##' @importFrom ggplot2 scale_fill_continuous
##' @importFrom ggplot2 scale_x_discrete
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
##' # data(input) # Load example input genes
##' # dbs <- c("GO_Molecular_Function_2023", "GO_Cellular_Component_2023",
##' #          "GO_Biological_Process_2023")
##' # if (getOption("enrichR.live")) {
##' #   enriched <- enrichr(input, dbs)
##' #   print(head(enriched[[1]]))
##' #   # Plot top 20 terms from "GO_Biological_Process_2023" and ordered by P-value
##' #   plotEnrich(enriched[[3]], showTerms = 20, numChar = 50, y = "Count",
##' #              orderBy = "P.value")
##' # }
plotEnrich <- function(df, showTerms = 20, numChar = 40, y = "Count", orderBy = "P.value",
                       xlab = NULL, ylab = NULL, title = NULL) {
    if(!is.data.frame(df)) {
        stop("Input df is malformed - must be a data.frame object.")
    }
    if(nrow(df) == 0 | ncol(df) == 0) {
        stop("Input df is empty.")
    }
    if(!is.numeric(numChar)) {
        stop(paste0("numChar '", numChar, "' is invalid."))
    }

    if(orderBy == "Adjusted.P.value" | orderBy == "FDR") {
        orderBy = "Adjusted.P.value"
    } else if(orderBy == "Combined.Score" | orderBy == "Score") {
        orderBy = "Combined.Score"
    } else {
        orderBy = "P.value"
    }

    df <- .enrichment_prep_df(df, showTerms, orderBy)

    # Create trimmed name (as seen in topGO)
    shortName <- paste(substr(df$Term, 1, numChar),
                       ifelse(nchar(df$Term) > numChar, '...', ''), sep = '')
    names(shortName) <- df$Term

    # Print warning if there are any duplicated trimmed names
    if(any(duplicated(shortName))) {
        warning("There are duplicated trimmed names in the plot, consider increasing the 'numChar' setting.")
    }

    # Define fill variable (P.value or Combined.Score)
    if(orderBy == "Combined.Score") {
        fill <- "Combined.Score"
    } else {
        fill <- "P.value"
    }

    # Define y variable ("Count" or "Ratio")
    if(y == "Ratio") {
        if("Overlap" %in% colnames(df)) {
            df$Ratio <- df$Significant/df$Annotated
        } else {
            warning('"Ratio" is not available when analysing with background genes. Plotting with y = "Count" instead.')
            y <- "Significant"
        }
    } else {
        y <- "Significant"
    }

    # Define variable mapping
    map <- aes_string(x = "Term", y = y, fill = orderBy)

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
    p <- ggplot(df, map) + geom_bar(stat = "identity") + coord_flip() + theme_bw() +
        scale_x_discrete(labels = rev(shortName), limits = rev(df$Term))

    if(orderBy == "Combined.Score") {
        p <- p + scale_fill_continuous(low = "blue", high = "red") +
                guides(fill = guide_colorbar(title = "Combined Score", reverse = FALSE))
    } else {
        p <- p + scale_fill_continuous(low = "red", high = "blue")
        if(orderBy == "Adjusted.P.value") {
            p <- p + guides(fill = guide_colorbar(title = expression(atop("Adjusted", paste(italic("P")," value")),
						  reverse = FALSE), title.vjust = 0.1))
	} else {
            p <- p + guides(fill = guide_colorbar(title = expression(paste(italic("P")," value")),
						  reverse = FALSE))
	}
    }

    # Adjust theme components
    p <- p + theme(axis.text.x = element_text(colour = "black", vjust = 1),
                   axis.text.y = element_text(colour = "black", hjust = 1),
                   axis.title = element_text(color = "black", margin = margin(10, 5, 0, 0)),
                   axis.title.y = element_text(angle = 90))

    p <- p + xlab(xlab) + ylab(ylab) + ggtitle(title)

    return(p)
}
