sneakyreqpkg = function(pkg, quietly = FALSE) {
    req = tryCatch(get("requireNamespace"), error = identity)
    if(is(req, "error"))
        req = get("require")
    
    req(pkg, quietly = quietly)
}

    

getBiocRepos = function() {
    ## this sucks but I can't afford the dependency on 3.5.x+ that comes
    ## with BiocManager :(
    if(sneakyreqpkg("BiocManager", quietly = TRUE)) { 
        bioc = get("repositories", asNamespace("BiocManager"))()
    } else if(sneakyreqpkg("BiocInstaller", quietly = TRUE)) {
        bcrepofun = get("biocinstallRepos", envir = asNamespace("BiocInstaller")) 
        bioc = bcrepofun()
    } else if(beforeBiocInstaller()) {
        if(!exists("biocinstallRepos"))
            source("http://bioconductor.org/biocLite.R")
        bioc = biocinstallRepos()
    } else {
        if(is.null(defaultBiocRepos)) {
            bioc = tryCatch(getBiocReposFromRVers(), function(e) character())
            if(length(bioc) == 0)
                warning("Unable to determine Bioc repositories. They will not be included in the set of default dependency repos")
        } else
            bioc = defaultBiocRepos
    }
    if(anyNA(bioc))  {
        warning("Attempt to determine default Bioconductor repos returned one or more NAs. These will be omitted from the set of default repositories.")
        bioc = bioc[!is.na(bioc)]
    }
    bioc
}




##' defaultRepos
##'
##' Get default repositories for use as dependency repos and within
##' install_packages
##'
##' @return A character vector of package repository urls
##' @export
##' @importFrom utils chooseCRANmirror
defaultRepos = function() {
    bioc = getBiocRepos()
    optrepos = getOption("repos")
    if(is.na(optrepos["CRAN"]) || optrepos["CRAN"] == "@CRAN@") {
        ## if bioc has a cranmirror (which it should)
        if(any(grepl("cran", bioc, ignore.case=TRUE))) 
            optrepos = optrepos[!grepl("CRAN", names(optrepos))]
        else {

            if(interactive())
                chooseCRANmirror()
            else{
                message("Switchr needs a default CRAN mirror set via R options. Using the Rstudio mirror. This happens only when no CRAN mirror is selected *and* the BiocInstaller package is not installed.")
                chooseCRANmirror(ind= 1)
            }
            optrepos = getOption("repos")
        }
    } else if (!is.null(names(bioc))) 
          bioc = bioc[!names(bioc) == "CRAN"]
    
    granrepos = NULL
    if(exists("defaultGRANURL")) 
        granrepos = get("defaultGRANURL")()
    repos = unique(c(granrepos, optrepos, bioc))
    repos
}
