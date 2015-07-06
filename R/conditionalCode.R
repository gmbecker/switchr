getBiocRepos = function() {
    if(requireNamespace2("BiocInstaller", quietly=TRUE)) {
        bioc = BiocInstaller::biocinstallRepos()
    } else {
        if(is.null(defaultBiocRepos)) {
            bioc = tryCatch(getBiocReposFromRVers(), function(e) character())
            if(length(bioc) == 0)
                warning("Unable to determine Bioc repositories. They will not be included in the set of default dependency repos")
        } else
            bioc = defaultBiocRepos
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
    if(optrepos["CRAN"] == "@CRAN@") {
        
        if(any(grepl("cran", bioc, ignore.case=TRUE))) 
            optrepos = optrepos[!names(optrepos) == "CRAN"]
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
