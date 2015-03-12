getBiocRepos = function() {
if(compareVersion(paste(R.version$major, R.version$minor, sep="."), "2.14.0") < 0) {
    message("R before 2.14 detected. Attempting to determine BioC repos by sourcing biocLite.R")
    env = new.env()
    res = tryCatch(source("http://bioconductor.org/biocLite.R"), error = function(x) x)
    if(!is(res, "error"))
        bioc = biocinstallRepos()
    else {
        message("Sourcing remote file failed. This installation will not have a set of default BioC repositories")
        bioc = NULL
    }
} else if(require("BiocInstaller", quietly=TRUE)) {
    bioc = biocinstallRepos()
} else {

    bioc = NULL
    reps = NULL
    p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
    if (file.exists(p)) {
        reps <- tools:::.read_repositories(p)
        if (!"BioCsoft" %in% rownames(reps))
            reps <- NULL
    }
    if (is.null(reps)) {
        p <- file.path(R.home("etc"), "repositories")
        if(file.exists(p))
            reps <- tools:::.read_repositories(p)
    }
    if(!is.null(reps) && any(grepl("^bioc", rownames(reps), ignore.case = TRUE))) {
        bioc = reps[grep("^bioc", reps[,"menu_name"], ignore.case=TRUE), "URL"]
    }
}
bioc
}

globalVariables("defaultGRANURL")



##' defaultRepos
##'
##' Get default repositories for use as dependency repos and within
##' install_packages
##'
##' @return A character vector of package repository urls
##' @export
defaultRepos = function() {
    bioc = getBiocRepos()
    optrepos = getOption("repos")
    if(optrepos["CRAN"] == "@CRAN@")
        optrepos = optrepos[!names(optrepos) == "CRAN"]
    else if(!is.null(bioc))
        bioc = bioc[!names(bioc) == "CRAN"]
        

    granrepos = NULL
    if(exists("defaultGRANURL"))
        granrepos = defaultGRANURL()
    repos = unique(c(granrepos, optrepos, bioc))
    repos
}
