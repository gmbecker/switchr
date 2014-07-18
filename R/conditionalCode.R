    if(require(GRAN, quietly=TRUE)) {
        message("Found GRAN package installed. Adding it to list of default repositories for dependencies.")
        defGRAN = defaultGRAN()
        
    } else
        defGRAN = NULL

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
} else if(require(BiocInstaller, quietly=TRUE)) {
    bioc = biocinstallRepos()
} else {
    bioc = NULL
}

