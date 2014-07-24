
##' RComputingEnv
##'
##' An object that represents a "computing environment" (a specific set of libpaths and the packages installed to them)
##'@export
setClass("RComputingEnv", representation(name = "character",
                                         libpaths = "character",
                                         exclude.site = "logical",
                                         packages = "data.frame",
                                         attached = "character",
                                         src_url = "character"))

##'@export
RComputingEnv = function(name, libpaths, exclude.site = TRUE, src_url) {
    if(exclude.site)
        pathsToLook = unique(c(libpaths, .Library))
    else
        pathsToLook = unique(c(libpaths, .Library.site, .Library))

    pkgs = installed.packages(pathsToLook, noCache=TRUE)[,c("Package", "Version", "LibPath")]
    pkgs = pkgs[!duplicated(pkgs[,"Package"]),]
    pkgs = as.data.frame(pkgs, stringsAsFactors = FALSE)
    
    new("RComputingEnv", name= name, libpaths = libpaths, exclude.site = exclude.site, packages = pkgs, src_url = src_url)

}

switchrOpts = new.env()

##' RepoSubset
##'
##' An object that represents a subset of packages available in a repo. When switched to, switchr will default to only installing the
##' specified packages, rather than all packages in the repository.
setClass("RepoSubset", representation(repos = "character",
                                      pkgs = "character",
                                      default_name = "character"))


RepoSubset = function(repos, pkgs, default_name) {
    new("RepoSubset", repos = repos, pkgs = pkgs, default_name = default_name)
}
