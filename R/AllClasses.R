##' @export
setClass("SwitchrParam", representation(logfun = "function", shell_init = "character"))


##' RComputingEnv
##'
##' An object that represents a "computing environment" (a specific set of libpaths and the packages installed to them)
##'@export
setClass("RComputingEnv", representation(name = "character",
                                         libpaths = "character",
                                         exclude.site = "logical",
                                         packages = "data.frame",
                                         seed = "ANY"))

##'@export
RComputingEnv = function(name, libpaths, exclude.site = TRUE, seed = NULL) {
    if(exclude.site)
        pathsToLook = unique(c(libpaths, .Library))
    else
        pathsToLook = unique(c(libpaths, .Library.site, .Library))

    pkgs = installed.packages(pathsToLook, noCache=TRUE)[,c("Package", "Version", "LibPath")]
    pkgs = pkgs[!duplicated(pkgs[,"Package"]),]
    pkgs = as.data.frame(pkgs, stringsAsFactors = FALSE)
    
    new("RComputingEnv", name= name, libpaths = libpaths, exclude.site = exclude.site, packages = pkgs, seed = seed)

}

switchrOpts = new.env()

##' RepoSubset
##'
##' An object that represents a subset of packages available in a repo. When switched to, switchr will default to only installing the
##' specified packages, rather than all packages in the repository.
setClass("RepoSubset", representation(repos = "character",
                                      pkgs = "character",
                                      default_name = "character"))


##' @export
RepoSubset = function(repos, pkgs, default_name) {
    new("RepoSubset", repos = repos, pkgs = pkgs, default_name = default_name)
}


##' @export
setClass("PkgSource", representation(name = "character",location="character",
                                     branch = "character",
                                     subdir = "character", user = "character",
                                     password="character"))
setClass("SVNSource", contains = "PkgSource")
setClass("GitSource", contains = "PkgSource")
setClass("GithubSource", contains = "GitSource")
setClass("CVSSource", contains = "PkgSource")
setClass("LocalSource", contains = "PkgSource")
setClass("CRANSource", contains = "PkgSource")
setClass("TarballSource", contains = "PkgSource")



setAs("GitSource", "SVNSource",
      function(from) {
              if(!grepl("github", location(from)))
                  stop("Cannot convert non-github GitSource object to SVNSource")
              else {
                  url = gsub( "\\.git", "", location(from))
                  url = gsub("git://", "http://", url)
                  br = if(branch(from) == "master") "trunk" else branch(from)
                  makeSource(name = from@name, url = url, branch = br,
                             subdir = subdir(from), user = "",
                             password = "", type = "svn")
              }
          })






##'@export
##'
setClass("PkgManifest", representation( manifest = "data.frame",
                                          dependency_repos = "character"))

##'@export
##' @import RCurl
PkgManifest = function(manifest, dep_repos = c(biocinstallRepos(), defaultGRAN()), ...) {
    if(is.character(manifest)) {
        if(is.url(manifest)) {
            fil = tempfile()
            download.file(manifest, method = "curl", fil)
            manifest  = fil
        }

        if(file.exists(manifest))
            manifest = read.table(manifest, header= TRUE, sep= ",", stringsAsFactors = FALSE, ...)
        else
            stop("invalid manifest")
    }

    new("PkgManifest", manifest = manifest, dependency_repos = dep_repos)
}

setClass("GithubPkgManifest", contains = "PkgManifest")



#manifest is a data.frame with the following columns:
##name, url, type, subdir, branch, extra
manifestBaseCols = c("name", "url", "type", "subdir", "branch", "extra")






##'@export
setClass("SessionManifest", representation(pkg_versions = "data.frame",
                                           pkg_manifest = "PkgManifest"))

##' @export
SessionManifest = function(manifest, versions) {
    new("SessionManifest", pkg_versions = versions, pkg_manifest = manifest)
}


##'@export
setClass("parsedSessionInfo", representation(version = "character",
                                             platform="character",
                                             attached = "data.frame",
                                             loaded = "data.frame"))
