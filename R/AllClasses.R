##' @import methods
##' @importFrom utils chooseCRANmirror
##' 
NULL

setOldClass("sessionInfo")

##' @export
##' @rdname SwitchrParam
setClass("SwitchrParam", representation(logfun = "function", shell_init = "character",
                                        archive_timing="numeric", archive_retries="numeric",
                                        dl_method = "character",
                                        shell_timing = "numeric"),
         prototype = prototype(logfun = message, shell_init = "", archive_timing = 2, archive_retries = 2,
                          dl_method = "auto"))



setClass("SwitchrCtx", representation(name = "character",
                                         libpaths = "character",
                                         exclude.site = "logical",
                                         packages = "data.frame",
                                         seed = "ANY"))
##' SwitchrCtx
##'
##' A constructor for class SwitchrCtx, represenging a switchr installed-package library.
##' @param name The name to associate with the context
##' @param libpaths The directories where the installed packages are located
##' @param exclude.site Should the current site library be included in the
##' context when it is switched to (TRUE) '
##' @param seed An object representing the list of packages the switchr context
##' was seeded with.
##' @aliases SwitchrCtx-class
##' @references Becker G, Barr C, Gentleman R, Lawrence M; Enhancing Reproducibility and Collaboration via Management of R Package Cohorts. Journal of Statistical Software, 81(1). 2017. doi: 10.18637/jss.v082.i01 
##'@export
SwitchrCtx = function(name, libpaths, exclude.site = TRUE, seed = NULL) {
    
    ctx = new("SwitchrCtx", name= name, libpaths = libpaths,
        exclude.site = exclude.site,
        packages = data.frame(Package = character(),
            Version = character(),
            LibPath = character(),
            stringsAsFactors = FALSE),
        seed = seed)
    update_pkgs_list(ctx)
}

switchrOpts = new.env()
setClass("RepoSubset", representation(repos = "character",
                                      pkgs = "character",
                                      default_name = "character"))


##' RepoSubset
##'
##' An object that represents a subset of packages available in a repo. When switched to, switchr will default to only installing the
##' specified packages, rather than all packages in the repository.
##' @param repos The traditional repositories to select the packages from
##' @param pkgs The packages included in the subset
##' @param default_name The default name to use when the RepoSubset is used to
##' seed a switchr context
##' @aliases RepoSubset-class
##' @export
RepoSubset = function(repos, pkgs, default_name) {
    new("RepoSubset", repos = repos, pkgs = pkgs, default_name = default_name)
}

##' PkgSource
##' 
##' An object representing the source location of a package. This is a virtual
##' used exlusively through its subclasses, which are used to differentiate the
##' different types of package source locations.
##' 
##' @export
setClass("PkgSource", representation(name = "character",location="character",
                                     branch = "character",
                                     subdir = "character", user = "character",
                                     password="character"),
         prototype = list(branch = NA_character_, subdir = "."))
##' @export
##' @rdname PkgSource-class
setClass("SVNSource", contains = "PkgSource")
##' @export
##' @rdname PkgSource-class
setClass("GitSource", contains = "PkgSource")
##' @rdname PkgSource-class
##' @export
setClass("GithubSource", contains = "GitSource")
##' @rdname PkgSource-class
##' @export
setClass("CVSSource", contains = "PkgSource")
##' @rdname PkgSource-class
##' @export
setClass("LocalSource", contains = "PkgSource")
##' @rdname PkgSource-class
##' @export
setClass("CRANSource", contains = "PkgSource")
##' @rdname PkgSource-class
##' @export
setClass("BiocSource", contains = "PkgSource")
##' @rdname PkgSource-class
##' @export
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



ensureCRANmirror = function(ind=1L) {
    repopt = getOption("repos")
    if(!interactive() &&
       (any(repopt == "@CRAN@") || is.na(repopt[["CRAN"]])))
        chooseCRANmirror(ind=ind)
}



##'@export
##'
setClass("PkgManifest", representation( manifest = "data.frame",
                                          dependency_repos = "character"))

##' PkgManifest
##'
##' Construct a PkgManifest, which can be installed from using \code{\link{install_packages}}
##' @param manifest  The manifest (data.frame) of packages and their locations
##' @param dep_repos A list of traditional pkg repositories which can contain dependencies
##' for the packages listed in \code{manifest}.
##' @param \dots Arguments passed to \code{\link{ManifestRow}} if \code{manifest} is not specified
##' @param dl_method Download method. Ignored unless \code{manifest} is a
##' character scalar containing a URL to a serialized manifest
##' @details If a package is found in both the manifest dataf.frame and the dependency
##' repositories, the version in the manifest will always take precidence within the
##' switchr framework.
##' @export
##' @aliases PkgManifest-class
##' @importFrom utils read.table
PkgManifest = function(manifest = ManifestRow(...), dep_repos = defaultRepos(), ..., dl_method){
    ensureCRANmirror(1L)
    if(is.character(manifest)) {
        if(url.exists(manifest)) {
            fil = tempfile()
            download.file2(manifest, fil)
            manifest  = fil
        }

        if(file.exists(manifest)) {
            manifest = read.table(manifest, header= TRUE, sep= ",", stringsAsFactors = FALSE, ...)
            manifest = manifest[,names(ManifestRow())]
        } else
            stop("invalid manifest")
    }

    if(anyNA(dep_repos)) {
        stop("dep_repos includes NAs:", paste(dep_repos, collapse = ", "))
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

##' SessionManifest
##'
##' A manifest which includes both a PkgManifest containing package source
##' information, and a data.frame defining a filter with exact versions
##' of some or all packages
##' @param manifest A PkgManifest
##' @param versions A data.frame with 2 columns: name and version, or a named
##' character vector. In the case of a character vector, the names are taken to
##' be package names
##' @return A SessionManifest object
##' @aliases SessionManifest-class
##' @export
SessionManifest = function(manifest, versions = character()) {
    if(is(versions, "character"))
        versions = data.frame(name = as.character(names(versions)), version = versions,
            stringsAsFactors=FALSE)
    unknown = setdiff(versions$name, manifest_df(manifest)$name)
    if(length(unknown) > 0)
        stop("Setting version constraints on packages not listed in the manifest is not currently supported")
    
    new("SessionManifest", pkg_versions = versions, pkg_manifest = manifest)
}


##' Parsed sessionInfo output
##'
##' An object representing the information in printed sessionInfo() output
##' @export
setClass("parsedSessionInfo", representation(version = "character",
                                             platform="character",
                                             attached = "data.frame",
                                             loaded = "data.frame"))



##' LibraryProfile (experimental)
##' 
##' Currently unused/under heavy development.
##'
##' An object
setClass("LibraryProfile", representation(autoloads = "character",
                                          script = "character"))

LibraryProfile = function(...) new("LibraryProfile", ...)
