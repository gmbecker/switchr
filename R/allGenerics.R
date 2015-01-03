##' @export
setGeneric("makePkgDir",
           function(name,
                    source,
                    path,
                    latest_only,
                    param = SwitchrParam(),
                    forceRefresh = FALSE) standardGeneric("makePkgDir"))

##' lazyRepo
##'
##' Create a lazy repository for installing directly from a package
##' manifest. Most users will want to call \code{Install} directly,
##' which will call this as needed behind the scenes.
##'
##' @param pkgs The packages to install
##' @param manifest The manifest to use
##' @param version Specific versions of the packages to install. Should be a
##' vector of the same length as \code{pkgs} (and in the same order). Defaults
##' to NA (any version) for all packages.
##' @param dir The directory packages should be downloaded/checkedout/built into
##' @param rep_path The path of the final repository
##' @param get_suggests Whether suggested packages should be included
##' in the lazy repository. Defaults to FALSE
##' @param verbose Should extra information be printed to the user during
##' the construction process
##' @param scm_auths Named list of username/password credentials for checking
##' out package sources from one or more sources listed in \code{manifest}
##' Defaults to readonly access to Bioconductor SVN
##'
##' @return A path to the populated lazy repository, suitable for 'coercing' to
##' a url and installing from.
##' @export
##' @author Gabriel Becker
setGeneric("lazyRepo",
           function(pkgs,
                    pkg_manifest,
                    versions = rep(NA, times = length(pkgs)),
                    dir = tempdir(),
                    rep_path = file.path(dir, "repo"),
                    get_suggests = FALSE,
                    verbose = FALSE,
                    scm_auths = list(bioconductor = c("readonly", "readonly")),
                    param = SwitchrParam()) standardGeneric("lazyRepo"))


##' @export
setGeneric("gotoVersCommit", function(dir, src, version, param = SwitchrParam()) standardGeneric("gotoVersCommit"))
