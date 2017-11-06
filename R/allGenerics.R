##' makePkgDir
##' 
##' This is an internal function not intended to be called directly by end users
##'
##' Create a directory and populate it with package source code from the
##' specified source
##' @param name The package
##' @param source A PkgSource
##' @param path The path to place the directory
##' @param latest_only Should a fastpath for downloading the latest commit
##' in a SCM package without a formal checkout be used?
##' @param param A SwitchrParam
##' @param forceRefresh Should an existing instance of the package source be
##' deleted/refreshed
##' @docType methods
##' @rdname makePkgDir
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
##' @param pkg_manifest The manifest to use
##' @param versions Specific versions of the packages to install. Should be a
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
##' @param param A SwitchrParam object
##' @param force_refresh If a package already appears in the lazy repo area,
##' it be updated (e.g. from SCM) and built again? Defaults to FALSE
##' @return A path to the populated lazy repository, suitable for 'coercing' to
##' a url and installing from.
##' @details When checking building from SVN or git checkouts, this function
##' will first look for existing checkouts for the relevant packages in
##' \code{dir}. If found, these will be updated (in the case of conflicts, the
##' behavior is undefined and will likely fail if they are not resolvable). This
##' allows the user to have an existing, checkout directory where he or she
##' works on development versions of multiple, interrelated packages, as local
##' changes WILL be reflected in the packages built into the lazy repository.
##' @export
##' @author Gabriel Becker
##' @rdname lazyRepo
##' @references Becker G, Barr C, Gentleman R, Lawrence M; Enhancing Reproducibility and Collaboration via Management of R Package Cohorts. Journal of Statistical Software, 81(1). 2017. doi: 10.18637/jss.v082.i01 
##' @docType methods
setGeneric("lazyRepo",
           function(pkgs,
                    pkg_manifest,
                    versions = rep(NA, times = length(pkgs)),
                    dir = tempdir(),
                    rep_path = file.path(dir, "repo"),
                    get_suggests = FALSE,
                    verbose = FALSE,
                    scm_auths = list(bioconductor = c("readonly", "readonly")),
                    param = SwitchrParam(),
                    force_refresh = FALSE) standardGeneric("lazyRepo"))



##' gotoVersCommit
##' 
##' This is a low-level function not intended for direct use by the end user.
##' @docType methods
##' @param dir Directory
##' @param src A PkgSource (or subclass) object
##' @param version The exact version to locate
##' @param param A SwitchrParam
##' @rdname gotoVersCommit
##' @export
setGeneric("gotoVersCommit", function(dir, src, version, param = SwitchrParam()) standardGeneric("gotoVersCommit"))
