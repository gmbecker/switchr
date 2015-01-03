##' install_packages
##'
##' Install packages from a set of traditional repositories, or a Just-in-time
##' repository constructed using a PkgManifest or SessionManifest
##' @param pkgs The names of the packages to install
##' @param repos The (generalized) repositor(ies) to install the packages from.
##' Can be a character vector of traditional package repositories (as with install.packages)
##' or a PkgManifest or SessionManifest (or a url thereof)
##' @param versions An optional data.frame specifying exact versions of the packages to install
##' @param verbose Should extra information be printed during the console during installation
##' @param \dots{} extra parameters passed directly to install.packages
##'
##' @details In addition to installing the specified packages, this function
##' annotates the installed DESCRIPTION files with provenance information
##' about where the packages were installed from. This retains the information
##' necessary to generate a manifest of installed packages for publication or
##' reinstallation.
##'
##' When \code{repos} is a vector of traditional repositories, this function -
##' with the exception of the provenance mentioned above - behaves identically
##' to \code{\link{install.packages}}. Otherwise, a Just-in-Time package
##' repository is constructed using the information in the manifest(s) passed
##' to \code{repos}, which is then used in conjuction with
##' \code{link{install.packages}} to do the actual installation.
##'
##' @author Gabriel Becker
##' @export
setGeneric("install_packages", function(pkgs, repos, versions = NULL, verbose = FALSE, ...) standardGeneric("install_packages"))

setMethod("install_packages", c("character", "character"), function(pkgs, repos, versions, verbose, ...) {

    man = PkgManifest(manifest = ManifestRow(), dep_repos = repos)
    if(!is.null(versions))
        man = SessionManifest(pkg_manifest = man, pkg_versions = version)
    install_packages(pkgs, repos = man, verbose = verbose, ...)
    
})

setMethod("install_packages", c(pkgs = "character", repos= "missing"), function(pkgs, repos, verbose, ...) {
    install_packages(pkgs, repos = defaultRepos(), verbose = verbose,
            ...)
})


setMethod("install_packages", c(pkgs = "SessionManifest", repos= "ANY"), function(pkgs, repos, verbose, ...) {
    ghrepo = lazyRepo(pkgs)
    .install_packages(pkgs = versions_df(pkgs)$name, lazyrepo = ghrepo, man = manifest(pkgs), ...)
})


setMethod("install_packages", c(pkgs = "character", repos= "SessionManifest"), function(pkgs, repos, verbose, ...) {
    install_packages(pkgs, repos = defaultRepos(), verbose = verbose,
            ...)
    vdf = versions_df(repos)
    rownames(vdf) = vdf$name
    vers = vdf[pkgs, "version"]
    ghrepo = lazyRepo(pkgs = pkgs, versions = vers, manifest = manifest(repos))
    .install_packages(pkgs = pkgs, lazyrepo = ghrepo, man = manifest(repos), ...)
})





setMethod("install_packages", c(pkgs = "character", repos= "PkgManifest"), function(pkgs, repos, verbose, ...) {

    ghrepo= lazyRepo(pkgs, repos, verbose = verbose)
    .install_packages(pkgs, ghrepo, man = repos, ...)
})

## @param man A PkgManifest
.install_packages = function(pkgs, lazyrepo, man, ...) {
    
    avail1 = available.packages(lazyrepo)
    avail2 = available.packages(contrib.url(dep_repos(man)))
    new = !avail2[,"Package"] %in% avail1[,"Package"]
    avail = rbind(avail1, avail2[new,])
    oldpkgs = installed.packages()[,"Package"]
    oldinfo = lapply(oldpkgs, function(x) file.info(system.file("DESCRIPTION", package = x)))
    
    install.packages(pkgs, available = avail, ...)

    newpkgs  = installed.packages()[,"Package"]

    newinds = !newpkgs %in% oldpkgs

    possupdates = newpkgs[!newinds]

    newinfo = lapply(possupdates, function(x) file.info(system.file("DESCRIPTION", package = x)))

    updated = mapply(function(old, new) !identical(old, new), old = oldinfo, new = newinfo)
    installedpkgs = c(newpkgs[newinds], newpkgs[updated])
    annotateDESCs(installedpkgs, man)
    installedpkgs
}

    
    
     
