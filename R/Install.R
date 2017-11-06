##' install_packages
##'
##' Install packages from a set of traditional repositories, or a Just-in-time
##' repository constructed using a PkgManifest or SessionManifest
##' @param pkgs The names of the packages to install
##' @param repos The (generalized) repositor(ies) to install the packages from.
##' Can be a character vector of traditional package repositories (as with install.packages)
##' or a PkgManifest or SessionManifest (or a url thereof)
##' @param versions An optional named character vector or data.frame specifying exact versions of the packages to install
##' @param verbose Should extra information be printed during the console during installation
##' @param \dots extra parameters passed directly to install.packages
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
##' @examples
##' \dontrun{
##' ## equivalent to install.packages, except it stores
##' ## package provenance and knows about bioconductor repos
##' install_packages("nlme")
##'
##' ## install from a manifest
##' man = GithubManifest("gmbecker/fastdigest")
##' install_packages("fastdigest", man)
##'
##' ## install a full seeding manifest
##' man2 = makeSeedMan("myotherlib")
##' install_packages(man2)
##' }
##' @author Gabriel Becker
##' @docType methods
##' @rdname install
##' @references Becker G, Barr C, Gentleman R, Lawrence M; Enhancing Reproducibility and Collaboration via Management of R Package Cohorts. Journal of Statistical Software, 81(1). 2017. doi: 10.18637/jss.v082.i01 
##' @export
##' @importFrom utils available.packages contrib.url installed.packages
setGeneric("install_packages", function(pkgs, repos, versions = NULL, verbose = FALSE, ...) standardGeneric("install_packages"))
##'@rdname install
##' @aliases install_packages,character,character
setMethod("install_packages", c("character", "character"), function(pkgs, repos, versions, verbose, ...) {
              chtypes = getStringType(repos)
              if(any(chtypes %in% c("sessioninfo", "manifestdir")))
                  stop("Unsupported character format passed to repos argument")
              repos = mapply(repoFromString, str = repos, type = chtypes)
              
              man = PkgManifest(manifest = ManifestRow(name = character()), dep_repos = repos)
              ## FIXME: I should only have to handle versions type and missing versions in
              ## one method, currently its handled in two. Bad design
              if(!is.null(versions)) {
                  if(is(versions, "character"))
                      versions = data.frame(name = names(versions),
                          version = versions, stringsAsFactors=FALSE)
                  if(any(!versions$names %in% pkgs))
                      stop("Versions specified for packages not being installed. This is not currently supported.")
                  if(any(!pkgs %in% versions$name)) {
                      
                      manifest_df(man) = ManifestRow(name = versions$name)
                      man = .findThem(man, PkgManifest(dep_repos = repos))
                      man = SessionManifest(manifest = man, versions = versions)
                  }
              } else {
                  versions = rep(NA_character_, times = length(pkgs))
                  names(versions) = pkgs
              }
              install_packages(pkgs, repos = man, verbose = verbose, versions = versions, ...)
    
          })


##' @rdname install
##' @aliases install_packages,character,missing

setMethod("install_packages", c(pkgs = "character", repos= "missing"), function(pkgs, repos, versions = NULL, verbose, ...) {
    install_packages(pkgs, repos = defaultRepos(), verbose = verbose,
                     versions = versions, ...)
})
 
##'@rdname install
##' @aliases install_packages,SessionManifest,ANY

setMethod("install_packages", c(pkgs = "SessionManifest", repos= "ANY"), function(pkgs, repos, verbose, ...) {
              ## convenience wrapper. Don't want any logic here to avoid
              ## duplication
              install_packages(versions_df(pkgs)$name, repos = pkgs, verbose = verbose, ...)

})

##'@rdname install
##' @aliases install_packages,character,SessionManifest

setMethod("install_packages", c(pkgs = "character", repos= "SessionManifest"), function(pkgs, repos, verbose, ...) {

              if(nrow(versions_df(repos))) {
                  vdf = versions_df(repos)
                  rownames(vdf) = vdf$name
                  vers = vdf[pkgs, "version"]
                  ghrepo = lazyRepo(pkgs = pkgs, versions = vers, pkg_manifest = manifest(repos))
              } else {
                  ghrepo = contrib.url(dep_repos(repos))
                  
              }
    .install_packages(pkgs = pkgs, lazyrepo = ghrepo, man = manifest(repos), ...)
})




##' @rdname install
##' @aliases install_packages,character,PkgManifest
          
setMethod("install_packages", c(pkgs = "character", repos= "PkgManifest"), function(pkgs, repos, versions, verbose,...) {
              if(nrow(manifest_df(repos)) == 0) {
                  ## This means we don't really have a manifest, so no
                  ## need to do a lazy repo...
                  ## The only reason we don't directly call install.packages in
                  ## this case is for the DESCRIPTION annotation.
                  ghrepo = contrib.url(dep_repos(repos))
              } else {
                  if(missing(versions) || is.null(versions))
                      versions = rep(NA_character_, times = length(pkgs))
                  else if (is(versions, "data.frame")) {
                      ord = match(pkgs, versions$name)
                      ord = ord[!is.na(ord)]
                      versions = versions$name[ord]
                  } else if (!is(versions, "character") ||
                             (length(versions) != length(pkgs) && is.null(names(versions))))
                        stop("unsupported specification of package versions")
                  
                  if(is.null(names(versions)))
                      names(versions) = pkgs
                  mtch = match(pkgs, names(versions))
                  miss = is.na(mtch)
                  if(any(miss)) {
                      new = rep(NA_character_, times = sum(miss))
                      names(new) = pkgs[miss]
                      versions = c(versions, new)
                  }
              
              
                  ghrepo= lazyRepo(pkgs, repos, verbose = verbose, versions = versions)
              }
              .install_packages(pkgs, ghrepo, man = repos, ...)
          })

## @param man A PkgManifest
.install_packages = function(pkgs, lazyrepo, man, type = "source", ...) {
    if ("lib" %in% list(...))
        libloc = list(...)["lib.loc"]
    else
        libloc = .libPaths()[1]
    if(type != "source")
        warning("using type other than source is not officially supported with switchr. Use at your own risk")
    
    avail1 = available.packages(lazyrepo, type = "source")
    avail2 = available.packages(contrib.url(dep_repos(man), type = type))
    new = !avail2[,"Package"] %in% avail1[,"Package"]
    avail = rbind(avail1, avail2[new,])
    
    oldpkgs = installed.packages(libloc)[,"Package"]
    oldinfo = lapply(oldpkgs, function(x) file.info(system.file("DESCRIPTION", package = x)))
    
    utils::install.packages(pkgs, available = avail, repos = unique(c(lazyrepo, contrib.url(dep_repos(man)))),
                            type = type, ...)

    newpkgs  = installed.packages(libloc)[,"Package"]

    newinds = !newpkgs %in% oldpkgs
    if(!all(newinds)) {
        possupdates = newpkgs[!newinds]
        
        newinfo = lapply(possupdates, function(x) file.info(system.file("DESCRIPTION", package = x)))
        oldmatchinds = match(possupdates, oldpkgs) ## should never be NA because of how possupdates is defined
        
        updated = mapply(function(old, new) !identical(old, new), old = oldinfo[oldmatchinds],
            new = newinfo)
        installedpkgs = c(newpkgs[newinds], newpkgs[updated])
    } else
        installedpkgs = newpkgs

    ## wrap in try so that permission errors don't ill the call entirely, even though
    ## the important stuff has already happened.
    try(annotateDESCs(installedpkgs, man))
    installedpkgs
}

    
    
     
