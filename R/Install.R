
##' @export
setGeneric("Install", function(pkgs, repos, versions = NULL, verbose = FALSE, ...) standardGeneric("Install"))

setMethod("Install", c("character", "character"), function(pkgs, repos, versions, verbose, ...) {

    man = PkgManifest(manifest = ManifestRow(), dep_repos = repos)
    if(!is.null(versions))
        man = SessionManifest(pkg_manifest = man, pkg_versions = version)
    Install(pkgs, repos = man, verbose = verbose, ...)
    
})



setMethod("Install", c(pkgs = "character", repos= "PkgManifest"), function(pkgs, repos, verbose, ...) {

    ghrepo= lazyRepo(pkgs, repos, verbose = verbose)
    avail1 = available.packages(ghrepo)
    avail2 = available.packages(contrib.url(dep_repos(repos)))
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
    annotateDESCs(installedpkgs, repos)
    installedpkgs
})




    
    
     
