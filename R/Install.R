
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

    
    
     
