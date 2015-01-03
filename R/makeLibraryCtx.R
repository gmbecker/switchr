##' makeLibraryCtx
##'
##' Locate or create a specified switchr library
##'
##' This function is not intended to be called directly in most cases; switchTo
##' calls it automatically.
##' 
##' @param name The name for the library
##' @param seed The object to seed the library from
##' @param pkgs Pkgs to install upon creation. Deprecated, use a seeding object
##' instead.
##' @param exclude.site Whether the site library should be excluded when
##' switching to this library
##' @param contains Currently unused.
##' @param rvers Optional R version. If specified, existing libraries much be
##' associated with the same R version to be considered a match.
##' @export
makeLibraryCtx = function(name, seed=NULL, pkgs, exclude.site = TRUE,
                   contains, rvers = NULL) {

    cenv = findCompEnv(name = name, rvers = rvers)
    
    if(!is.null(cenv))
        return(cenv)
    
    libloc = file.path(switchrBaseDir(), name)
    
    if(!file.exists(libloc))
        dir.create(libloc, recursive=TRUE)
    oldlp = .libPaths()
    if(exclude.site)
        .libPaths2(unique(.Library))
    else 
        .libPaths2(unique(c(.Library.site, .Library)))
    on.exit(.libPaths(oldlp))    

    if(missing(pkgs) && !missing(seed))
        if(is(seed, "SessionManifest"))
            install_packages(seed)
        else if(is(seed, "PkgManifest"))
            pkgs = manifest_df(seed)$name

    if(!missing(pkgs))
        install_packages(pkgs, repos = seed)
    write.table(data.frame(name = name, url = "", paths = libloc,
                           excl.site = exclude.site,
                           rversion = gsub("R version ([^ ]*).*", "\\1",
                               R.version.string),
                           stringsAsFactors=FALSE),
                file = file.path(libloc[1], "lib_info")
                )
    updateManifest()
    SwitchrCtx(name = name, libpaths = libloc, exclude.site = exclude.site, seed = seed)
}
