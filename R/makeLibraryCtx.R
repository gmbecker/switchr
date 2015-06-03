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
##' @param verbose Should informative messages be emitted to the console
##' @export
makeLibraryCtx = function(name, seed=NULL, pkgs, exclude.site = TRUE,
                   contains, rvers = NULL, verbose=FALSE) {

    cenv = findCompEnv(name = name, rvers = rvers)
    
    if(!is.null(cenv))
        return(cenv)
    
    libloc = file.path(switchrBaseDir(), name)
    
    if(!file.exists(libloc))
        dir.create(libloc, recursive=TRUE)
    oldlp = .libPaths()
    .libPaths2(libloc, exclude.site)
    on.exit(.libPaths(oldlp))    

    if(missing(pkgs) && !missing(seed)) {
        if(is(seed, "SessionManifest"))
            install_packages(seed)
        else if(is(seed, "PkgManifest"))
            pkgs = manifest_df(seed)$name
        else if(is(seed, "character")) {
            avl = tryCatch(available.packages(contrib.url(seed, type = "source"), type="source"))
            if(!is(avl, "error"))
                pkgs = avl[,"Package"]
        }
            
    }

    if(!missing(pkgs))
        install_packages(pkgs, repos = seed, type="source")
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
