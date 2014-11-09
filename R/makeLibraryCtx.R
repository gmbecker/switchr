##' makeLibraryCtx
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
            Install(seed)
        else if(is(seed, "PkgManifest"))
            pkgs = manifest_df(seed)$name

    if(!missing(pkgs))
        Install(pkgs, repos = seed)
    write.table(data.frame(name = name, url = "", paths = libloc,
                           excl.site = exclude.site,
                           rversion = gsub("R version ([^ ]*).*", "\\1",
                               R.version.string),
                           stringsAsFactors=FALSE),
                file = file.path(libloc[1], "lib_info")
                )
    updateManifest()
    RComputingEnv(name = name, libpaths = libloc, exclude.site = exclude.site, seed = seed)
}
