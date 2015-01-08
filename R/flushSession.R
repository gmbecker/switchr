
basepkgs = installed.packages(priority="base")[, "Package"]
##'switchrDeps
##' The base packages, as well as switchr and its dependencies.
##' @export
switchDeps = c(basepkgs, "switchr", "digest", "RCurl", "bitops")#, "BiocInstaller")

##' flushSession
##' Unload currently loaded packages from the current R session
##'
##' @param dontunload Non-base packages to ignore (not detatch/unload)
##' @details Attached packages are detached (and unloaded) first. After this is
##' done, loaded packages, such as those imported by (previously) attached
##' packages, are unloaded.
##'
##' Finally, after all packages have been unloaded, native libraries
##' loaded by those packages are unloaded (on systems where this is supported).
##' @return NULL, called for its side-effect of unloading packages
##' @note Failing to include switchr, any of its dependencies, or any base
##' packages (available as a vector in the \code{\link{switchDeps}} object)
##' in \code{dontunload} will result in undefined, likely erroneous behavior. 
##' @export
##' 

flushSession = function(dontunload = switchDeps) {
    
    dontunload = c(basepkgs, dontunload)
    
    atched = names(sessionInfo()$otherPkgs)
    if(is.null(atched))
        atched = character()
    
    ## detatch attached packages
    sapply(atched[!atched %in% dontunload], function(x) {
        pkg = paste("package", x, sep=":")
        detach(pkg, character.only = TRUE)
            
    })
    
    ## unload imported namespaces
    ##while loop to deal with interdependencies between loaded namespaces
    lded = rev(names(sessionInfo()$loadedOnly))
    lded = lded[!lded %in% dontunload]
    lded2 = lded
    cnt = 1
    while(length(lded) && cnt < 1000) {
        sapply(lded, function(x) {
            res = tryCatch(unloadNamespace(getNamespace(x)), error = function(e) e)
            if(!is(res, "error")) {
            }
        })
        lded = rev(names(sessionInfo()$loadedOnly))
        lded = lded[!lded %in% dontunload]
        cnt = cnt +1
    }
    ## while loop never naturally completed
    if(cnt == 1000)
        warning("Unable to unload all namespaces")
    
    ##deal with all DLLs now that the rest is done.
    pkgs = unique(c(atched, lded2))
    pkgs = pkgs[! pkgs %in%  dontunload]            
    sapply(pkgs, function(x) {
        dll = getLoadedDLLs()[[x]]
        
        if(!is.null(dll))
            tryCatch(library.dynam.unload(x, dirname(dirname(dll[["path"]]))), error = function(e) NULL)
    })

    NULL
}
