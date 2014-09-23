
basepkgs = installed.packages(priority="base")[, "Package"]
dontunload = c(basepkgs, "switchr", "digest")

##' Empty current R session
##'
##' @param dontunload Non-base packages to ignore (not detatch/unload)
##' @export
##' 

flushSession = function(dontunload = c("switchr", "digest")) {
    
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
    
}
