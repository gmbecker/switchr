
basepkgs = installed.packages(priority="base")[, "Package"]
##' @title switchrDeps
##' 
##' @description The base packages, as well as switchr and its dependencies.
##'
##' @export
switchDeps = c(basepkgs, "switchr", "RCurl", "bitops", "BiocInstaller", "BiocManager", "RJSONIO")

##' @title Get or set packages to not unload when flushing the system
##'
##' @description Get or set packages which should NOT be unloaded when flushing the system,
##' e.g., when switching between libraries.
##' 
##' @param value The packages to not unload when switching libraries.
##' @param add Should \code{value} be added to the existing list?
##' @note By default switchr will not attempt to unload any base packages,
##' itself, or any of its dependencies. Attempting to unload any of these
##' packages (e.g. \code{add=FALSE}) will result in undefined behavior and
##' is not recommended.
##' @export
switchrDontUnload = function(value, add=TRUE) {
    if(missing(value)){
        if(is.null(switchrOpts$dontunload)) 
            switchrOpts$dontunload = switchDeps
        switchrOpts$dontunload
    } else {
        ## had to add our own code for this because package_dependencies is
        ## relatively new and we need switchr to install on R's as old as
        ## possible
        
        value = .dodepsourselves(value, incl_pkgs = TRUE)
        if(add)
            value = unique(c(switchrDontUnload(), value))
        switchrOpts$dontunload = value
    }

}

.dodepsourselves = function(pkg, av = available.packages(contrib.url(defaultRepos())), incl_pkgs = TRUE) {
    ## XXX should it always be?
    if(nrow(av) == 0)
        av = installed.packages()

    deps = .innerdodeps(pkg, av)
    done = pkg
    remaining = deps
    while(length(remaining) > 0) {
        d = remaining[1]
        newdeps = .innerdodeps(d, av, done = done)
        done = c(done, d)
        deps = c(deps, newdeps)
        remaining = unique(c(remaining[-1], newdeps))
    }
    if(incl_pkgs)
        deps = unique(c(pkg, deps))
    deps
}

.innerdodeps = function(pkg, av, done = character() ) {
    deps = av[pkg, c("Depends", "Imports")]
    deps = unlist(as.vector(deps))
    deps = deps[!is.na(deps)]
    if(length(deps) >0) {
        deps = unlist(strsplit(deps, "[[:space:]]*,[[:space:]]*"))
        deps = gsub("[[:space:]]*([\\._[:alnum:]]*).*", "\\1", deps)
        deps = deps[deps!= "R"]
        deps = unique(deps)
        deps = deps[!deps %in% c(done, basepkgs)]
    }
    deps
    
}
        

##' Skip unloading of packages in session
##'
##' Set whether or not ANY packages are unloaded when switching libraries.
##' 
##' @param value A logical value, or missing to return the current option
##' @return A logical indicating whether or not calling \code{flushSession} will skipped during the library switching process.
##' @details This should be set to TRUE  when using switchr in the context of dynamic documents such as .Rnw and .Rmd files. 
##' @export
##' 
switchrNoUnload = function(value) {
    if(missing(value)){
        if(is.null(switchrOpts$noflush)) 
            switchrOpts$noflush = FALSE
        switchrOpts$noflush
    } else {
        if(is.na(value))
            value = FALSE
        if(!is(value, "logical"))
            stop("The no unload option must be a logical value") 
        switchrOpts$noflush = value
    }
    
}

##' flushSession
##' 
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
##' @importFrom tools write_PACKAGES

flushSession = function(dontunload = switchrDontUnload()) {
    

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
