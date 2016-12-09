findCompEnv = function(url = NULL, name, rvers = NULL, allMatches = FALSE) {
    if(missing(url) && missing(name) || is.null(url) && is.null(name))
        stop("Must specify either a url or a name for the desired context")
    man = switchrManifest()
    if(!is.null(rvers))
        man = man[man$rversion == rvers,]

    i = numeric()

    i = which(name == man$name)
    if(!length(i))
        return(NULL)
    else {
        manrow = man[i,]
        if(allMatches)
            return(mapply(SwitchrCtx, name = manrow$name,
                          seed = list(NULL), ## XXX TODO
                          libpaths = lapply(manrow$paths,
                              function(x) strsplit(x, ";")[[1]]),
                          exclude.site = manrow$excl.site))
        else
            return(SwitchrCtx(name = manrow$name,
                                 seed = NULL, ## XXX TODO
                                 libpaths = strsplit(manrow$paths, ";")[[1]],
                                 exclude.site = manrow$excl.site))
    }
}
            

##' Get or set the base directory for switchr libraries
##'
##' @param value A new value for the base directory
##' @details If \code{value} is missing, the current base directory
##' is returned. Otherwise the \code{value} is set as the default directory
##' and returned.
##' @export
switchrBaseDir = function(value) {
    if(missing(value))
        if(is.null(switchrOpts$basedir)) "~/.switchr" else switchrOpts$basedir
    else {
        if(!file.exists(value))
            dir.create(value, recursive=TRUE)
        switchrOpts$basedir = value
    }
}

##' switchrManifest
##'
##' Generate a manifest of all currently available (existing) swtichr libraries.
##'
##' @return A data.frame with information about the located switchr libraries
##' @note This function reads cached metadata from the current switchr base
##' directory (~/.switchr by default). This cache is updated whenever
##' the switchr framework is used to create or destroy a switchr library,
##' but will not be updated if one is added or removed manually. In
##' such cases \code{\link{updateManifest}} must be called first
##' @export
switchrManifest = function() {
    dir = switchrBaseDir()
    manfile = file.path(dir, "manifest.dat")
    
    if(!file.exists(manfile) || length(readLines(manfile)) == 0)
        data.frame(url = character(), name = character(), libpaths = character(),
                   stringsAsFactors = FALSE, rversion = character())
    else
        read.table(file.path(dir, "manifest.dat"), header=TRUE, stringsAsFactors=FALSE)
}

##' updateManifest
##'
##' Update the cached information regarding available switchr libraries.
##' @return NULL, used for it's side-effect of updating the switchr library
##' metadata cache.
##' @importFrom utils write.table
updateManifest = function() {
    dirs = list.dirs(switchrBaseDir(), recursive=FALSE)
    fils = file.path(dirs, "lib_info")
    fils = fils[file.exists(fils)]
                     
    man = do.call(rbind.data.frame, lapply(fils, function(x) read.table(x, stringsAsFactors = FALSE, header = TRUE)))
    if(nrow(man) > 0)
        write.table(man, file = file.path(switchrBaseDir(), "manifest.dat"))
    else
        file.remove(file.path(switchrBaseDir(), "manifest.dat"))
    NULL
}
