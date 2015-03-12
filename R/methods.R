Renvs= new.env()
##this doesn't seem to work anyway...
#if(getRversion() >= "2.15.1") globalVariables(".lib.loc")

##' switchTo
##'
##' Switch to a different computing environment (set of installed R packages
##' and library location paths for new pkg installs)
##'
##' If switchr does not now about the specified computing environment, a new one
##' will be created via installCompEnv. This includes
##' creating a directory under the switchr base directory and installing
##' packages into it. See \code{installCompEnv} for more details.
##'
##' @param name The name associated (or to associate) with the computing
##' environment.
##' @param seed The seed, indicating packages to install into a newly created
##' package library
##' No effect if the library already exists
##' @param reverting Indicates whether we are reverting to the environment in
##' use before the current one. Typically not set directly by the user.
##' @param ignoreRVersion Should the R version in use be ignored when checking
##' for existing computing environmeSnts. This is experimental.
##' @param ... Passed directly to \code{installCompEnv} if an existing
##' computing environment is not found.
##' @details This function has the side effect of unloading all loaded
##' packages (other than base packages, GRAN or GRANBAse,  switchr itself, and
##' switchr's dependencies) and the associated DLLs. It also changes the library
##' location R will use to search for packages, e.g. when you call
##' \code{library}.
##'
##' This means you will have to reinstall packages after switching, which is
##' important and intended (e.g. when switching to using Bioc devel from Bioc
##' release).
##'
##'
##' @return Invisibly returns the SwitchrCtx object representing the new
##' computing environment
##' @export
##' @docType methods
##' @rdname switchTo
setGeneric("switchTo", function(name, seed = NULL, reverting = FALSE,
                                ignoreRVersion = FALSE,  ...)
           standardGeneric("switchTo"))

##' @rdname switchTo
##' @aliases switchTo,character,character
setMethod("switchTo", c(name = "character", seed = "character"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE, ...) {
    chtype = getStringType(seed)
    if(chtype == file) {
        seed = readLines(seed)
        chtype = getStringType(seed)
    }
    
    if(chtype == "sessioninfo") {
        ## we have session info output
        ##XXX need to make sure double use of ... is safe!
        sr = lazyRepo(seed, ...)

        
        seed = if(grepl(sr, "file://")) sr else paste("file://",sr, sep="")
        seed = gsub("/src/contrib.*", "", seed)
        chtype = "repourl"
          
    } else if (grepl("(repo|contrib)", chtype)) {
        seed = repoFromString(seed, chtype)
        chtype = "repourl"
    }

    if(chtype != "repourl") {
        man = readManifest(seed)
        if(!is(man, "SessionManifest"))
            man = SessionManifest(versions = data.frame(name = manifest_df(man)$name,
                                      version = NA, stringsAsFactors=FALSE), manifest = man)
        seed = lazyRepo(man)
        chtype = "repourl"
    }
        
    
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")

    cenv = findCompEnv(name = name, rvers = rvers)

    if(is.null(cenv))
        cenv = makeLibraryCtx(name = name, seed = seed, ...)

    if(!is.null(cenv))
        ##        switchTo(name = name, seed = cenv)
        switchTo(name = cenv)
    else
        stop("unable to switch to computing environment")
})

repoFromString = function(str, type) {
    switch(type,
           repodir = paste("file://", str, sep=""),
           contribdir = paste("file://",
               gsub("/(src|bin/windows|bin/macosx).*", "", str),
               sep=""),
           reporul = str,
           contriburl = gsub("/(src|bin/windows|bin/macosx).*", "", str))
}
           

##' @rdname switchTo
##' @aliases switchTo,character,SwitchrCtx

setMethod("switchTo", c(name = "character", seed= "SwitchrCtx"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,...) {
              
              if(ignoreRVersion)
                  rvers = NULL
              else
                  rvers = paste(R.version$major, R.version$minor, sep=".")
              exsting = findCompEnv(name = name, rvers = rvers)
              if(!is.null(exsting)) {
                  warning("A switchr context with that name already exists")
                  switchTo(exsting)
              }
              cenv = makeLibraryCtx(name = name, seed = NULL,
                  exclude.site = seed@exclude.site,
                  ...)
              
              ## copy existing library contents to the new one
              dirs = list.dirs(file.path(switchrBaseDir(), seed@name), recursive = FALSE)
            ##  dests = file.path(library_paths(cenv)[1], basename(dirs))
    ##          dir.create(dests)
     ##         mapply(file.copy,dirs, dests, recursive=TRUE)
              file.copy(dirs, library_paths(cenv)[1],
                        recursive = TRUE, overwrite = FALSE)

              cenv = update_pkgs_list(cenv)
              switchTo(cenv)
          })

##' @rdname switchTo
##' @aliases switchTo,character,missing

setMethod("switchTo", c("character", "missing"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,...) {
              
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")

    cenv = findCompEnv(name = name, rvers = rvers)

    if(is.null(cenv))
        cenv = makeLibraryCtx(name = name, ...)


    if(!is.null(cenv))
        ##        switchTo(name = name, seed = cenv)
        switchTo(name = cenv)
    else
        stop("unable to switch to computing environment")
})


getStringType = function(str) {
    if(any(grepl("Platform:", str)))
        return("sessioninfo")
    if(length(str) > 1)
        stop("Char vector of length >1 with non-sessionInfo contents detected")

    if(file.exists(str)) {
        if( !file.exists(file.path(str, ".")))
            return("file")
        else {
            if(grepl("contrib/{0,1}$", str))
                return("contribdir")

            if(file.exists(file.path(str,
                                     "src/contrib/PACKAGES")))
                return("repodir")
            else
                return("manifestdir")
        }
                                     
    }
    if(url.exists(str)) {
        if(grepl("contrib/{0,1}$", str))
            return("contriburl")
                   
        if(url.exists(paste(str, "src/contrib/PACKAGES", sep ="")) ||
           url.exists(paste(str, "bin/windows/contrib/PACKAGES", sep ="")) ||
           url.exists(paste(str, "bin/macosx/contrib/PACKAGES", sep ="")))
            return("repourl")
        else
            return("manifesturl")
    }
    stop("Unidentifiable string:", str)
}

##' @rdname switchTo
##' @aliases switchTo,SwitchrCtx,ANY

setMethod("switchTo", c(name = "SwitchrCtx", seed = "ANY"), function(name, seed, reverting=FALSE, ...) {
        if(is.null(Renvs$stack)) {
            paths = .libPaths()
            paths = paths[!paths %in% c(.Library.site, .Library)]
            Renvs$stack = list(original = SwitchrCtx("original", paths, exclude.site=FALSE, seed = NULL))
        }

        flushSession()
        
        if(!name@exclude.site)
            .libPaths(library_paths(name))
        else
            .libPaths2(c(library_paths(name), .Library))

         if(!reverting) {
#            attachedPkgs(Renvs$stack[[length(Renvs$stack)]]) = atched
             Renvs$stack = c(name, Renvs$stack)
        } else
            Renvs$stack = Renvs$stack[-1]
        announce(name, reverted = reverting)

        invisible(name)
    })

##' @rdname switchTo
##' @aliases switchTo,character,RepoSubset

setMethod("switchTo", c(name = "character", seed="RepoSubset"), function(name, seed = NULL,
                                             reverting = FALSE,
                                             ignoreRVersion = FALSE,
                            ...) {
    if(any(c("pkgs", "repo_name") %in% names(list(...))))
        stop("Cannot specify pkgs or repo_name when switching to a RepoSubset")
    ##seed is a RepoSubset object

    if(missing(name)) {
        name = seed@default_name
    }
        
    switchTo(seed = seed@repos, name = name, pkgs = seed@pkgs, ...)
})


##' @rdname switchTo
##' @aliases switchTo,character,PkgManifest

setMethod("switchTo", c("character", seed = "PkgManifest"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE, ...) {
             
              if(ignoreRVersion)
                  rvers = NULL
              else
                  rvers = paste(R.version$major, R.version$minor, sep=".")
              exsting = findCompEnv(name = name, rvers = rvers)
              if(!is.null(exsting)) {
                  warning("A switchr context with that name already exists")
                  switchTo(exsting)
              }
              cenv = makeLibraryCtx(name = name, seed = NULL,
                  ...)
              oldlp = .libPaths()
              .libPaths2(library_paths(cenv))
              on.exit(.libPaths2(oldlp))

              install_packages(manifest_df(seed)$name, seed, lib = library_paths(cenv)[1])
              cenv = update_pkgs_list(cenv)
              .libPaths2(oldlp)
              on.exit(NULL)
              switchTo(cenv)
          })


##' @rdname switchTo
##' @aliases switchTo,character,SessionManifest

setMethod("switchTo", c("character", seed = "SessionManifest"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE, ...) {
              
              if(ignoreRVersion)
                  rvers = NULL
              else
                  rvers = paste(R.version$major, R.version$minor, sep=".")
              exsting = findCompEnv(name = name, rvers = rvers)
              if(!is.null(exsting)) {
                  warning("A switchr context with that name already exists")
                  switchTo(exsting)
              }
              cenv = makeLibraryCtx(name = name, seed = NULL,
                  ...)
              

                       
              install_packages(pkgs = seed, lib = library_paths(cenv)[1])
              cenv = update_pkgs_list(cenv)
              switchTo(cenv)
          })






setGeneric("attachedPkgs<-", function(seed, value) standardGeneric("attachedPkgs<-"))
setMethod("attachedPkgs<-", "SwitchrCtx", function(seed, value) {
    seed@attached = value
    seed
})







setGeneric("announce", function(seed, reverted=FALSE) standardGeneric("announce"))

setMethod("announce", "SwitchrCtx", function(seed, reverted=FALSE) {
    message(sprintf("%s to the '%s' computing environment. %d packages are currently available. Packages installed in your site library ARE %ssuppressed.\n To switch back to your previous environment type switchBack()",
                    ifelse(reverted, "Reverted", "Switched"),
                    seed@name, nrow(seed@packages),
                    ifelse(seed@exclude.site, "", "NOT ")))
})

setMethod("show", "SwitchrCtx", function(object) {
    message(paste(sprintf("An SwitchrCtx object defining the '%s' computing environment", object@name),
              "\n\n\t", sprintf("Primary library location(s): %s", paste(object@libpaths, collapse=";")),
              "\n\t", sprintf("Packages: %d packages installed in %d directories (including R's base library)", nrow(object@packages), length(unique(object@packages$LibPath))),
              "\n\t", paste("This environment DOES ", ifelse(object@exclude.site, "NOT ", ""), "combine with the current site library location when loaded.", sep=""),
              "\n\n"))
})

##' switchBack
##'
##' A convenience function to switch back to the previously used computing
##' environment.
##' @export
switchBack = function() {
    if(length(Renvs$stack) < 2) {
        warning("No previous computing environment to switch back to. Computing environment will remain unchanged")
        return(NULL)
    }
    switchTo(Renvs$stack[[2]], reverting=TRUE)
}

##' currentCompEnv
##'
##' Display the computing environment currently in use. If switchTo has not been
##' called, a new SwitchrCtx object describing the current environment is
##' created.
##' @export   
currentCompEnv = function() {
            if(is.null(Renvs$stack)) {
                lp = .libPaths()
                lp = lp[!(lp %in% .Library | lp %in% .Library.site)]
                Renvs$stack = list(original = SwitchrCtx("original",
                                       libpaths = lp , seed = NULL,
                                       exclude.site=FALSE))
            }
            Renvs$stack[[1]]
        }


.libPaths2 = function(fulllp) {
    fun = function(x) .lib.loc <<- unique(x)
    environment(fun) = environment(.libPaths)
    fun(fulllp)
}


    
