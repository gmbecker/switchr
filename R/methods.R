Renvs= new.env()

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
##' @param exclude.site Should the Site library be excluded when creating
##' and switching to the specified library. Defaults to \code{TRUE}
##' @param ... Passed directly to \code{makeLibraryCtx} if an existing
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
##' @note By default, this process involves a call to \code{flushSession} which will
##' attempt to unload all loaded packages. While some support of configuring
##' what is unloaded is provided via \code{switchrDontUnload}, it is recommended
##' that you turn this feature entirely off via \code{switchrNoUnload(TRUE)} when
##' using switchr within dyanmic documents (.Rnw/.Rmd files, etc), particularly
##' when using the knitr package.
##' @return Invisibly returns the SwitchrCtx object representing the new
##' computing environment
##' @seealso \code{\link{makeLibraryCtx}}
##'
##' @examples
##' \dontrun{
##' switchTo("mynewlibrary")
##' switchBack()
##' 
##' fdman = GithubManifest("gmbecker/fastdigest")
##' switchTo("fastdigestlib", seed = fdman)
##' }
##' @export
##' @references Becker G, Barr C, Gentleman R, Lawrence M; Enhancing Reproducibility and Collaboration via Management of R Package Cohorts. Journal of Statistical Software, 81(1). 2017. doi: 10.18637/jss.v082.i01 
##' @docType methods
##' @rdname switchTo
setGeneric("switchTo", function(name, seed = NULL, reverting = FALSE,
                                ignoreRVersion = FALSE,
                                exclude.site = TRUE, ...)
    standardGeneric("switchTo"),
    signature = c("name", "seed"))

##' @rdname switchTo
##' @aliases switchTo,character,character
setMethod("switchTo", c(name = "character", seed = "character"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,
                   exclude.site = TRUE, ...) {
        
    
    ## At this point seed is guaranteed to be a repo url
    
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")

    cenv = findCompEnv(name = name, rvers = rvers)

    if(is.null(cenv)) {
            chtype = getStringType(seed)
            if(chtype == "file") {
                seed = readLines(seed)
                chtype = getStringType(seed)
            }
            
            if(chtype == "sessioninfo") {
                ## we have session info output
                ##XXX need to make sure double use of ... is safe!
                seed2 = makeSeedMan(parseSessionInfoString(seed))
                sr = lazyRepo(seed2, ...)
                
                
                seed = if(grepl("file://", sr)) sr else makeFileURL(sr)
                seed = gsub("(/|\\\\)src(/|\\\\)contrib.*", "", seed)
                chtype = "repourl"
                
            } else if (chtype == "manifesturl") {
                seed = strsplit(RCurl::getURL(seed), "\n")[[1]]
                chtype = "manifesttxt"
            }
            if (chtype == "manifesttxt") {
                con = textConnection(seed)
                on.exit(close(con))
                seed2 = loadManifest(con)
                close(con)
                on.exit(NULL)

                
                sr = lazyRepo(seed2, ...)
                seed = if(grepl("file://", sr)) sr else makeFileURL(sr)
                seed = gsub("(/|\\\\)src(/|\\\\)contrib.*", "", seed)
                
                chtype = "repourl"
            }
            
            if(grepl("(repo|contrib)", chtype)) {
                seed = mapply(repoFromString, str = seed, type = chtype)
                chtype = "repourl"
            }
            
            if(chtype != "repourl") {
                stop("We should have a repository by this point. This shouldn't happen. Contact the maintainers")
            }
            
            cenv = makeLibraryCtx(name = name, seed = seed,
                                  exclude.site = exclude.site,  ...)
        } else {
            message(sprintf("Library %s already exists. Ignoring seed and switching to existing library", name))
        }
    if(!is.null(cenv))
        switchTo(name = cenv)
    else
        stop("unable to switch to computing environment")
})

repoFromString = function(str, type) {
    switch(type,
           repodir = makeFileURL(str),
           contribdir = makeFileURL(gsub("/(src|bin/windows|bin/macosx|bin/macos).*", "", str)),
           repourl = str,
           contriburl = gsub("/(src|bin/windows|bin/macosx|bin/macos).*", "", str))
}
           

##' @rdname switchTo
##' @aliases switchTo,character,SwitchrCtx

setMethod("switchTo", c(name = "character", seed= "SwitchrCtx"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,
                   exclude.site= TRUE, ...) {
              
              if(ignoreRVersion)
                  rvers = NULL
              else
                  rvers = paste(R.version$major, R.version$minor, sep=".")
              exsting = findCompEnv(name = name, rvers = rvers)
              if(!is.null(exsting)) {
                  message("Found existing switchr context. Ignoring seed value")
                  return(switchTo(exsting))
              }
              cenv = makeLibraryCtx(name = name, seed = NULL,
                  exclude.site = seed@exclude.site,
                  ...)
              
              ## copy existing library contents to the new one
              dirs = list.dirs(file.path(switchrBaseDir(), seed@name), recursive = FALSE)

              file.copy(dirs, library_paths(cenv)[1],
                        recursive = TRUE, overwrite = FALSE)

              cenv = update_pkgs_list(cenv)
              switchTo(cenv)
          })

##' @rdname switchTo
##' @aliases switchTo,character,missing

setMethod("switchTo", c("character", "missing"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,
                   exclude.site = TRUE, ...) {
              
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")

    cenv = findCompEnv(name = name, rvers = rvers)

    if(is.null(cenv))
        cenv = makeLibraryCtx(name = name, exclude.site = exclude.site, ...)


    if(!is.null(cenv))
        ##        switchTo(name = name, seed = cenv)
        switchTo(name = cenv)
    else
        stop("unable to switch to computing environment")
})


gistregex = "gist\\.githubusercontent\\.com"


getStringType = function(str) {
    if(any(grepl("Platform:", str)))
        return("sessioninfo")
    if(grepl("^# R manifest", str[1]))
        return("manifesttxt")
    
    if(length(str) > 1) {
        ret = unique(sapply(str, getStringType))
        if(length(ret)  > 1)
            stop("Got mixed string types (likely in seed). This shouldn't happen.")
        return(ret)
    }

    
    if(grepl("file://", str)) {
        isfilurl = TRUE
        str = fileFromFileURL(str)
    } else
        isfilurl = FALSE
    
    if(file.exists(str)) {
        if( !file.exists(file.path(str, ".")))
            ret = "file"
        else { #if str points to a directory
            if(grepl("contrib/{0,1}$", str))
                ret = "contribdir"
            else if(file.exists(file.path(str,
                                     "src/contrib/PACKAGES")))
                ret = "repodir"
            else
                ret = "manifestdir"
            
        }
        if(!is.null(ret)) {
            if(ret != "file" && isfilurl)
                ret = gsub("dir$", "url", ret)
            return(ret)
        }
    } else if (isfilurl) { # file doesn't exist, but its a file url
        stop("file urls to non-existent files are not allowed as seeds/repos")
    }
                                     
    ## gist urls have a weird thing where if you put *any* valid url
    ## after a gist raw link that works you get the same contents
    ## rather than 404, so the check for PACKAGES.gz isn't safe
    ## until after we've ruled out a gist
    if(grepl(gistregex, str)){
        if(!grepl("/raw/", str))
            stop("When seeding with a manifest within a gist, use the URL to the raw file contents, not the overall gist URL.")
        return("manifesturl")
    }

    ## https://www.stats.ox.ac.uk/pub/RWin/garbage redirects to the oxford stats homepage,
    ## thus "succeeds"
    
    if (grepl("(cran|cloud.r-project.org)", str, ignore.case=TRUE) ||
        url.exists(paste0(str, "/src/contrib/PACKAGES.gz")))       
      return("repourl")
    else if(url.exists(paste0(str, "/PACKAGES.gz")))
        return("contriburl")
    else if (url.exists(str))
        return("manifesturl")
    
    stop("Unidentifiable string:", str)
}

##' @rdname switchTo
##' @aliases switchTo,SwitchrCtx,ANY

setMethod("switchTo", c(name = "SwitchrCtx", seed = "ANY"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,
                   exclude.site = TRUE,
                    ...) {
        if(is.null(Renvs$stack)) {
            paths = .libPaths()
            paths = paths[!paths %in% c(.Library.site, .Library)]
            Renvs$stack = list(original = SwitchrCtx("original", paths, exclude.site=FALSE, seed = NULL))
        }

        if(!switchrNoUnload())
            flushSession()

        .libPaths2(library_paths(name), name@exclude.site)
        
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
                                             exclude.site = TRUE,
                            ...) {
    if(any(c("pkgs", "repo_name") %in% names(list(...))))
        stop("Cannot specify pkgs or repo_name when switching to a RepoSubset")
    ##seed is a RepoSubset object

    if(missing(name)) {
        name = seed@default_name
    }
        
    switchTo(seed = seed@repos, name = name, pkgs = seed@pkgs,
             exclude.site = exclude.site, ...)
})


##' @rdname switchTo
##' @aliases switchTo,character,PkgManifest

setMethod("switchTo", c("character", seed = "PkgManifest"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,
                   exclude.site = TRUE, ...) {
    
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")
    exsting = findCompEnv(name = name, rvers = rvers)
    if(!is.null(exsting)) {
        message("Found existing switchr context. Ignoring seed value")
        return(switchTo(exsting))
    }
    cenv = makeLibraryCtx(name = name, seed = seed,
                          exclude.site = exclude.site,
                          ...)
    ## oldlp = .libPaths()
    ## .libPaths2(library_paths(cenv), cenv@exclude.site)
    ## on.exit(.libPaths2(oldlp))
    
    ## install_packages(manifest_df(seed)$name, seed, lib = library_paths(cenv)[1])
    ## cenv = update_pkgs_list(cenv)
    ## .libPaths2(oldlp)
    ## on.exit(NULL)
    switchTo(cenv)
})


##' @rdname switchTo
##' @aliases switchTo,character,SessionManifest

setMethod("switchTo", c("character", seed = "SessionManifest"),
          function(name, seed, reverting = FALSE, ignoreRVersion = FALSE,
                   exclude.site = TRUE, ...) {
              
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")
    exsting = findCompEnv(name = name, rvers = rvers)
    if(!is.null(exsting)) {
        message("Found existing switchr context. Ignoring seed value")
        return(switchTo(exsting))
    }
    cenv = makeLibraryCtx(name = name, seed = seed, #NULL,
                          exclude.site = exclude.site,
                          ...)
    
    
    
#    install_packages(pkgs = seed, lib = library_paths(cenv)[1])
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
    message(sprintf("%s to the '%s' computing environment. \n%d packages are currently available.", ifelse(reverted, "Reverted", "Switched"),
                    seed@name,  nrow(seed@packages)))
    if(seed@exclude.site)
        message("Packages installed in your site library are suppressed.")
    message("To switch back to your previous environment type switchBack()")
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



##' .libpaths2
##'
##' A version of .libPaths which allows for excluding the site library
##'
##' @param fulllp The libpath to use, as in .libPaths
##' @param exclude.site logical. Should the site library be suppressed.
##' Defaults to TRUE
##' @details Behaves exactly as the .libPaths function does, with the exception
##' of optionally excluding the site library
##' @rdname librarypath
##' @export

.libPaths2 = function(fulllp, exclude.site=TRUE) {
    fun = .libPaths
    lst = list()
    lst$.Library.site = if(exclude.site) character() else .Library.site
    
    environment(fun) = list2env(lst,
                   parent = environment(.libPaths))
    fun(fulllp)
}
