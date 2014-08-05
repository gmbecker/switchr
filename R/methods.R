
Renvs= new.env()

##' Install a new set of packages
##'
##' @param repo_base The repo base url. repo_name (which may be "") will be concatenated to it
##' @param doi (optional) a DOI to associate with this computing environment (typically the DOI of the work being reproduced)
##' @param repo_name The name of the repository (on top of repo_base). Defaults to doi if it is specified, and "" otherwise
##' @param pkgs The packages to install from the specified repository. Defaults to all packages in the repo
##' @param name The name to associate with the computing environment Defaults to doi if it is non-null, and a hash of the full repository url otehrwise
##' @param exclude.site Whether packages installed in the current R_LIBS_SITE location should be counted as installed dependencies 
##' @note The default is to install *ALL* packages in the specified repository. This is appropriate for reproducibility-based repositories but NOT for
##' more standard ones like CRAN or BioConductor.
##' @export
##' @importFrom digest digest
#compEnvFromRepo = function(repo, pkgs = available.packages(contrib.url(repo))[,"Package"], libloc, name, exclude.site = TRUE, switchTo = FALSE, deps_repos = c(defaultGRAN(), biocinstallRepos()), download.dir = NULL, ...) {
installCompEnv = function(repo_base,
    doi,
    repo_name = if(!missing(doi)) doi else "",
    pkgs = available.packages(contrib.url(repo))[,"Package"],
    name,
    exclude.site = TRUE,
    switchTo = FALSE,
    deps_repos = c(defGRAN, bioc),
    download.dir = tempdir(), ...) {
    ##need to make sure any dependencies that live in the site lib get installed if the environment is intended to be self-sufficient (exclude.site=TRUE)
    sep = if(any(grepl("(http[s]{0,1}|file):", repo_base))) "/" else .Platform$file.sep
    if(missing(repo_base) || !length(repo_base)) 
        repo = ""
    else
        repo = paste(repo_base, repo_name, sep = sep)


    if(missing(name)) {
        if(!missing(doi))
            name = doi
        else if(nchar(repo))
            name = digest(repo)
        else
            stop("No name, doi, or repository identified. Unable to determine library location")
    }
       

    ## if it already exists, we're done. Load it from disk and return.
    ret = findCompEnv(url = repo, name = name, rvers = paste(R.version$major, R.version$minor, sep="."))
    if(!is.null(ret)) {
        if(switchTo)
            switchTo(ret)
        return(ret)
    }
    libloc = file.path(switchrBaseDir(), name)
    
    if(!file.exists(libloc))
        dir.create(libloc, recursive=TRUE)
    oldlp = .libPaths()
    if(exclude.site)
        .libPaths2(unique(.Library))
    else 
        .libPaths2(unique(c(.Library.site, .Library)))
    on.exit(.libPaths(oldlp))

    if(nchar(repo)) {
        ## We want to be guaranteed to always get the version in repo, even if it is lower than the same package in one of the deps_repos.
        avail = available.packages(contriburl = contrib.url(c(repo, deps_repos)), filters = c( "OS_type")) ## XXX no Rversion filtering. IS THIS A GOOD IDEA??!?!?!
        corepkgs = avail[avail[,"Repository"] %in% contrib.url(repo), "Package"]
        torem = which(avail[,"Package"] %in% corepkgs & ! avail[,"Repository"] %in% contrib.url(repo))
        avail = avail[-torem,]

        ## remove 'installed package' caches to make absolutely sure we don't hit the wrong package versions when finding dependencies
        file.remove(list.files(pattern="libloc_.*.rds", path = tempdir()))
        install.packages(pkgs, lib = libloc, repos = unique(c(repo, deps_repos)), destdir = download.dir, INSTALL_opts = sprintf("--library=%s", libloc), available = avail, ...)
    }

    .libPaths(oldlp)
    on.exit(NULL)
    ret = RComputingEnv(name, libpaths = libloc, exclude.site = exclude.site, src_url = repo)
    write.table(data.frame(name = name, url = paste(repo, collapse = ";"), paths = paste(libloc, collapse=";"), excl.site = exclude.site, rversion = paste(R.version$major, R.version$minor, sep=".")), file = file.path(libloc[1], "lib_info"))
    updateManifest()
    if(switchTo)
        switchTo(ret)
    ret
}

        
##' switchTo
##'
##' Switch to a different computing environment (set of installed R packages and library location paths for new pkg installs)
##'
##' If switchr does not now about the specified computing environment, a new one will be created via installCompEnv. This includes
##' creating a directory under the switchr base directory and installing packages into it. See \code{installCompEnv} for more details.
##'
##' @param Renv An object representing the computing environment to switch to (and if necessary install). Currently supported are RComputingEnv objects,
##' character vectors indicating repository urls, and RepoSubset objects (such as \code{BiocDevel} and \code{BiocRelease}.
##' @param reverting Indicates whether we are reverting to the environment in use before the current one. Typically not set directly by the user.
##' @param ignoreRVersion Should the R version in use be ignored when checking for existing computing environments. This is experimental.
##' @param ... Passed directly to \code{installCompEnv} if an existing computing environment is not found.
##' @details This function has the side effect of unloading all loaded packages (other than base packages, switchr itself, and switchr's dependencies) and
##' the associated DLLs. It also changes the library location R will use to search for packages, e.g. when you call \code{library}.
##'
##' This means you will have to reinstall packages after switching, which is important and intended (e.g. when switching to using Bioc devel from Bioc release).
##'
##'
##' @return Invisibly returns the RComputingEnv object representing the new computing environment
##' @export
setGeneric("switchTo", function(Renv = NULL, reverting = FALSE, ignoreRVersion = FALSE,  ...) standardGeneric("switchTo"))


basepkgs = installed.packages(priority="base")[, "Package"]
dontunload = c(basepkgs, "switchr", "digest")

setMethod("switchTo", "character", function(Renv, reverting = FALSE, ignoreRVersion = FALSE, name, ...) {
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")
    isURL = any(grepl("(http[s]{0,1}|file)://", Renv))
    if(isURL) {
        if(missing(name))
            name = digest(Renv)
        cenv = findCompEnv(url = Renv, name = name, rvers = rvers)
    } else {
        cenv = findCompEnv(name = Renv, rvers = rvers)
    }

    if(is.null(cenv)) {
        if(isURL)
            cenv = installCompEnv(repo_base = Renv, ...)
        else 
            cenv = installCompEnv(repo_base = NULL, name = Renv, ...)
    }
    if(!is.null(cenv))
        switchTo(cenv)
    else
        stop("unable to switch to computing environment")
})








setMethod("switchTo", "RComputingEnv", function(Renv, reverting=FALSE, reloadPkgs = FALSE, ...) {
        if(is.null(Renvs$stack)) {
            paths = .libPaths()
            paths = paths[!paths %in% c(.Library.site, .Library)]
            Renvs$stack = list(original = RComputingEnv("original", paths, exclude.site=FALSE, src_url=""))
        }


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
        pkgs = unique(c(atched, lded))
        pkgs = pkgs[! pkgs %in%  dontunload]            
        sapply(pkgs, function(x) {
            dll = getLoadedDLLs()[[x]]
            
            if(!is.null(dll))
                tryCatch(library.dynam.unload(x, dirname(dirname(dll[["path"]]))), error = function(e) NULL)
        })

        
        if(!Renv@exclude.site)
            .libPaths(library_paths(Renv))
        else
            .libPaths2(c(library_paths(Renv), .Library))

        if(!reverting) {
            attachedPkgs(Renvs$stack[[length(Renvs$stack)]]) = atched
            Renvs$stack = c(Renv, Renvs$stack)
       } else
            Renvs$stack = Renvs$stack[-1]
        announce(Renv, reverted = reverting)
        if(reloadPkgs)
            sapply(Renv@attached, library)
        invisible(Renv)
    })

setMethod("switchTo", "RepoSubset", function(Renv = NULL,
                                             reverting = FALSE,
                                             ignoreRVersion = FALSE,
                                             name,
                                             doi,
                                             ...) {
    if(any(c("pkgs", "repo_name") %in% names(list(...))))
        stop("Cannot specify pkgs or repo_name when switching to a RepoSubset")
    ##Renv is a RepoSubset object

    if(missing(name)) {
        if(!missing(doi))
            name = doi
        else
            name = Renv@default_name
    }
        
    switchTo(Renv@repos, name = name, pkgs = Renv@pkgs, ...)
})
          
setGeneric("attachedPkgs<-", function(Renv, value) standardGeneric("attachedPkgs<-"))
setMethod("attachedPkgs<-", "RComputingEnv", function(Renv, value) {
    Renv@attached = value
    Renv
})







setGeneric("announce", function(Renv, reverted=FALSE) standardGeneric("announce"))

setMethod("announce", "RComputingEnv", function(Renv, reverted=FALSE) {
    message(sprintf("%s to the '%s' computing environment. %d packages are currently available. Packages installed in your site library ARE %ssuppressed.\n To switch back to your previous environment type switchBack()", ifelse(reverted, "Reverted", "Switched"), Renv@name, nrow(Renv@packages), ifelse(Renv@exclude.site, "", "NOT ")))
})

##' library_paths
##'
##' Accessor for which directories an RComputingEnv is associated with.
##' @param Renv An RComputingEnv
##' @export
setGeneric("library_paths", function(Renv) standardGeneric("library_paths"))

setMethod("library_paths", "RComputingEnv", function(Renv) {
    Renv@libpaths
})

setMethod("show", "RComputingEnv", function(object) {
    cat(paste(sprintf("An RComputingEnv object defining the '%s' computing environment", object@name),
              "\n\n\t", sprintf("Primary library location(s): %s", paste(object@libpaths, collapse=";")),
              "\n\t", sprintf("Packages: %d packages installed in %d directories (including R's base library)", nrow(object@packages), length(unique(object@packages$LibPath))),
              "\n\t", paste("This environment DOES ", ifelse(object@exclude.site, "NOT ", ""), "combine with the current site library location when loaded.", sep=""),
              "\n\n"))
})

##' @export
setGeneric("packages", function(Renv) standardGeneric("packages"))
setMethod("packages", "RComputingEnv", function(Renv) Renv@packages)
##' switchBack
##'
##' A convenience function to switch back to the previously used computing environment.
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
##' Display the computing environment currently in use. If switchTo has not been called, a new RComputingEnv object
##' describing the current environment is created.
##' @export   
currentCompEnv = function() {
            if(is.null(Renvs$stack)) {
                lp = .libPaths()
                lp = lp[!(lp %in% .Library | lp %in% .Library.site)]
                Renvs$stack = list(original = RComputingEnv("original", lp , exclude.site=FALSE))
            }
            Renvs$stack[[1]]
        }


##scary and bad!!!!
.libPaths2 = function(fulllp) {
    fun = function(x) .lib.loc <<- unique(x)
    environment(fun) = environment(.libPaths)
    fun(fulllp)
}

    
