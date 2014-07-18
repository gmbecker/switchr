
Renvs= new.env()

##' Install a new set of packages
##'
##' @param repo_base The repo base url. repo_name (which may be "") will be concatenated to it
##' @param doi (optional) a DOI to associate with this computing environment (typically the DOI of the work being reproduced)
##' @param repo_name The name of the repository (on top of repo_base). Defaults to doi if it is specified, and "" otherwise
##' @param pkgs The packages to install from the specified repository. Defaults to all packages in the repo
##' @param name The name to associate with the computing environment Defaults to doi if it is non-null, and a hash of the full repository url otehrwise
##' @param exclude.site Whether packages installed in the current R_LIBS_SITE location should be counted as installed dependencies 
##' oc
##' @export
##' @importFrom digest digest
#compEnvFromRepo = function(repo, pkgs = available.packages(contrib.url(repo))[,"Package"], libloc, name, exclude.site = TRUE, switchTo = FALSE, deps_repos = c(defaultGRAN(), biocinstallRepos()), download.dir = NULL, ...) {
installCompEnv = function(repo_base,
    doi,
    repo_name = if(!missing(doi)) doi else "",
    pkgs = available.packages(contrib.url(repo))[,"Package"],
    name = if(!missing(doi)) doi else digest(paste(repo_base, repo_name, sep="/")),
    exclude.site = TRUE,
    switchTo = FALSE,
    deps_repos = c(defGRAN, bioc),
    download.dir = tempdir(), ...) {
    ##need to make sure any dependencies that live in the site lib get installed if the environment is intended to be self-sufficient (exclude.site=TRUE)
    sep = if(grepl("http", repo_base)) "/" else .Platform$file.sep
    repo = paste(repo_base, repo_name, sep = sep)

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

    ## We want to be guaranteed to always get the version in repo, even if it is lower than the same package in one of the deps_repos.
    avail = available.packages(contriburl = contrib.url(c(repo, deps_repos)), filters = c("R_version", "OS_type"))
    corepkgs = avail[avail[,"Repository"] == contrib.url(repo), "Package"]
    torem = which(avail[,"Package"] %in% corepkgs & avail[,"Repository"] != contrib.url(repo))
    avail = avail[-torem,]


    pkgtbs = download.packages(pkgs, repos = unique(c(repo, deps_repos)), destdir = download.dir, available = avail)[,2]

    tmprepo = tempRepo(tarballs = pkgtbs)
    
##    install.packages(pkgs, lib = libloc, repos = unique(c(repo, deps_repos)), destdir = download.dir, INSTALL_opts = sprintf("--library=%s", libloc), available = avail, ...)
    install.packages(pkgs, lib = libloc, repos = paste("file://", normalizePath(tmprepo), sep=""), INSTALL_opts = sprintf("--library=%s", libloc), ...)
    .libPaths(oldlp)
    on.exit(NULL)
    ret = RComputingEnv(name, libpaths = libloc, exclude.site = exclude.site, src_url = repo)
    write.dcf(data.frame(name = name, url = repo, paths = paste(libloc, collapse=";"), excl.site = exclude.site, rversion = paste(R.version$major, R.version$minor, sep=".")), file = file.path(libloc[1], "lib_info"))
    updateManifest()
    if(switchTo)
        switchTo(ret)
    ret
}

        
##' switchTo
##' @export
setGeneric("switchTo", function(Renv = NULL, reverting = FALSE, ignoreRVersion = FALSE,  ...) standardGeneric("switchTo"))


basepkgs = installed.packages(priority="base")[, "Package"]
dontunload = c(basepkgs, "switchr", "digest")

setMethod("switchTo", "character", function(Renv, reverting = FALSE, ...) {
    if(ignoreRVersion)
        rvers = NULL
    else
        rvers = paste(R.version$major, R.version$minor, sep=".")
    cenv = findCompEnv(url = Renv, name = Renv, rvers = rvers)
    if(is.null(cenv))
       cenv = installCompEnv(repo_base = Renv, ...)
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

        lded = names(sessionInfo()$loadedOnly)
        atched = names(sessionInfo()$otherPkgs)
        pkgs = c(lded, atched)
        pkgs = pkgs[! pkgs %in%  dontunload]
        
        sapply(pkgs, function(x) {
            dll = getLoadedDLLs()[[x]]
            unloadNamespace(getNamespace(x))
            if(!is.null(dll))
                library.dynam.unload(x, dirname(dirname(dll[["path"]])))

        } )
    
        
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

##' @export
setGeneric("attachedPkgs<-", function(Renv, value) standardGeneric("attachedPkgs<-"))
setMethod("attachedPkgs<-", "RComputingEnv", function(Renv, value) {
    Renv@attached = value
    Renv
})

##' @export
setGeneric("announce", function(Renv, reverted=FALSE) standardGeneric("announce"))

setMethod("announce", "RComputingEnv", function(Renv, reverted=FALSE) {
    message(sprintf("%s to the '%s' computing environment. %d packages are currently available. Packages installed in your site library ARE %ssuppressed.\n To switch back to your previous environment type switchBack()", ifelse(reverted, "Reverted", "Switched"), Renv@name, nrow(Renv@packages), ifelse(Renv@exclude.site, "", "NOT ")))
})

##' @export
setGeneric("library_paths", function(Renv) standardGeneric("library_paths"))

setMethod("library_paths", "RComputingEnv", function(Renv) {
    Renv@libpaths
})

##' @export
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
##' @export
switchBack = function() {
    if(length(Renvs$stack) < 2) {
        warning("No previous computing environment to switch back to. Computing environment will remain unchanged")
        return(NULL)
    }
    switchTo(Renvs$stack[[2]], reverting=TRUE)
}

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

    
