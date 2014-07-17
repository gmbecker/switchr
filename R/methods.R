
Renvs= new.env()

##' @export
#compEnvFromRepo = function(repo, pkgs = available.packages(contrib.url(repo))[,"Package"], libloc, name, exclude.site = TRUE, switchTo = FALSE, deps_repos = c(defaultGRAN(), biocinstallRepos()), download.dir = NULL, ...) {
installCompEnv = function(repo_base,
    doi,
    repo_name = if(!missing(doi)) doi else "",
    pkgs = available.packages(contrib.url(repo))[,"Package"],
    libloc,
    name,
    exclude.site = TRUE,
    switchTo = FALSE,
    deps_repos = c(defGRAN, bioc),
    download.dir = NULL, ...) {
    ##need to make sure any dependencies that live in the site lib get installed if the environment is intended to be self-sufficient (exclude.site=TRUE)
    sep = if(grepl("http", repo_base)) "/" else .Platform$file.sep
    repo = paste(repo_base, repo_name, sep = sep)
    
    if(!file.exists(libloc))
        dir.create(libloc, recursive=TRUE)
    oldlp = .libPaths()
    if(exclude.site)
        .libPaths2(unique(.Library))
    else 
        .libPaths2(unique(c(.Library.site, .Library)))
    on.exit(.libPaths(oldlp))

    install.packages(pkgs, lib = libloc, repos = unique(c(repo, deps_repos)), destdir = download.dir, INSTALL_opts = sprintf("--library=%s", libloc), ...)
    .libPaths(oldlp)
    on.exit(NULL)
    ret = RComputingEnv(name, libpaths = libloc, exclude.site = exclude.site)
    if(switchTo)
        switchToCompEnv(ret)
    ret
}

        
##' switchToCompEnv
##' @export
setGeneric("switchToCompEnv", function(Renv, reverting = FALSE, ...) standardGeneric("switchToCompEnv"))

basepkgs = installed.packages(priority="base")[, "Package"]


setMethod("switchToCompEnv", "RComputingEnv", function(Renv, reverting=FALSE, reloadPkgs = FALSE, ...) {
        if(is.null(Renvs$stack)) {
            paths = .libPaths()
            paths = paths[!paths %in% c(.Library.site, .Library)]
            Renvs$stack = list(original = RComputingEnv("original", paths, exclude.site=FALSE))
        }

        lded = names(sessionInfo()$loadedOnly)
        atched = names(sessionInfo()$otherPkgs)
        pkgs = c(lded, atched)
        pkgs = pkgs[! pkgs %in%  c( "RComputingEnvs", basepkgs)]
        
        sapply(pkgs, function(x) {
            dll = getLoadedDLLs()[[x]]
            if(!is.null(dll))
                library.dynam.unload(x, dirname(dirname(dll[["path"]])))
            unloadNamespace(getNamespace(x))
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
    message(sprintf("%s to the '%s' computing environment. %d packages are currently available. Packages installed in your site library ARE %ssuppressed.\n To switch back to your previous environment type revertCompEnv()", ifelse(reverted, "Reverted", "Switched"), Renv@name, nrow(Renv@packages), ifelse(Renv@exclude.site, "", "NOT ")))
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
revertCompEnv = function() {
    if(length(Renvs$stack) < 2) {
        warning("No previous computing environment to revert to. Computing environment will remain unchanged")
        return(NULL)
    }
    switchToCompEnv(Renvs$stack[[2]], reverting=TRUE)
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

    
