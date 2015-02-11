
##' dep_repos
##'
##' Get or set repositories to be used to fullfill dependencies beyond packages
##' within the manifest
##' @return Character vector with existing repository urls
##' @param x A package or session manifest
##' @rdname dep_repos
##' @docType methods
##' @export
setGeneric("dep_repos", function(x) standardGeneric("dep_repos"))

##' @rdname dep_repos
##' @aliases dep_repos,PkgManifest
setMethod("dep_repos", "PkgManifest", function(x) x@dependency_repos)

##' @rdname dep_repos
##' @aliases dep_repos,SessionManifest
setMethod("dep_repos", "SessionManifest", function(x) manifest(x)@dependency_repos)

##'
##' @rdname dep_repos
##' @param value A character vector with the new dependency repos
##' @export
setGeneric("dep_repos<-", function(x, value) standardGeneric("dep_repos<-"))

##'@rdname dep_repos
##' @aliases dep_repos<-,PkgManifest
setMethod("dep_repos<-", "PkgManifest", function(x, value) {
    x@dependency_repos = value
    x
})


##'@rdname dep_repos
##' @aliases dep_repos<-,SessionManifest
setMethod("dep_repos<-", "SessionManifest", function(x, value) {
    man = manifest(x)
    dep_repos(man) = value
    manifest(x) = man
    x
})



##' Get or set the manifest associated with an object
##' @export
##' @rdname manifest_methods
##' @param x An object which contains a manifest
##' @docType methods
##' @return A PkgManifest or SessionManifest object
setGeneric("manifest", function(x) standardGeneric("manifest"))
##setMethod("manifest", "PkgManifest", function(x) x@manifest)

##' @export
##' @rdname manifest_methods
##' @param value A PkgManifest
setGeneric("manifest<-", function(x, value) standardGeneric("manifest<-"))

##' @rdname manifest_methods
##' @aliases manifest,SessionManifest
setMethod("manifest", "SessionManifest",
          function(x) x@pkg_manifest)



##' @rdname manifest_methods
##' @aliases manifest<-,SessionManifest
 
setMethod("manifest<-", "SessionManifest",
          function(x, value ) {
              x@pkg_manifest = value
              x
          })






##' manifest_df
##'
##' Get or set the package location manifest (data.frame) associated with an
##' object
##'
##' @rdname manifest_df
##' @param x The object
##' @param ... unused.
##' @docType methods
##' @export
setGeneric("manifest_df", function(x, ...) standardGeneric("manifest_df"))

## only get manifest rows for pkgs in the 'session' by default
## override with session_only=FALSE if desired
##' @aliases manifest_df,SessionManifest
##' @param session_only Only return manifest rows associated with 
##' @rdname manifest_df

setMethod("manifest_df", "SessionManifest",
          function(x, session_only = TRUE, ...) {
              ## all pkgs in the manifest
              mdf = manifest_df(manifest(x))
              ## restrict to pkgs in the 'session' if desired
              if(session_only)
                  mdf = mdf[mdf$name %in% versions_df(x)$name,]
              mdf
          })
          


##' @aliases manifest_df,PkgManifest
##' @rdname manifest_df
setMethod("manifest_df", "PkgManifest", function(x) x@manifest)


##' @rdname manifest_df
##' @param value A data.frame of package manifest information.
##' See \code{\link{ManifestRow}}
##' @export
setGeneric("manifest_df<-", function(x, value) standardGeneric("manifest_df<-"))


##' @aliases manifest_df<-,SessionManifest
##' @rdname manifest_df

setMethod("manifest_df<-", "SessionManifest", function(x, value) {
    manifest_df(manifest(x)) = value
    x
    })

##' @aliases manifest_df<-,PkgManifest
##' @rdname manifest_df

setMethod("manifest_df<-", "PkgManifest", function(x, value) {
    x@manifest = value
    x
    })

##' versions_df
##' 
##' Get or set the the versions information in a SessionManifest
##' 
##' @param x An object containing package version information
##' @rdname versions
##' @docType methods
##' @export
setGeneric("versions_df", function(x) standardGeneric("versions_df"))

##' @aliases versions_df,SessionManifest
##' @rdname versions
setMethod("versions_df", "SessionManifest",
          function(x) x@pkg_versions)


##' @rdname versions
##' @param value A data.frame of package version information.
##' @export
setGeneric("versions_df<-", function(x, value) standardGeneric("versions_df<-"))

##' @aliases versions_df<-,SessionManifest
##' @rdname versions
setMethod("versions_df<-", "SessionManifest", function(x, value) {
    x@pkg_versions = value
    x
    })











##' Get or set the branch associated with a Package Source
##' @export
##' @param x A source
##' @rdname branch
##' @docType methods
setGeneric("branch", function(x) standardGeneric("branch"))
##' @aliases branch,PkgSource
##' @rdname branch
setMethod("branch", "PkgSource", function(x) x@branch)
##' @rdname branch
##' @param value The new branch
setGeneric("branch<-", function(x, value) standardGeneric("branch<-"))
##' @aliases branch<-,PkgSource
##' @rdname branch
setMethod("branch<-", "PkgSource", function(x, value) {
    x@branch = value
    x
    })


##' subdir
##' @rdname subdir
##' @param x An object associated with a subdirectory, typically a PkgSource
##' @docType methods
##' @export

setGeneric("subdir", function(x) standardGeneric("subdir"))
##' @aliases subdir,PkgSource
##' @rdname subdir
setMethod("subdir", "PkgSource", function(x) x@subdir)

##' @export
##' @param value The new subdirectory to associate with the object
##' @rdname subdir
setGeneric("subdir<-", function(x, value) standardGeneric("subdir<-"))
##' @rdname subdir
##' @aliases subdir<-,PkgSource
setMethod("subdir<-", "PkgSource", function(x, value) {
    x@subdir = value
    x
    })



##' location generic
##' Retreive the directory associated with an object
##' @rdname location-methods
##' @return a character containing the associated path
##' @author Gabriel Becker
##' @param repo An object associated with a path
##' @docType methods
##' @export
setGeneric("location", function(repo) standardGeneric("location"))


##' Retreive the local directory associated with a PkgSource object for a package in a GRAN manifest
##' @rdname location-methods
##' @aliases location,PkgSource-method
##' @export
setMethod("location", "PkgSource", function(repo) repo@location)

##' Set or Retrieve the shell initialization script for an object
##' @export
##' @param x An object associated with a SwitchrParam object
##' @rdname sh_init
setGeneric("sh_init_script", function(x) standardGeneric("sh_init_script"))

##' @aliases sh_init_script,SwitchrParam
##' @rdname sh_init
setMethod("sh_init_script", "SwitchrParam", function(x) x@shell_init)

##' @export
##' @rdname sh_init
##' @docType methods
##' @param value The new value.
setGeneric("sh_init_script<-", function(x, value) standardGeneric("sh_init_script<-"))
##' @aliases sh_init_script<-,SwitchrParam,ANY
##' @rdname sh_init
setMethod("sh_init_script<-", "SwitchrParam", function(x, value) {
    x@shell_init = value
    x
})


##' Get or set the logging function in an object associated with a SwitchrParam
##' @rdname logfun
##' @param x An object with a SwitchrParam
##' @docType methods
##' @export
setGeneric("logfun", function(x) standardGeneric("logfun"))
##' @aliases logfun,SwitchrParam
##' @rdname logfun
setMethod("logfun", "SwitchrParam", function(x) x@logfun)


##' @export
##' @rdname logfun
##' @param value The new logging function
setGeneric("logfun<-", function(x, value) standardGeneric("logfun<-"))
##' @aliases logfun<-,SwitchrParam
##' @rdname logfun

setMethod("logfun<-", "SwitchrParam", function(x, value) {
    x@logfun = value
    x
    })



##' Add a package to an object associated with a manifest
##' @export
##' @rdname addPkg
##' @param x A manifest or manifest-associate objec tto add the pkg 2
##' @param \dots The information regarding the package to place in the manifest
##' @param rows An already-created data.frame to add to the manifest
##' @param versions A data.frame of package names and versions, if adding to
##' a SessionManifest, ignored otherwise
##' @docType methods
setGeneric("addPkg", function(x, ..., rows = makeManifest(...),
                              versions = data.frame(name = manifest_df(rows)$name,
                                  version = NA_character_,
                                  stringsAsFactors=FALSE))
           standardGeneric("addPkg")
           )
##' @rdname addPkg
##' @aliases addPkg,PkgManifest
setMethod("addPkg", "PkgManifest",
          function(x, ..., rows= makeManifest(...), versions) {
              manifest_df(x) = rbind(manifest_df(x), manifest_df(rows))
              dep_repos = unique(c(dep_repos(x), dep_repos(rows)))
              x
          })
##' @rdname addPkg
##' @aliases addPkg,SessionManifest
setMethod("addPkg", "SessionManifest",
          function(x, ..., rows, versions) {
              manifest(x) = addPkg(manifest(x), ..., rows = rows, versions = NULL)
              if(any(versions$name %in% versions_df(x)$name))
                  stop("Version already set for one or more packages")
              versions_df(x) = rbind(versions_df(x), versions)
              x
          })





##' library_paths
##'
##' Accessor for which directories an SwitchrCtx is associated with.
##' @param seed An SwitchrCtx
##' @export
##' @docType methods
##' @rdname library_paths
setGeneric("library_paths", function(seed) standardGeneric("library_paths"))

##' @rdname library_paths
##' @aliases library_paths,SwitchrCtx
setMethod("library_paths", "SwitchrCtx", function(seed) {
    seed@libpaths
})



##'List the packages installed in a switchr context (library)
##' @docType methods
##' @rdname packages
##' @param seed A switchr context
##' @export
setGeneric("packages", function(seed) standardGeneric("packages"))
##' @rdname packages
##' @aliases packages,SwitchrCtx
setMethod("packages", "SwitchrCtx", function(seed) seed@packages)



setGeneric("update_pkgs_list", function(seed) standardGeneric("update_pkgs_list"))
setMethod("update_pkgs_list", "SwitchrCtx", function(seed){

    if(seed@exclude.site)
        pathsToLook = unique(c(library_paths(seed), .Library))
    else
        pathsToLook = unique(c(library_paths(seed), .Library.site, .Library))

    
    pkgs = installed.packages(pathsToLook,
        noCache=TRUE)[,c("Package", "Version", "LibPath")]
    pkgs = pkgs[!duplicated(pkgs[,"Package"]),]
    pkgs = as.data.frame(pkgs, stringsAsFactors = FALSE)

    seed@packages = pkgs
    seed
})

##' Notrack directory
##' 
##' This function is not intended to be called directly by the user.
##'
##' @param repo The object.
##' @return the path where retrieved package versions should be. If \code{repo}
##' is NULL, a notrack directory is constructed within a temp directory.
##' @export
##' @docType methods
##' @rdname notrack
setGeneric("notrack", function(repo) standardGeneric("notrack"))
##' @rdname notrack
##' @aliases notrack,NULL
setMethod("notrack", "NULL", function(repo) file.path(tempdir(), "notrack"))
