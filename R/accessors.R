
##' dep_repos
##'
##' Get repositories to be used to fullfill dependencies beyond packages within the manifest
##' @return Character vector with existing repository urls
##' 
##' @rdname dep_repos
##' @export
setGeneric("dep_repos", function(x) standardGeneric("dep_repos"))


setMethod("dep_repos", "PkgManifest", function(x) x@dependency_repos)


setMethod("dep_repos", "SessionManifest", function(x) manifest(x)@dependency_repos)




##' manifest
##' Extract manifest data.frame associated with the manifest
##' @aliases manifest,PkgManifest-method
##' @export
setGeneric("manifest", function(x) standardGeneric("manifest"))
setMethod("manifest", "PkgManifest", function(x) x@manifest)

setGeneric("manifest<-", function(x, value) standardGeneric("manifest<-"))


setMethod("manifest", "SessionManifest",
          function(x) x@pkg_manifest)




setMethod("manifest<-", "SessionManifest",
          function(x, value ) {
              x@pkg_manifest = value
              x
          })




##' @export
setGeneric("versions", function(x) standardGeneric("versions"))
setMethod("versions", "SessionManifest",
          function(x) x@pkg_versions)

##' @export
setGeneric("versions<-", function(x, value) standardGeneric("versions<-"))
setMethod("versions<-", "SessionManifest",
          function(x, value){
              x@pkg_versions = value
              x
              })

setMethod("manifest", "SessionManifest", function(x) x@pkg_manifest)

##' @export
setGeneric("manifest_df", function(x, ...) standardGeneric("manifest_df"))

## only get manifest rows for pkgs in the 'session' by default
## override with session_only=FALSE if desired
setMethod("manifest_df", "SessionManifest",
          function(x, session_only = TRUE, ...) {
              ## all pkgs in the manifest
              mdf = manifest_df(manifest(x))
              ## restrict to pkgs in the 'session' if desired
              if(session_only)
                  mdf = mdf[mdf$name %in% versions_df(x)$name,]
              mdf
          })
          



setMethod("manifest_df", "PkgManifest", function(x) x@manifest)


##' @export
setGeneric("manifest_df<-", function(x, value) standardGeneric("manifest_df<-"))



setMethod("manifest_df<-", "SessionManifest", function(x, value) {
    manifest_df(manifest(x)) = value
    x
    })


setMethod("manifest_df<-", "PkgManifest", function(x, value) {
    x@manifest = value
    x
    })


##' @export
setGeneric("versions_df", function(x) standardGeneric("versions_df"))


setMethod("versions_df", "SessionManifest",
          function(x) x@pkg_versions)



##' @export
setGeneric("versions_df<-", function(x, value) standardGeneric("versions_df<-"))


setMethod("versions_df<-", "SessionManifest", function(x, value) {
    x@pkg_versions = value
    x
    })












##' @export
setGeneric("branch", function(x) standardGeneric("branch"))
setMethod("branch", "PkgSource", function(x) x@branch)

setGeneric("branch<-", function(x, value) standardGeneric("branch<-"))
setMethod("branch<-", "PkgSource", function(x, value) {
    x@branch = value
    x
    })


##' @export
setGeneric("subdir", function(x) standardGeneric("subdir"))
setMethod("subdir", "PkgSource", function(x) x@subdir)

##' @export
setGeneric("subdir<-", function(x, value) standardGeneric("subdir<-"))
setMethod("subdir<-", "PkgSource", function(x, value) {
    x@subdir = value
    x
    })



##' location generic
##' Retreive the directory associated with an object
##' @rdname location-methods
##' @return a character containing the associated path
##' @author Gabriel Becker
##' @param repo a GRANRepository object
##' @docType methods
##' @export
setGeneric("location", function(repo) standardGeneric("location"))


##' Retreive the local directory associated with a PkgSource object for a package in a GRAN manifest
##' @rdname location-methods
##' @aliases location,PkgSource-method
##' @export
setMethod("location", "PkgSource", function(repo) repo@location)

##' @export
setGeneric("sh_init_script", function(x) standardGeneric("sh_init_script"))

setMethod("sh_init_script", "SwitchrParam", function(x) x@shell_init)

##' @export
setGeneric("sh_init_script<-", function(x, value) standardGeneric("sh_init_script<-"))
setMethod("sh_init_script<-", "SwitchrParam", function(x, value) {
    x@shell_init = value
    x
})

##' @export
setGeneric("logfun", function(x) standardGeneric("logfun"))
setMethod("logfun", "SwitchrParam", function(x) x@logfun)

##' @export
setGeneric("addPkg", function(x, ..., rows = Manifest(...),
                              versions = data.frame(name = character(),
                                  stringsAsFactors=FALSE))
           standardGeneric("addPkg")
           )

setMethod("addPkg", "PkgManifest",
          function(x, ..., rows, versions) {
              manifest_df(x) = rbind(manifest_df(x), manifest_df(rows))
              dep_repos = unique(c(dep_repos(x), dep_repos(rows)))
              x
          })

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
setGeneric("library_paths", function(seed) standardGeneric("library_paths"))

setMethod("library_paths", "SwitchrCtx", function(seed) {
    seed@libpaths
})


##' @export
setGeneric("packages", function(seed) standardGeneric("packages"))
setMethod("packages", "SwitchrCtx", function(seed) seed@packages)


##' @export
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
