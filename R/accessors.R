
##' @title archive_timing
##'
##' @description Get or set the number of seconds to wait after trying to
##' retrieve a file from the CRAN Archive.
##'
##' This is intended to stop intermittent install failures
##' due to failing to retrieve files that *are* in the
##' archive but are not downloading properly when a larger 
##' number of packages is being retrieved.
##'
##' @param x A SwitchrParam object
##' @return When getting, the number of seconds to wait,
##' when setting, a new, updated SwitchrParam object.
##' @rdname archive_timing
##' @docType methods
##' @export
setGeneric("archive_timing", function(x) standardGeneric("archive_timing"))

##' @rdname archive_timing
##' @aliases archive_timing,SwitchrParam
setMethod("archive_timing", "SwitchrParam", function(x) x@archive_timing)

##' @rdname archive_timing
##' @aliases archive_timing<-
##' @export
setGeneric("archive_timing<-", function(x, value) standardGeneric("archive_timing<-"))

##' @rdname archive_timing
##' @param value The new number of seconds to wait
##' @aliases archive_timing<-,SwitchrParam
setMethod("archive_timing<-", "SwitchrParam", function(x, value) {
                                x@archive_timing = value
                                x
                            })



##' @title Get or set the number of seconds to wait between successive shell commands
##' 
##' @description This is intended to stop intermittent install failures
##' due to network drive latency interacting with git commands
##'
##' @param x A SwitchrParam object
##' @return When getting, the number of seconds to wait,
##' when setting, a new, updated SwitchrParam object.
##' @rdname shell_timing
##' @docType methods
##' @export
setGeneric("shell_timing", function(x) standardGeneric("shell_timing"))

##' @rdname shell_timing
##' @aliases shell_timing,SwitchrParam
setMethod("shell_timing", "SwitchrParam", function(x) x@shell_timing)

##' @rdname shell_timing
##' @aliases shell_timing<-
##' @export
setGeneric("shell_timing<-", function(x, value) standardGeneric("shell_timing<-"))

##' @rdname shell_timing
##' @param value The new number of seconds to wait
##' @aliases shell_timing<-,SwitchrParam
setMethod("shell_timing<-", "SwitchrParam", function(x, value) {
                                x@shell_timing = value
                                x
                            })


##' @title dl_method
##' @description Get or set the download method for retreiving files.
##' @param x A SwitchrParam object
##' @rdname dl_method
##' @docType methods
##' @export

setGeneric("dl_method", function(x) standardGeneric("dl_method"))

##' @rdname dl_method
##' @aliases dl_method,SwitchrParam
setMethod("dl_method", "SwitchrParam", function(x) x@dl_method)

##' @rdname dl_method
##' @aliases dl_method<-
##' @export
setGeneric("dl_method<-", function(x, value) standardGeneric("dl_method<-"))

##' @rdname dl_method
##' @param value The new number of seconds to wait
##' @aliases dl_method<-,SwitchrParam
setMethod("dl_method<-", "SwitchrParam", function(x, value) {
                                x@dl_method = value
                                x
                            })







##' @title archive_retries
##' 
##' @description Get or set the number of times to retry downloading a file from
##' the CRAN archive
##'
##' This is intended to stop intermittent install failures
##' due to failing to retrieve files that *are* in the
##' archive but are not downloading properly when a larger 
##' number of packages is being retrieved.
##'
##' @param x A SwitchrParam object
##' @return When getting, the number of seconds to wait,
##' when setting, a new, updated SwitchrParam object.
##' @rdname archive_retries
##' @docType methods
##' @export
setGeneric("archive_retries", function(x) standardGeneric("archive_retries"))

##' @rdname archive_retries
##' @aliases archive_retries,SwitchrParam
setMethod("archive_retries", "SwitchrParam", function(x) x@archive_retries)

##' @rdname archive_retries
##' @aliases archive_retries<-
##' @export
setGeneric("archive_retries<-",
           function(x, value) standardGeneric("archive_retries<-"))

##' @rdname archive_retries
##' @param value The new number of seconds to wait
##' @aliases archive_retries<-,SwitchrParam
setMethod("archive_retries<-", "SwitchrParam", function(x, value) {
                                x@archive_retries = value
                                x
                            })






##' @title dep_repos
##'
##' @description Get or set repositories to be used to fullfill dependencies beyond packages
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



##' @title Get or set the manifest associated with an object
##' 
##'
##' @description Get or set manifest associated with an object
##'
##' @rdname manifest_methods
##' @param x An object which contains a manifest
##' @docType methods
##' @return A PkgManifest or SessionManifest object
##' @export
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






##' @title manifest_df
##'
##' @description Get or set the package location manifest (data.frame) associated with an
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

##' @title versions_df
##' 
##' @description Get or set the the versions information in a SessionManifest
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











##' @title branch 
##' @description Get or set the branch associated with a Package Source
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




##' @title pkgname
##' @description Get or set the package name associated with a Package Source
##' @export
##' @param x A source
##' @rdname pkgname
##' @docType methods
setGeneric("pkgname", function(x) standardGeneric("pkgname"))
##' @aliases pkgname,PkgSource
##' @rdname pkgname
setMethod("pkgname", "PkgSource", function(x) x@name)
##' @rdname pkgname
##' @param value The new pkgname
setGeneric("pkgname<-", function(x, value) standardGeneric("pkgname<-"))
##' @aliases pkgname<-,PkgSource
##' @rdname pkgname
setMethod("pkgname<-", "PkgSource", function(x, value) {
    x@name = value
    x
    })




##' @title subdir
##' @description accessor for subdirectory. 
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



##' @title location
##' @description Retreive the directory associated with an object
##' @rdname location-methods
##' @return a character containing the associated path
##' @author Gabriel Becker
##' @param repo An object associated with a path
##' @docType methods
##' @export
setGeneric("location", function(repo) standardGeneric("location"))


##' @rdname location-methods
##' @aliases location,PkgSource-method
##' @export
setMethod("location", "PkgSource", function(repo) repo@location)

##' @title shell init
##' @description Set or Retrieve the shell initialization script for an object
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


##' @title logfun
##' @description Get or set the logging function in an object associated with a SwitchrParam
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



## @title Add/replace rows in a data.frame
## 
## Combine two dataframes together with rows in one optionally replacing
## those in the other when they match on a specified index column
##
## @param df data.frame. The "first" or "old" data.frame.
## @param newdf data.frame. The "new" data frame of rows to add to \code{df}
## @param replace logical. Should replacement happen when rows of \code{df} and
## \code{newdf} match based on \code{indexcol}. Defaults to \code{TRUE}. If
## \code{FALSE}, an error is thrown in the matching case.
## @param indexcol character. The name of the column to use for matching.
## Defaults to \code{"name"} for convenience of internal usage.
##
## @return A combined data.frame with only columns found in both data.frames
## and one row per unique value of the specified index column across both
## datasets.
addReplaceDF = function(df, newdf, replace = TRUE, indexcol = "name") {
    df = df[,intersect(names(df), names(newdf))]
    newdf = newdf[,intersect(names(df), names(newdf))]
    oldvec = df[[indexcol]]
    newvec = newdf[[indexcol]]
    dups = oldvec[oldvec %in% newvec]
    if(length(dups)) {
        if(!replace)
            stop("Values in new rows already appear in existing rows, set replace=TRUE to replace them inplace. [",
                 paste(dups, collapse=", "),
                 "]")
        
        dupdf = newdf[newvec %in% dups,]
        newdf = newdf[!newvec %in% dups,]
        df[match(dupdf[[indexcol]], oldvec),] = dupdf
    }
    
    rbind(df, newdf)
}




##' @title addPkg
##' @description Add a package to an object associated with a manifest
##' @export
##' @rdname addPkg
##' @param x A manifest or manifest-associate objec tto add the pkg 2
##' @param \dots The information regarding the package to place in the manifest
##' @param rows An already-created data.frame to add to the manifest
##' @param versions A data.frame of package names and versions, if adding to
##' a SessionManifest, ignored otherwise
##' @param replace logical. If true, the specified package info will replace
##' any already in the manifest in the case of duplicates. Otherwise, an error
##' is thrown.
##' @docType methods
setGeneric("addPkg", function(x, ..., rows = makeManifest(...),
                              versions = data.frame(name = manifest_df(rows)$name,
                                  version = NA_character_,
                                  stringsAsFactors=FALSE),
                              replace = FALSE)
           standardGeneric("addPkg")
           )
##' @rdname addPkg
##' @aliases addPkg,PkgManifest
setMethod("addPkg", "PkgManifest",
          function(x, ..., rows= makeManifest(...), versions, replace) {
              oldman = manifest_df(x)
              newman = manifest_df(rows)
              ## oldman = oldman[,names(newman)]
              ## dups = oldman$name[oldman$name %in% newman$name]
              ## if(length(dups)) {
              ##     if(!replace)
              ##         stop("Attempted to add package(s) already in manifest with replace=FALSE:  ",
              ##              paste(dups, collapse=", "))
              ##     dupman = newman[newman$name %in% dups,]
              ##     newman = newman[!newman$name %in% dups,]
              ##     oldman[match(newman$name, oldman$name),] = newman
              ## }
              ## manifest_df(x) = rbind(oldman, newman)
              manifest_df(x) = addReplaceDF(oldman, newman, replace= replace)
              dep_repos = unique(c(dep_repos(x), dep_repos(rows)))
              x
          })
##' @rdname addPkg
##' @aliases addPkg,SessionManifest
setMethod("addPkg", "SessionManifest",
          function(x, ..., rows, versions, replace) {
              manifest(x) = addPkg(manifest(x), ..., rows = rows, versions = NULL,
                                   replace = replace)
              if(!missing(versions) && length(versions) > 0) {
                  if(is(versions, "character"))
                      versions = data.frame(name = names(versions),
                          version = versions, stringsAsFactors = FALSE)
                  
                  oldv = versions_df(x)
                  ## versions = versions[,names(oldv)]
                  ## versions_df(x) = rbind(oldv, versions)
                  versions_df(x) = addReplaceDF(oldv, versions, replace = replace)
              }
              x
          })

##' @title library_paths
##'
##' @description Accessor for which directories an SwitchrCtx is associated with.
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


##' @title full_libpaths
##'
##' @description Accessor for the full library path associate with a SwitchrCtx, including
##' the R library and (if not excluded) the site library
##'
##' @param seed a SwitchrCtx
##' @export
##' @docType methods
##' @rdname full_libpaths

setGeneric("full_libpaths", function(seed) standardGeneric("full_libpaths"))

##' @rdname full_libpaths
##' @aliases full_libpaths,SwitchrCtx
setMethod("full_libpaths", "SwitchrCtx", function(seed) {
              unique(c(library_paths(seed), if(seed@exclude.site) character() else .Library.site, .Library))
          })



##' @title packages
##' @description List the packages installed in a switchr context (library)
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

              pathsToLook = full_libpaths(seed)

    
    pkgs = installed.packages(pathsToLook,
        noCache=TRUE)[,c("Package", "Version", "LibPath")]
    pkgs = pkgs[!duplicated(pkgs[,"Package"]),]
    pkgs = as.data.frame(pkgs, stringsAsFactors = FALSE)

    seed@packages = pkgs
    seed
})

##' @title Notrack directory
##' 
##' @description This function is not intended to be called directly by the user.
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


##' @description Number of rows
##'
##' @title Number of rows
##'
##' @param x A tabular data structure.
##' @return The number of rows in the structure
##' @docType methods
##' @export
##' @rdname nrow
setGeneric("nrow", nrow)
##' @rdname nrow
##' @aliases nrow,PkgManifest
setMethod("nrow", "PkgManifest",
          function(x) base::nrow(manifest_df(x)))
##' @rdname nrow
##' @aliases nrow,SessionManifest
setMethod("nrow", "SessionManifest",
          function(x) base::nrow(manifest_df(x)))
