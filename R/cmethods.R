##' @title cmethods
##' @description Combine 2 or more manifests of the same type (PkgManifest or SessionManifest)
##' @rdname cmethods
##' @docType methods
##' @param x An object (indicates the type of all objects to be combined)
##' @param \dots more objects
##' @param recursive Unused
##' @export
setMethod("c", "SessionManifest",
          function(x, ..., recursive = FALSE) {
              ## lifted from IRanges c method
              if (missing(x)) {
                  args <- unname(list(...))
                  x <- args[[1L]]
              }
              else {
                  args <- unname(list(x, ...))
              }
              if (length(args) == 1L) 
                  return(x)
              arg_is_null <- sapply(args, is.null)
              if (any(arg_is_null)) 
                  args[arg_is_null] <- NULL
              if (!all(sapply(args, is, class(x)))) 
                  stop("all arguments in '...' must be ", class(x), 
                       " objects (or NULLs)")
              ## end lifted code

              x = args[[1L]]
              args = args[-1]

              for(y in args) {
                  
                  versions_df(x) = rbind.data.frame(versions_df(x), versions_df(y))
                  
                  manifest(x) = c(manifest(x), manifest(y))
              }
              x
          })

##' @rdname cmethods
##' @aliases c,PkgManifest-method
##' @export
setMethod("c", "PkgManifest",
          function(x,..., recursive = FALSE) {

                            ## lifted from IRanges c method
              if (missing(x)) {
                  args <- unname(list(...))
                  x <- args[[1L]]
              }
              else {
                  args <- unname(list(x, ...))
              }
              if (length(args) == 1L) 
                  return(x)
              arg_is_null <- sapply(args, is.null)
              if (any(arg_is_null)) 
                  args[arg_is_null] <- NULL
              if (!all(sapply(args, is, class(x)))) 
                  stop("all arguments in '...' must be ", class(x), 
                       " objects (or NULLs)")
              ## end lifted code

              
              dep_repos(x) = unique(do.call(c, lapply(args, dep_repos)))
              manifest_df(x) = do.call(rbind, lapply(args, manifest_df))
              x
          })
              
                  
              
  
