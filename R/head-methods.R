##' Head and tail operations on manifests
##' @param x A manifest object
##' @param n The number of packages to keep
##' @param ... unused
##' @return An object of the same type as \code{x} containing \code{n} packages
##' @details In the case of a \code{PkgManifest}, the first or last \code{n}
##' packages are retained in the manifest, while all others are removed.
##'
##' In the case of a \code{SessionManifest}, \code{n} specified versions
##' are retained, while the underlying \code{PkgManifest} is unchanged.
##' @rdname headtail
##' @docType methods
##' @export
setGeneric("head", head)
##' @rdname headtail
##' @aliases head,SessionManifest
##' @export
setMethod(head, "SessionManifest", function(x, n = 5, ...) {
              obj = x
              versions_df(obj) = head(versions_df(obj), n = n, ...)
              obj
          })
##' @rdname headtail
##' @aliases head,PkgManifest
##' @export
setMethod(head, "PkgManifest", function(x, n = 5, ...) {
              obj = x
              manifest_df(obj) = head(manifest_df(obj), n = n, ...)
              obj
          })

##' @rdname headtail
##' @aliases tail
##' @export
setGeneric("tail", tail)
##' @rdname headtail
##' @aliases tail,SessionManifest
##' @export
setMethod(tail, "SessionManifest", function(x, n = 5, ...) {
              obj = x
              versions_df(obj) = tail(versions_df(obj), n = n, ...)
              obj
          })
##' @rdname headtail
##' @aliases tail,PkgManifest
##' @export
setMethod(tail, "PkgManifest", function(x, n = 5, ...) {
              obj = x
              manifest_df(obj) = tail(manifest_df(obj), n = n, ...)
              obj
          })
