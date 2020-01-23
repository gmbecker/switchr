if(!exists("globalVariables"))
    globalVariables = function(x) NULL

globalVariables("biocinstallRepos", "switchr", TRUE)
globalVariables("biocinstallname", "switchr", TRUE)


biocinstallname = "BiocInstaller"


biocrepostmpl = c("http://bioconductor.org/packages/%%%%/bioc" ,
    "http://bioconductor.org/packages/%%%%/data/annotation" ,
    "http://bioconductor.org/packages/%%%%/data/experiment" ,
              "http://bioconductor.org/packages/%%%%/extra" )

dev_vers_aliases = c("dev", "devel", "trunk", "master")








graceful_inet_constr = function() {
    on = NA
    function(val) {
        if(!missing(val))
            on <<- val
        on
    }
}
#' @import RCurl
#' @import RJSONIO
NULL


#' Internal internet harness
#'
#' 
#' This function should never be called by code outside of tests/vignettes
#' in this package or packages that depend on it.
#'
#' @param  val logical. NA means no additional handling, TRUE, means careful handling
#' but actually attempt the call, FALSE means force immedate failure without evaling
#' expressions wrapped in inet_handlers() calls
#' @rdname internal
#' @export 
graceful_inet = graceful_inet_constr()

inet_handlers = function(expr) {
    if(isTRUE(graceful_inet()))
        withCallingHandlers(tryCatch(expr,
                                     error = function(e) {
            message("caught error: ", e)
            return(e)
        }),
        warning = function(w) {
            message("caught warning: ", w)
            invokeRestart("muffleWarning")
        })
    else if(is.na(graceful_inet()))
        expr
    else { ## force feailture
        tryCatch(stop("Forced connectivity failure"),
                 error = function(e) {
            message("caught error: ", e)
            return(e)
        })
    }
}



#' @rdname internal
#' @param \dots passed to message or base::warning
#' @export
warning2 = function(...) {
    if(graceful_inet())
        message(...)
    else
        base::warning(...)
}
