##' SwitchrParam
##' 
##' A contstructor for a SwitchrParam object representing a number of common
##' parameters understood by the switchr framework
##' 
##' @param logfun The function to be called to write to logs
##' @param shell_init A character containing the location of a shell script to
##' be sourced before any system commands.
##' @param archive_timing The timeout after downloading a package from the CRAN Archive.
##' @param archive_retries Number of times to retry retrieving a package from the CRAN Archive.
##' @param dl_method The download method to use when retrieve package
##' source files. See \code{\link{download.file}} If none is specified,
##' the method defaults to "curl" if the RCurl package is installed and "auto" otherwise.
##' @param shell_timing numeric. The numer of seconds to wait between certain shell commands.
##' Defaults to 1, this should only need to be changed in the case of, e.g., networked drive latency issues.
##' 
##' @return A SwitchrParam object.
##' @author Gabriel Becker
##' @aliases SwitchrParam-class
##' @export
SwitchrParam = function(logfun = function(...) NULL, shell_init= character(),
    archive_timing = 2, archive_retries=2, dl_method, shell_timing = 1) {
    if(missing(dl_method)) {
        dl_method = "curl"
    }
    
    new("SwitchrParam", logfun = logfun, shell_init = shell_init,
        archive_timing=archive_timing, archive_retries = archive_retries,
        dl_method = dl_method, shell_timing=shell_timing)
}
