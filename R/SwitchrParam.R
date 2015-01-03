##' SwitchrParam
##' A contstructor for a SwitchrParam object representing a number of common
##' parameters understood by the switchr framework
##' @param logfun The function to be called to write to logs
##' @param shell_init A character containing the location of a shell script to
##' be sourced before any system commands.
##'
##' @return A SwitchrParam object.
##' @author Gabriel Becker
##' @export
SwitchrParam = function(logfun = function(...) NULL, shell_init= character())
    new("SwitchrParam", logfun = logfun, shell_init = shell_init)
