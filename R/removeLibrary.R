##' removeLib
##' 
##' Remove a switchr library and update the manifest of existing libraries
##' @param name The name of the switchr library to remove
##' @param repos the url used to seed the library
##' @param compEnv a SwitchrCtx representing the library to remove
##' @param fromStack Whether the library should be removed if it
##' currently appears in the Context stack Defaults to false.
##'
##' @note Only one of \code{name}, \code{repos} and \code{compEnv} should be
##' specified. An error will be thrown otherwise.
##' @return NULL, called for its side-effect of removing/destroying a switchr
##' library
##' @examples
##' \dontrun{
##' removeLib("mylibrary")
##' }
##' @export
removeLib = function(name = NULL, repos = NULL, compEnv = NULL, fromStack = FALSE) {
    if(is.null(name) &&
       is.null(repos) &&
       is.null(compEnv))
        stop("One of name, repos, or compEnv must be specified")

    man = switchrManifest()
    if(is.null(compEnv)) {
        compEnv <-  findCompEnv(name = name, url = repos, allMatches=TRUE)
        if(!length(compEnv))
            stop("No switchr library matching that name",
                 "and/or set of repositories found")
        else if(length(compEnv) > 1)
            stop("More than one switchr library matched.",
                 "Aborting removal.")
        else
            compEnv = compEnv[[1]]
    }

    if(identical(compEnv, currentCompEnv()))
        stop("Cannot remove a library currently in use")

    name = compEnv@name
    if(name == "original")
        stop("Cannot delete default ('original') library")
    else if (name %in% names(Renvs$stack)) {
        if(!fromStack) {
            warning("Not removing library", name,
                    "which appears  in the stack. Use fromStack=TRUE",
                    "to force removal")
            return(NULL)
        } else {
            message(paste("Removing computing environment",
                          name, "from the stack."))
            i = which(names(Renvs$stack) == name)
            Renvs$stack = Renvs$stack[-i]
        }
    }
    
    res = unlink(compEnv@libpaths[1], recursive = TRUE, force=TRUE)
    if(res)
        stop("unlink failed")
    updateManifest()
}
 
