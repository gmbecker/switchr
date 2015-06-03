
##' Load a GRAN repo package
##'
##' @param nm The name of the repository for which to load the package. Defaults
##' \code{"current"}
##' @return NULL. Called for the side-effect of loading the specified package
##' @details This function is a convenience to load the package GRAN<nm>, which
##' will provide the contained GRAN repository as default repository within
##' the switchr framework.
##' @export
loadGRAN = function(nm = "current") {
    code = sprintf("library(%s)", paste0("GRAN", nm))
    eval(parse(text = code), envir = .GlobalEnv)
    NULL
}
