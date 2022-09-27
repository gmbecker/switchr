
#' Create Manifest from 'checkedout' directory containing many pkg dirs
#'
#' This function is useful when a developer has a 'checkout' directory
#' where the sources for multiple packages live. Particularly, it
#' allows one to work on multiple interlocking packages at the same
#' time and have a manifest which will install them all together
#' automatically when time for testing.
#'
#' @param pdir character(1). Parent directory which contains package source directories.
#' @param recursive logical(1). Should directories within \code{pdir} be searched recursively
#'  to find package source directories. Defaults to \code{FALSE} for efficiency reasons.
#' @param excl_pat character(1) or NULL. A regular expression for directories/packages to
#'  exclude from the manifest.
#' @return A Package manifest with 'local' type entries for each package found within \code{pdir}.
#'
#' @export
#' @examples
#' \dontrun{
#' manifestFromCheckoutDir(".")
#' }
manifestFromCheckoutDir <- function(pdir, recursive = FALSE, excl_pat = NULL) {
    if(recursive) {
        descrfiles = list.files(pdir, pattern = "^DESCRIPTION$", recursive = recursive, all.files = FALSE, full.names = TRUE)
    } else {
        allfils = list.files(pdir, full.names = TRUE)
        descrfiles = file.path(allfils, "DESCRIPTION")
        descrfiles = descrfiles[file.exists(descrfiles)]
    }
    if(length(excl_pat) > 0) {
        excl_pat = sprintf("((%s)|\\.Rcheck)", excl_pat)

        descrfiles = descrfiles[!grepl(excl_pat, descrfiles)]
    }
    pkgdirs = dirname(descrfiles)
    pkgnames = basename(pkgdirs)
    dup <- duplicated(pkgnames)
    if(sum(dup) > 0) {
        warning(sum(dup), " Packages found more than once: ", paste(unique(pkgnames[dup]), collapse = ", "),
                "first instance of each will be used")
        inds <- which(dup)
        pkgnames <- pkgnames[-1*inds]
        pkgdirs <- pkgdirs[-1*inds]
    }
    PkgManifest(name = pkgnames, url = pkgdirs, type = "local")
}
