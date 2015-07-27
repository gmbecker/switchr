##' Create a manifest of Bioc SVN locations
##' @param bioc_vers A version number for a bioc release, or \code{"devel"} to
##' for the current devel trunk
##' @param not_in_repo character. A vector of package names which are
##' in SVN but do not appear in the bioconductor repository
##' @param software_only logical. Should only software packages be
##' included in the manifest? Defaults to TRUE
##' @return A PkgManifest which contains SVN locations for all
##' packages found in the specified bioc repositories, as well
##' as those listed in \code{not_in_repo}
##' @details  In combination with the \code{lazyRepo} function, this
##' manifest can be used to work from a local, working checkout of a
##' set of inter-dependent Bioconductor packages.
##' @seealso \code{\link{lazyRepo}}
##' @examples
##' bman = BiocSVNManifest()
##' bman
##' \dontrun{
##' repo = lazyRepo("rtracklayer", bman)
##' }
##' @export
BiocSVNManifest = function(bioc_vers = "devel", not_in_repo = character(), software_only = TRUE) {
    if(tolower(bioc_vers) %in% dev_vers_aliases)
        vernum = develVers
    else
        vernum = bioc_vers
    reps = biocReposFromVers(vernum)
    if (software_only)
        reps = reps[1]
    pkgs = available.packages(contrib.url(reps))[,"Package"]
    pkgs = unique(c(pkgs, not_in_repo))
    urls = makeBiocSVNURL(pkgs, bioc_vers)
    PkgManifest(name = pkgs, url = urls, type = "svn")
    
        
}    



    
