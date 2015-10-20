##' Create a manifest of Bioc SVN locations
##' @param bioc_vers A version number for a bioc release, or \code{"devel"} to
##' for the current devel trunk
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
BiocSVNManifest = function(bioc_vers = "devel", software_only = TRUE) {

    rpackBase = makeBiocSVNURL("", bioc_vers)
    rpkgs = gsub( "/$", "",
                system2("svn", args = c("ls", rpackBase,
                                        "--username=readonly --password=readonly"),
                         stdout = TRUE, stderr = TRUE))
    rpackurls = paste0(rpackBase, rpkgs)
    
    if(software_only) {
        exppkgs = character()
        expurls = character()
    } else {
        experimentBase = "https://hedgehog.fhcrc.org/bioc-data/branches/RELEASE_3_2/experiment/pkgs"
        exppkgs = gsub( "/$", "",
                    system2("svn", args = c("ls", experimentBase,
                                            "--username=readonly --password=readonly"),
                            stdout = TRUE, stderr = TRUE))
        expurls = paste0(experimentBase, exppkgs)
    }
    pkgs = c(rpkgs, exppkgs)
    urls = c(rpackurls, expurls)
    PkgManifest(name = pkgs, url = urls, type = "svn")
    
        
}    



    
