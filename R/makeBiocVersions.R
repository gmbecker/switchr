biocReleaseV = "2.14"
biocDevelV = "3.0"

##lifted from biocLite.R
obiocrepos <- NULL

##' @export
biocReposForVers = function(version) {
    if(is.null(bioc))
        NULL
    else
        gsub("(.*)/[[:digit:]\\.]+/(.*)", sprintf("\\1/%s/\\2", version), bioc)
}



##' @export
BiocVers = function(version = biocReleaseV,
    name = paste("BioC", version, sep="_"),
    repos  = biocReposForVers(version)) {

    if(is.null(repos))
        stop("I don't know where the bioconductor repositories are. Unable to proceed")
    if(numeric_version(version) < "2.9")
        stop("Bioconductor versions earlier than 2.9 did not have a BiocInstaller package. Use switchTo(biocReposForVers(<version>), packages = <packages you want>, name = <name you want>) for this.")
    RepoSubset(repos, pkgs = "BiocInstaller", default_name = name)
}

##' @export
BiocRelease = BiocVers(biocReleaseV)

##' @export
BiocDevel = BiocVers(biocDevelV)
