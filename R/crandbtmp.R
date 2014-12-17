##' rVersionManifest
##' Create a Pkg manifest which points to tarballs representing the
##' cohort of packages associated with a particular release of R
##'
##' @param vers The version of R to create a manifest for
##' @param curr_avail The output from available.packages(). Used to identify
##' whether the necessary version is in the CRAN archive or normal repository
##' 
##' @return A PkgManifest object
##' @references "Gabor Csardi" (2014). crandb: Query the unofficial CRAN metadata
##'  database. R package version 1.0.0. https://github.com/metacran/crandb
##'

## Eventually replace with crandb but it has lots of deps and seems broken now
##' @export
rVersionManifest = function(vers, curr_avail = available.packages()) {
    if(!require("RJSONIO") && !exists("fromJSON", mode="function"))
        stop("This function requires the RJSONIO package or another package which provides a 'fromJSON' function")
    
    url = paste("http://crandb.r-pkg.org/-/release/", vers, sep="")
    resp = getURL(url)
    cont = fromJSON(resp)
    tb_urls = buildTarURLs(cont, curr_avail)
    PkgManifest(name = names(cont), url = tb_urls, type = "tarball",
                dep_repos = character())
}

buildTarURLs = function(pkgvers, avail) {
    
    stillthere = which(names(pkgvers) %in% avail[,"Package"])
    currentpkgs = avail[names(pkgvers)[stillthere], "Version"] == pkgvers[stillthere]
    
    
    iscurrent = rep(FALSE, times=length(pkgvers))
    iscurrent[stillthere[currentpkgs]] = TRUE
    
    baseurl = ifelse(iscurrent, "http://cran.rstudio.com/src/contrib",
        paste("http://cran.r-project.org/src/contrib/Archive", names(pkgvers), sep="/")
        )
    tarnames = paste0(names(pkgvers), "_", pkgvers, ".tar.gz")
    cranurls = paste(baseurl, tarnames, sep = "/")
    cranurls
}
