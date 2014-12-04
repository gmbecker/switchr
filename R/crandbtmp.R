##' @importFrom RCurl getURL
##'
##'

## Eventually replace with crandb but it has lots of deps and seems broken now

rVersionManifest = function(vers, curr_avail = available.packages()) {
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
