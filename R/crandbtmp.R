crandburl = "http://crandb.r-pkg.org/"

##' rVersionManifest
##'
##' Create a Pkg manifest which points to tarballs representing the
##' cohort of packages associated with a particular release of R
##'
##' @param vers The version of R to create a manifest for
##' @param curr_avail The output from available.packages(). Used to identify
##' whether the necessary version is in the CRAN archive or normal repository
##' @return A SessionManifest object
##' @references "Gabor Csardi" (2014). crandb: Query the unofficial CRAN metadata
##'  database. R package version 1.0.0. https://github.com/metacran/crandb
##'
##' Becker G, Barr C, Gentleman R, Lawrence M; Enhancing Reproducibility and Collaboration via Management of R Package Cohorts. Journal of Statistical Software, 81(1). 2017. doi: 10.18637/jss.v082.i01 
##' @author Gabriel Becker
##' @examples
##' \dontrun{
##' man = rVersionManifest("3.1.1")
##' man
##' }
## Eventually replace with crandb but it has lots of deps and seems broken now
##' @export
rVersionManifest = function(vers, curr_avail = available.packages()) {
    
    url = paste("http://crandb.r-pkg.org/-/release/", vers, sep="")
    con = url(url)
    resp = inet_handlers(readLines(con))
    close(con)
    if(is.null(resp) || is(resp, "error")) {
        message("Connectivity problem when accessing crandb url ", url)
        return(SessionManifest(PkgManifest()))
    }
    cont = fromJSON(resp)
    tb_urls = buildTarURLs(cont, curr_avail)
    man = PkgManifest(name = names(cont), url = tb_urls, type = "tarball",
                dep_repos = character())
    vers = gsub(".*_(.*)\\.tar\\.gz", "\\1", tb_urls)
    names(vers) = names(cont)
    SessionManifest(man, versions = vers)
}


##' cranPkgVersManifest
##'
##' Create a Pkg manifest which points to tarballs representing a particular
##' version of a CRAN package and versions of its (recursive) dependencies
##' that were contemporary on the first or last day the specified package
##' version resided on CRAN
##'
##' @param pkg The package on which to base the generated manifest
##' @param vers The version of \code{pkg} to construct the cohort around. Note
##' this must match the the version string exactly, i.e. 1.3.1 and 1.3-1 are
##' *not* equivalent.
##' @param earliest Should the package dependencies be contemporary with the
##' first (TRUE) or last (FALSE) day the specified package version was
##' (the latest version) on CRAN?
##' @param cur_avail The output from available.packages(). Used to identify
##' whether the necessary version is in the CRAN archive or normal repository
##' @param verbose Should debugging information about the recursive traversal of
##' package dependencies be printed (defaults to FALSE).
##' @param suggests Which Suggests'ed packages should be included. Currently
##' supported possibilites are direct, indicating Suggestions of \code{pkg}
##' should be included, and none, indicating that no Suggests'ed packages
##' should be counted.
##' @param delay Number of seconds to delay between successive REST calls
##' to the crandb database. Defaults to 1 second
##' @param erronfail how should connection errors be handled. \code{TRUE} (the default) throws an error, \code{NA} throws a warning, \code{FALSE} emits a message.
##' @return A SessionManifest object
##' @references "Gabor Csardi" (2014). crandb: Query the unofficial CRAN metadata
##'  database. R package version 1.0.0. https://github.com/metacran/crandb
##'
##' Becker G, Barr C, Gentleman R, Lawrence M; Enhancing Reproducibility and Collaboration via Management of R Package Cohorts. Journal of Statistical Software, 81(1). 2017. doi: 10.18637/jss.v082.i01 
##' @note Some packages retain the same version on CRAN for long periods of
##' time. The cohort in the manifest represents a gross proxy for the cohort
##' used in conjunction within an analysis which used a the \code{vers} version
##' of the specified package. In general it will *not* perfectly recreate
##' the set of package versions originally used.
##' @author Gabriel Becker
##' @examples
##' \dontrun{
##' man = cranPkgVersManifest("devtools", "1.6")
##' }
## Eventually replace with crandb but it has lots of deps and seems broken now
##' @export


cranPkgVersManifest = function(pkg, vers, earliest = TRUE,
                               cur_avail = available.packages(),
                               verbose = FALSE,
                               suggests = c("direct", "none"),
                               delay = 1,
                               erronfail = TRUE
    ) {
    suggests = match.arg(suggests)
    
    urlpkg = paste0(crandburl, pkg, "/all")
    con = url(urlpkg)
    resp = inet_handlers(readLines(con))
    close(con)
    if(is.null(resp) || is(resp, "error")) {
        message("Connectivity problem when trying to access crandb url ", urlpkg)
        return(SessionManifest(PkgManifest()))
    }
    cont = as.list(RJSONIO::fromJSON(resp))
    cont2 = cont[["versions"]][[vers]]
    tl = do.call(c, lapply(cont$timeline, as.Date))
    
    vdate = tl[vers]
    if(earliest)
        date = vdate
    else
        date = tl[min(which(tl > vdate))]-1
    
    sugneeded = if(suggests == "direct") cont2$Suggests else NULL
    deps = names(c(cont2$Depends, cont2$Imports, sugneeded))
    cnt = 1L
    versneeded = vers
    names(versneeded) = pkg
    i = 1
    while(i <= length(deps) && cnt < 10L) {
        deps = deps[!deps %in% c("R", basepkgs)]
        tmpkg = deps[i]
        if(verbose)
            message(paste("Resolving dependency", i, "of", length(deps), "-",
                        tmpkg ))
        urlpkg = paste0(crandburl, tmpkg, "/all")
        con = url(urlpkg)
        resp = inet_handlers(readLines(con))
        close(con)
        if(is.null(resp) || is(resp, "error")) {
            cnt = cnt + 1L
            Sys.sleep(2*delay)
            next
        }
        depcont = as.list(RJSONIO::fromJSON(resp))
        if(!identical(names(depcont), c("error", "reason"))) {
            tl = do.call(c, lapply(depcont$timeline, as.Date))
        ## we put the 1 in here for packages whose first release
        ## was later than {date}
            depvers = names(tl)[max(c(1,which(tl <= date)))]
            if(verbose)
                message(paste("  Need version", depvers))
            names(depvers) = tmpkg
            tmpcont = as.list(depcont[["versions"]][[depvers]])
            tmpdeps = names(c(tmpcont$Depends, tmpcont$Imports))
            tmpdeps = unique(tmpdeps[!tmpdeps %in% c("R", basepkgs, deps)])
            deps = c(deps, tmpdeps)
            versneeded = c(versneeded, depvers)
            Sys.sleep(delay)
        } else
            warning(paste("Package", tmpkg, "does not appear to be a CRAN package"))
        
        i = i + 1
        
    }

    if(cnt == 10L) {
        msg = "failed contacting crandb 10 times while attempting to resolve recursive dependencies. Unable to build complete manifest."
        if(graceful_inet())
            message(msg)
        else if(erronfail)
            stop(msg)
        else
            warning(msg)
    }        
    pkgurls = buildTarURLs(versneeded, cur_avail)
    man = PkgManifest(name = names(versneeded), url = pkgurls, type = "tarball",
                dep_repos = character())
    SessionManifest(man, versions = data.frame(name = names(versneeded), version = versneeded,
                                               stringsAsFactors = FALSE))
    
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
