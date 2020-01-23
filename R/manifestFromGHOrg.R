ghcaches = new.env()
getWithCache = function(url, env = ghcaches) {
    cleanurl = gsub("[^[:alnum:]]", "", url)
    if(exists(cleanurl, env))
        return(get(cleanurl, env))
    res = RCurl::getURL(url, .opts = list(useragent="switchr-r-package", ssl.verifypeer=FALSE), header = TRUE)
    assign(cleanurl, res, envir = env)
    res
}

makeSearchURL = function(org, coreurl = "https://api.github.com") {
    paste0(coreurl, "/search/code?per_page=100&q=", RCurl::curlPercentEncode("filename:DESCRIPTION"), "+", RCurl::curlPercentEncode("org:"), org)
}


globalVariables("fromJSON")
getOrgSummary = function(orgname, coreurl = "https://api.github.com") {
                                        #url = paste(coreurl, "orgs", orgname, "repos?per_page=100", sep ="/")
     url = makeSearchURL(orgname, coreurl)
    cat(url, "\n")
    res = getWithCache(url, ghcaches)
    attr(res, "apiurl") = url
    procres = consumeOrgSummary(res)
    sresult = fromJSON(procres$rawcontent)
    ret = sresult$items
    nexturl = procres$nextlink
    while(!is.null(nexturl)) {
        cat(nexturl, "\n")
        res = getWithCache(nexturl, ghcaches)
        procres = consumeOrgSummary(res)
        sresult = fromJSON(procres$rawcontent)
        ret = c(ret, sresult$items)
        nexturl = procres$nextlink
    }
    ret
}

consumeOrgSummary = function(raw_resp) {
    rlines = strsplit(raw_resp, "\\r\\n")[[1]]
    nlines = length(rlines)
    hlines = rlines[-nlines]
    hlines = hlines[-1] # the curl status, not valid dcf
    hcon = textConnection(hlines)
    on.exit(close(hcon))
    hdf = as.data.frame(read.dcf(hcon), stringsAsFactors = FALSE)
    linkinfo = getNextLinkURL(hdf)
    list(rawcontent = rlines[nlines],
         nextlink = linkinfo)
    
}


    ## returns exact api url for next page or NULL if on the last page
getNextLinkURL = function(hdf) {
    if(!("Link" %in% names(hdf)))
        return(NULL)
    linkinfo = hdf[,"Link"]
    linklines  = strsplit(linkinfo, ", ")[[1]]
    linkurls = gsub("<([^>]+)>;.*", "\\1", linklines)
    pgnums = gsub(".*page=([[:digit:]]+).*", "\\1", linklines)
    pgtypes = gsub('.*; rel="([^"]+)"$', "\\1", linklines)
    ind <- which("next" == pgtypes)
    if(length(ind))
        linkurls[ind]
    else
        NULL
}


globalVariables("mclapply")
manifestFromGHOrg = function(org, coreurl = "https://api.github.com", cores = 1L) {
    sresults = getOrgSummary(org, coreurl)
    repourls = vapply(sresults, function(x) x$repository$html_url, "")
    keep = !duplicated(repourls)
    sresults = sresults[keep]
    if(!is.null(tryCatch(parallel::mclapply, error = function(e) NULL)))
        rows = mclapply(sresults,makeGitPkgSource, mc.cores =cores)
    else
        rows = lapply(sresults,makeGitPkgSource)
    do.call(rbind, rows)
}

makeGitPkgSource = function(repoinfo) {
    if(!identical(getwd(), tempdir())) {
        old = setwd(tempdir())
        on.exit(setwd(old))
    }
    
    rurl = repoinfo$repository$html_url
    repname = repoinfo$repository$name
    if(!file.exists(repname))
        res = system_w_init("git", args = c("clone", rurl))
    if(!file.exists(repname))
        stop("failed to clone repo", rurl)
    desc = file.path(repname, repoinfo$path)
    descdf = read.dcf(desc)
    
    ManifestRow(name = descdf[,"Package"],
                type= "git",
                url = rurl,
                subdir = if(repoinfo$path == "DESCRIPTION") "." else dirname(repoinfo$path))

}

