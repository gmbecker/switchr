        



## using closures for state/"activeBinding" style behavior here because
## reading the yaml file may fail on installation
doyamlsetup = function() {
    yamlval = NULL
    stale = TRUE
    initial = TRUE
    fun = function() {
        if(!is.null(yamlval) && !initial && !stale)
            return(yamlval)
        
        con = url("http://bioconductor.org/config.yaml")
        on.exit(close(con))
        yaml = inet_handlers(readLines(con))
        if(is(yaml, "error")) {
            return(NULL)
        } else {
            yamlval <<- yaml
            if(initial) {
                stale <<- TRUE
                initial <<- FALSE
            } else {
                stale <<- FALSE
            }
        }
        yamlval
    }
    fun
}


## can't require yaml since that package is very new, switchr
## can't afford any new (or even newish) dependencies!

## in zzz.R as delayed assign now.
##getBiocYaml = doyamlsetup()

getBiocReposFromRVers = function() {
    myyaml = getBiocYaml()
    biocvers = getBiocvrFromRvr(myyaml)
    reps = gsub("%%%%", biocvers, biocrepostmpl)
    exst = inet_handlers(url.exists(reps[4]))
    if(!is(exst, "error") && !exst) ## extra repo isn't on newer repos
        reps = reps[-4]
    reps
}

getMultilineYamlField = function(yaml = getBiocYaml(), field) {
    pat = paste0("^", field, ":")
    ln = grep(pat, yaml)
    if(length(ln) == 0)
        stop(sprintf("field %s not found in Bioc yaml data", field))

    lnends = grep("^[^[:space:]]", yaml)
    lnend = min(lnends[lnends > ln])
    mylines = yaml[seq(ln+1, lnend-1)]
    mylines = cleanem(mylines)
    mylines
}
    

getBiocvrFromRvr = function(yaml  = getBiocYaml(), Rvers, first = TRUE) {
    if(missing(Rvers))
        Rvers = paste(R.version$major, gsub("(.*)\\..*", "\\1", R.version$minor), sep=".")
    if(is.null(yaml))
        return(NULL)
    
    mylines = getMultilineYamlField(yaml, "r_ver_for_bioc_ver")
    mymatty = do.call(rbind, strsplit(mylines, ":"))
    matches = which(mymatty[,2] == Rvers)
    if(all(is.na(matches))) {
        return(getBiocDevelVr())
    }
    
    if(first)
        row = min(matches)
    else
        row = max(matches)
    biocvers = mymatty[row, 1]
}

getBiocDevelVr = function() {
    yaml = getBiocYaml()
    if(is.null(yaml))
        return(NULL)
    develln = grep("^devel_version:",yaml)
    develvr = gsub('.*:.*"(.*)".*', "\\1", yaml[develln])
    develvr
}

getBiocReleaseVr = function() {
    yaml = getBiocYaml()
    if(is.null(yaml))
        return(NULL)
    develln = grep("^release_version:",yaml)
    develvr = gsub('.*:.*"(.*)".*', "\\1", yaml[develln])
    develvr
}

## alias to avoid immediate refactor
## We want the devel version because we want to hit the
## devel repo, if called for, I think.
highestBiocVers = function() biocReposFromVers(getBiocDevelVr())

## in zzz.R as delayed assign now.
##develVers = getBiocDevelVr()

isCurrentDevelVr = function(vr, yaml) {
    develvr = getBiocDevelVr()
    vr == develvr
}


cleanem = function(lines) {
    lines = gsub("#.*", "", lines)
    lines = gsub('[" \\t]', "", lines)
    lines
}

## in zzz.R as delayed assign now.

## defaultBiocRepos = tryCatch(getBiocReposFromRVers(), error = function(e) {
##                                 warning("Unable to access http://bioconductor.org/config.yaml. This installation won't have a baked-in default set of Bioc Repositories. You may want to try reinstalling. switchr will attempt to determine default Bioc repos when the package is loaded.")
##                                 NULL
##                             })


biocreposfactory = function() {
    bcrepos = NULL
    function() {
        if(!is.null(bcrepos))
            bcrepos
        else if(length(getBiocRepos()))
            bcrepos = getBiocRepos()
        else
            stop("Unable to determine bioc base repos. Please install BiocManager or BiocInstaller, depending on R version.")
    }
}
biocBaseRepos = biocreposfactory()

##highestVs = c(9, 14, 2)
allBiocReleases = function(includeDev = FALSE) {
    yaml = getBiocYaml()
    if(is.null(yaml)) {
        warning2("Unable to access bioconductor yaml")
        return(NULL)
    }
    mylines = getMultilineYamlField(yaml, "release_dates")
    ret = gsub("(.*):.*", "\\1", mylines)
    if(includeDev)
        ret = c(ret, getBiocDevelVr())
    ret
}


biocVersAsGitBr = function(vers) {
    paste0("RELEASE_", gsub(".", "_", vers, fixed=TRUE))
}




decrBiocVersion = function(biocVers) {

    if(biocVers == "1.0") {
        warning("Unable to decrement Bioconductor version below 1.0")
        return(NULL)
    }
    allvers = allBiocReleases(includeDev = TRUE)
    if(is.null(allvers)) {
        warning2("Bioconductor functionality doesn't appear to be working right now. Connectivity problem?")
        return(NULL)
    }
    ind = which(allvers == biocVers)
    if(length(ind) == 0)
        stop(sprintf("invalid bioc version? %s", biocVers))
    else if(ind == 1)
        stop(sprintf("unable to decrement bioc version: %s", biocVers))
    allvers[ind - 1]
}

decrBiocRepo = function(repos, vers = biocVersFromRepo(repos)) {
    if(!is.character(vers))
        vers = as.character(vers)

    pieces = strsplit(repos, vers, fixed=TRUE)
    newvers = decrBiocVersion(vers)
    if(is.null(newvers)) {
        return(NULL)
    }
    sapply(pieces, function(x) paste0(x, collapse = newvers))
}

biocVersFromRepo = function(repos) gsub(".*/([0-9][^/]*)/.*", "\\1", repos[1])

biocReposFromVers = function(vers = develVers) {
    if(beforeBiocInstaller()) {
        if(!exists("biocinstallRepos")) {
            res =try(source("http://bioconductor.org/biocLite.R"))
            if(is(res, "try-error")) {
                warning2("Unable to source biocLite.R. Connectivity problem?")
                return(NULL)
            }
        }
        repos = biocinstallRepos()
        repos = repos[grepl("bioconductor.org", repos)]
    } else {
        
        repos = biocBaseRepos()
        repos = repos[grep("BioC", names(repos))]
    }

    bef= gsub("(.*/)[0-9][^/]*/.*", "\\1", repos)
    af = gsub(".*/[0-9][^/]*(/.*)", "\\1", repos)
    paste0(bef, vers, af)
}    

getRemoteBranches = function(dir = ".") {

    res = system_w_init("git", dir = dir, args = "branch -r", intern = TRUE)
    res = res[!grepl("HEAD ->", res)]
    res = gsub(".*origin/(.*)", "\\1", res)
    res
}    



sneakyreqpkg = function(pkg, quietly = FALSE) {
    req = tryCatch(get("requireNamespace"), error = identity)
    if(is(req, "error"))
        req = get("require")
    
    req(pkg, quietly = quietly)
}

    

getBiocRepos = function() {
    ## this sucks but I can't afford the dependency on 3.5.x+ that comes
    ## with BiocManager :(
    if(sneakyreqpkg("BiocManager", quietly = TRUE)) { 
        bioc = inet_handlers(get("repositories", asNamespace("BiocManager"))())
    } else if(sneakyreqpkg("BiocInstaller", quietly = TRUE)) {
        bcrepofun = get("biocinstallRepos", envir = asNamespace("BiocInstaller")) 
        bioc = bcrepofun()
    } else if(beforeBiocInstaller()) {
        if(!exists("biocinstallRepos"))
            source("http://bioconductor.org/biocLite.R")
        bioc = biocinstallRepos()
    } else {
        if(is.null(defaultBiocRepos)) {
            bioc = tryCatch(getBiocReposFromRVers(), function(e) character())
            if(length(bioc) == 0)
                warning("Unable to determine Bioc repositories. They will not be included in the set of default dependency repos")
        } else
            bioc = defaultBiocRepos
    }
    if(is(bioc, "error")) 
        return(character())

    if (anyNA(bioc))  {
        warning("Attempt to determine default Bioconductor repos returned one or more NAs. These will be omitted from the set of default repositories.")
        bioc = bioc[!is.na(bioc)]
    }
    bioc
}




##' defaultRepos
##'
##' Get default repositories for use as dependency repos and within
##' install_packages
##'
##' @return A character vector of package repository urls
##' @export
##' @importFrom utils chooseCRANmirror
defaultRepos = function() {
    bioc = getBiocRepos()
    optrepos = getOption("repos")
    if(is.na(optrepos["CRAN"]) || optrepos["CRAN"] == "@CRAN@") {
        ## if bioc has a cranmirror (which it should)
        if(any(grepl("cran", bioc, ignore.case=TRUE))) 
            optrepos = optrepos[!grepl("CRAN", names(optrepos))]
        else {

            if(interactive())
                chooseCRANmirror()
            else{
                message("Switchr needs a default CRAN mirror set via R options. Using the cloud mirror. This happens only when no CRAN mirror is selected *and* the BiocInstaller package is not installed.")
                chooseCRANmirror(ind= 1L)
            }
            optrepos = getOption("repos")
        }
    } else if (!is.null(names(bioc))) 
          bioc = bioc[!names(bioc) == "CRAN"]
    
    granrepos = NULL
    if(exists("defaultGRANURL")) 
        granrepos = get("defaultGRANURL")()
    repos = unique(c(granrepos, optrepos, bioc))
    repos
}

