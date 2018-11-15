if(exists("globalVariables")) {
    globalVariables("biocinstallRepos", "switchr", TRUE)
    globalVariables("biocinstallname", "switchr", TRUE)
}

biocinstallname = "BiocInstaller"


biocrepostmpl = c("http://bioconductor.org/packages/%%%%/bioc" ,
    "http://bioconductor.org/packages/%%%%/data/annotation" ,
    "http://bioconductor.org/packages/%%%%/data/experiment" ,
              "http://bioconductor.org/packages/%%%%/extra" )

dev_vers_aliases = c("dev", "devel", "trunk", "master")

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
        yaml = try(readLines(con))
        if(is(yaml, "try-error")) {
            warning("Unable to read http://bioconductor.org/config.yaml. Bioconductor version information may be unavailable or out-of-date")
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

getBiocYaml = doyamlsetup()

getBiocReposFromRVers = function() {
    myyaml = getBiocYaml()
    biocvers = getBiocvrFromRvr(myyaml)
    reps = gsub("%%%%", biocvers, biocrepostmpl)
    if(!url.exists(reps[4])) ## extra repo isn't on newer repos
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
    ## ln = grep("^r_ver_for_bioc_ver:", yaml)
    ## lnends = grep("^[^[:space:]]", yaml)
    ## lnend = min(lnends[lnends > ln])
    ## mylines = yaml[seq(ln+1, lnend-1)]
    ## mylines = cleanem(mylines)
    mylines = getMultilineYamlField(yaml, "r_ver_for_bioc_ver")
    mymatty = do.call(rbind, strsplit(mylines, ":"))
    matches = which(mymatty[,2] == Rvers)
    if(first)
        row = min(matches)
    else
        row = max(matches)
    biocvers = mymatty[row, 1]
}

getBiocDevelVr = function() {
    yaml = getBiocYaml()
    develln = grep("^devel_version:",yaml)
    develvr = gsub('.*:.*"(.*)".*', "\\1", yaml[develln])
    develvr
}

getBiocReleaseVr = function() {
    yaml = getBiocYaml()
    develln = grep("^release_version:",yaml)
    develvr = gsub('.*:.*"(.*)".*', "\\1", yaml[develln])
    develvr
}

## alias to avoid immediate refactor
## We want the devel version because we want to hit the
## devel repo, if called for, I think.
highestBiocVers = function() biocReposFromVers(getBiocDevelVr())


develVers = getBiocDevelVr()

isCurrentDevelVr = function(vr, yaml) {
    develvr = getBiocDevelVr()
    vr == develvr
}


cleanem = function(lines) {
    lines = gsub("#.*", "", lines)
    lines = gsub('[" \\t]', "", lines)
    lines
}
    
defaultBiocRepos = tryCatch(getBiocReposFromRVers(), error = function(e) {
                                warning("Unable to access http://bioconductor.org/config.yaml. This installation won't have a baked-in default set of Bioc Repositories. You may want to try reinstalling. switchr will attempt to determine default Bioc repos when the package is loaded.")
                                NULL
                            })


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
    mylines = getMultilineYamlField(yaml, "release_dates")
    ret = gsub("(.*):.*", "\\1", mylines)
    ## ret = names(yaml.load(paste(getBiocYaml(),
    ##                             collapse = "\n")
    ##                       )$release_dates)
    if(includeDev)
        ret = c(ret, getBiocDevelVr())
    ret
}


biocVersAsGitBr = function(vers) {
    paste0("RELEASE_", gsub(".", "_", vers, fixed=TRUE))
}




decrBiocVersion = function(biocVers) {

    if(biocVers == "1.0")
        return(NULL)
    
    allvers = allBiocReleases(includeDev = TRUE)
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
        warning("Cannot decrement bioc repo version below 1.0")
        return(NULL)
    }
    sapply(pieces, function(x) paste0(x, collapse = newvers))
}

biocVersFromRepo = function(repos) gsub(".*/([0-9][^/]*)/.*", "\\1", repos[1])

biocReposFromVers = function(vers = develVers) {
    if(beforeBiocInstaller()) {
        if(!exists("biocinstallRepos"))
            source("http://bioconductor.org/biocLite.R")
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

    
