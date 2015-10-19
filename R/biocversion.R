biocrepostmpl = c("http://bioconductor.org/packages/%%%%/bioc" ,
    "http://bioconductor.org/packages/%%%%/data/annotation" ,
    "http://bioconductor.org/packages/%%%%/data/experiment" ,
              "http://bioconductor.org/packages/%%%%/extra" )

dev_vers_aliases = c("dev", "devel", "trunk")

## using closures for state/"activeBinding" style behavior here because
## reading the yaml file may fail on installation
doyamlsetup = function() {
    yamlval = NULL
    fun = function() {
        if(!is.null(yamlval))
            return(yamlval)
        
        con = url("http://bioconductor.org/config.yaml")
        on.exit(close(con))
        yaml = tryCatch(readLines(con), error = function(e) {
                            warning("Unable to access http://bioconductor.org/config.yaml to determine Bioc releases and associated R versions. switchr will attempt this again when the package is loaded. You may want to reinstall with network access, some Bioc-related functionality may not work properly.")
                            NULL
                        })
        yamlval <<- yaml
        yamlval
    }
    fun
}


getBiocYaml = doyamlsetup()

getBiocReposFromRVers = function() {
    myyaml = getBiocYaml()
    biocvers = getBiocvrFromRvr(myyaml)
    gsub("%%%%", biocvers, biocrepostmpl)
}
getBiocvrFromRvr = function(yaml, Rvers, first = TRUE) {
    if(missing(Rvers))
        Rvers = paste(R.version$major, gsub("(.*)\\..*", "\\1", R.version$minor), sep=".")
    ln = grep("^r_ver_for_bioc_ver:", yaml)
    lnends = grep("^[^[:space:]]", yaml)
    lnend = min(lnends[lnends > ln])
    mylines = yaml[seq(ln+1, lnend-1)]
    mylines = cleanem(mylines)
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
