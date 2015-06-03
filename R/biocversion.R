biocrepostmpl = c("http://bioconductor.org/packages/%%%%/bioc" ,
    "http://bioconductor.org/packages/%%%%/data/annotation" ,
    "http://bioconductor.org/packages/%%%%/data/experiment" ,
              "http://bioconductor.org/packages/%%%%/extra" )

getBiocReposFromRVers = function() {
    con = url("http://bioconductor.org/config.yaml")
    myyaml = readLines(con)
    close(con)
    biocvers = getBiocvrFromRvr(myyaml)
    gsub("%%%%", biocvers, biocrepostmpl)
}
getBiocvrFromRvr = function(yaml, Rvers) {
    if(missing(Rvers))
        Rvers = paste(R.version$major, gsub("(.*)\\..*", "\\1", R.version$minor), sep=".")
    ln = grep("^r_ver_for_bioc_ver:", yaml)
    lnends = grep("^[^[:space:]]", yaml)
    lnend = min(lnends[lnends > ln])
    mylines = yaml[seq(ln+1, lnend-1)]
    mylines = cleanem(mylines)
    mymatty = do.call(rbind, strsplit(mylines, ":"))
    row = which(mymatty[,2] == Rvers)[1] #first one, do we want first or last???
    biocvers = mymatty[row, 1]
}

isCurrentDevelVr = function(vr, yaml) {
    develln = grep("^devel_version:",yaml)
    develvr = gsub('.*:.*"(.*)".*', "\\1", yaml[develln])
    vr == develvr
}


cleanem = function(lines) {
    lines = gsub("#.*", "", lines)
    lines = gsub('[" \\t]', "", lines)
    lines
}
    
defaultBiocRepos = getBiocReposFromRVers()
