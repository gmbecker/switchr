if(FALSE){
fixNAMESPACE = function(tarball, dir = tempdir()) {
    #only need to do this if we're in R 3.0.0 + 
    if(compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.0.0") < 0)
        return(tarball)

    if(getwd() != dir) {
        oldwd = getwd()
        setwd(dir)
        on.exit(setwd(oldwd))
    }
    
    if(length(tarball) > 1)
        return(sapply(tarball, fixNAMESPACE, dir = dir))
    fils = untar(tarball, compressed= "gzip", list=TRUE)
    untar(tarball, compressed = "gzip", exdir = dir)
    if(any(grepl("NAMESPACE", fils)))
        return(tarball)
    else {
        nsFile = gsub("DESCRIPTION", "NAMESPACE", grep("DESCRIPTION$", fils, value=TRUE))
        writeLines(con = nsFile, 'exportPattern(".*")')
        pkgname = basename(dirname(nsFile))
        system(sprintf("R CMD build --no-build-vignettes --no-resave-data %s", pkgname))
        normalizePath(list.files( pattern=paste(pkgname, "_.*\\.", sep=""), full.names=TRUE))
    }
}
}
##' @importFrom tools write_PACKAGES
tempRepo = function(tarballs, dir = tempdir()) {
 #   tarballs2 = fixNAMESPACE(tarballs, dir = dir)
    tarballs2 = tarballs
    repbase = tempfile(tmpdir = dir, pattern = "repo")

    repdir = contrib.url(repbase, type = "source")
    dir.create(repdir, recursive = TRUE)
    file.copy(tarballs2, repdir)
    write_PACKAGES(repdir)
    repbase
}



