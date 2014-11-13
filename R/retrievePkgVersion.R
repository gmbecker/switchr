
##' locatePkgVersion
##'
##' Locate and download/build the exact version of a single package.
##'
##' @param name package name
##' @param version package version string
##' @param repo (optional) GRANRepository object to search
##' @param dir directory to download package into
##' @return The full path to the downloaded file , or NULL if unable to
##' locate the package
##' @author Gabriel Becker
##' @export
locatePkgVersion = function(name, version, manifest, param = SwitchrParam(),
    dir = if(is.null(repo)) tempdir() else notrack(repo), repo = NULL) {
    
    ##There are %three places we might find what we need in increasing order of computational cost:
    ##1. Already in the parent repository (including the associated notrack directory)
    ##2. In the cran archives
    ##3. By wading backwards in an SCM repository (SVN/git)



    ## round 0: did we already find this package version?
    if(file.exists(dir)) {
        lf = list.files(dir, full.names=TRUE)
        fname = grep(paste(name, "_", version, "\\.(tar\\.gz|tgz|zip)", sep=""),
            lf, value = TRUE)
        if(length(fname))
            return(fname)
    }
    
    ##round 1: check the repo
    if(!is.null(repo)) { 
        fname = findPkgVersionInRepo(name, version, param = param)
        if(length(fname) && file.exists(fname))
            return(fname)
    }

    ## round 2: check the manifest
    fname = findPkgVersionInManifest(name, version, manifest,
        dir = dir, param = param)
    if(length(fname) && file.exists(fname))
        return(fname)
    
    ##round 3a: Look in CRAN repo and  archive
    fname = findPkgVersionInCRAN(name, version, param = param, dir = dir)
    if(length(fname) && file.exists(fname))
        return(fname)

    ##round 3b: Look in bioc repo and SVN
    fname = findPkgVersionInBioc(name, version, param = param, dir = dir)
    if(length(fname) && file.exists(fname))
        return(fname)
    
    
    warning(sprintf("Unable to locate version %s of package %s", version, name))
    NULL

}

if(FALSE) {
##TODO: make this able to see into sibling GRAN repositories. Right now it
## will only look in repo itself.
findPkgVersionInRepo = function(name, version, repo)
{
    repdir = repobase(repo)

    tarname = paste0( name, "_", version, ".tar.gz")
    fname = c(list.files(destination(repo), pattern = tarname,
        full.names = TRUE, recursive = TRUE),
        list.files(notrack(repo), pattern = tarname, full.names=TRUE,
                   recursive = TRUE)
        )
    if(!length(fname))
        fname = NULL
    else
        fname = normalizePath2(fname[1])
    fname
}

}

findPkgVersionInCRAN = function(name, version, param = SwitchrParam(), dir)
{
    destpath = dir
    pkgs = as.data.frame(available.packages( fields = c("Package", "Version")))
    if(nrow(pkgs) && name %in% pkgs$Package)
    {
        pkg = pkgs[pkgs$Package == name,]
        if(pkg$Version == version){
           return(download.packages(name, destdir = destpath, )[1,2])
        }
    }
        

    tarname = paste0(name, "_", version, ".tar.gz")
    
    cranurl = paste("http://cran.r-project.org/src/contrib/Archive", name, tarname, sep = "/")

  
    if(!file.exists(destpath))
        dir.create(destpath, recursive = TRUE)
    destfile = file.path(destpath, tarname)
    res = tryCatch(download.file(cranurl, destfile), error = function(e) e)
    if(is(res, "error") || res > 0)
        destfile = NULL

    destfile
}


## git other than github not currently supported.
## Github allows us to checkout via svn

findPkgVersionInManifest = function(name, version, manifest, dir,
    param = SwitchrParam() ) {
    if(!name %in% manifest_df(manifest)$name)
        return(NULL)

    ## XXX this is a hack that should be happening elsewhere!!!
    manrow = manifest_df(manifest)[manifest_df(manifest)$name == name,]

    if(manrow$type =="github" ||
       manrow$type == "git" && grepl("github", manrow$url)) {
        manrow$type = "svn"
        manrow$url = gsub("git://", "http://", manrow$url)
        manrow$url = gsub("\\.git$", "", manrow$url)
        if(manrow$branch == "master")
            manrow$branch = "trunk"
    }

    src = makeSource(name = name, url = manrow$url, type = manrow$type,
        branch = manrow$branch, subdir = manrow$subdir, user = "", password="")
    pkgdir = makePkgDir(name, source = source, path = dir, param = param)



    ## XXX branches aren't handled here!!!!
    findSVNRev(name = name, version = version,
               svn_repo = makeSVNURL(src),   destpath, param = param)
}

setGeneric("makeSVNURL", function(src) standardGeneric("makeSVNURL"))

setMethod("makeSVNURL", "SVNSource",
          function(src) {
              if(branch(src) == "trunk")
                  br = "trunk"
              else
                  br = paste("branches", branch(src), sep = "/")
              paste(location(src), br, subdir(src), sep = "/")
          })
                  
setMethod("makeSVNURL", "GitSource",
          function(src) {
              src = as(src, "SVNSource")
              makeSVNURL(src)
              
          })





    
##' @importFrom BiocInstaller biocinstallRepos biocVersion
findPkgVersionInBioc = function(name, version, param = SwitchrParam(), dir)
{
    
    ret = .biocTryToDL(name, version, dir = dir)
    if(!is.null(ret$file))
        return(ret$file)
    else {
        br = ret$biocVers
        ##XXX need to figure out how to deal with bioc branches.
        ## they aren't at the package level so this is difficult

        if(!file.exists(dir))
            dir.create(dir, recursive=TRUE)
        oldwd = getwd()
        setwd(dir)
        on.exit(setwd(oldwd))
        
        commit = findBiocSVNRev(name, version, destpath = dir,
            param = param, ret$biocVers)
        if(is.null(commit))
            return(NULL)
        pkgdir = file.path(dir, name)
        system_w_init(paste("R CMD build",
                            "--no-build-vignettes",
                            "--no-resave-data",
                            "--no-manual",
                            pkgdir), param = param)
        ret = normalizePath2(list.files(pattern  = paste0(name, "_", version, ".tar.gz"), full.names=TRUE))
        setwd(pkgdir)
        system_w_init("svn up", param = param) #this gets us back to the trunk

    }
    ret
}


## tries to download the file. Returns list with two elements (file:dl'ed file or NULL and versionToSearch:bioc version)
.biocTryToDL = function(name, version, param, dir, verbose = FALSE) {
    
    destpath = dir
    ##    urls = contrib.url(biocinstallRepos())
    ##    urls = urls[-length(urls)]
    urls = contrib.url(highestBiocVers())
#    biocVers = as.character(biocVersion())
    biocVers = biocVersFromRepo(urls)
    
    pkgAvail = TRUE
    everAvail = FALSE
    ret = NULL
    while(!is.null(urls) && pkgAvail && is.null(ret)) {
        if(verbose)
            message(sprintf("Searching Bioc repository for release %s", biocVersFromRepo(urls)))
        
        pkgs = as.data.frame(available.packages(urls, fields = c("Package", "Version")), stringsAsFactors=FALSE)
        pkg = pkgs[pkgs$Package == name,]       
        
        pkgAvail = nrow(pkg) > 0
        
        if(pkgAvail) {
            everAvail = TRUE
            versAvail = pkg[,"Version"]       
            if(compareVersion(versAvail, version) < 0) {
                if(verbose)
                    message(sprintf("Bioc repo for release %s has package version %s, earlier than desired version %s", biocVersFromRepo(urls), versAvail, version))
                pkgAvail = FALSE
            } else if (compareVersion(versAvail, version) == 0) {
                                        #                ret = download.packages(name, destdir = destpath, repos = urls)[1,2]
                filname = paste0(name, "_", version, .getExt(pkg[1,"Repository"]))
                dstfile = file.path(dir, filname)
                ret = tryCatch(download.file(paste(pkg[1,"Repository"], filname, sep="/"), destfile=dstfile), error=function(x) x)
                if(!is(ret, "error") && ret == 0) {
                    ret = dstfile
                    if(verbose)
                        message(sprintf("FOUND package %s version %s in Bioc release repository %s", name, version, biocVersFromRepo(urls)))
                } else {
                    stop(paste("Package version found but error occured when trying to retrieve it:", ret))
                }
            } else {
                biocVers = biocVersFromRepo(urls)
                urls = decrBiocRepo(urls)
            }
            
        }
        
    }
    list(file = ret, biocVers = biocVers)
    
}

.getExt = function(repourl) {
    typ = gsub(".*/(.*)/contrib", "\\1", repourl)
    switch(typ,
           src = ".tar.gz",
           win = ".zip",
           mac = ".tgz")
}


findBiocSVNRev = function(name, version, destpath, param, biocVers="trunk")
{
    if(biocVers != biocVersFromRepo(highestBiocVers()) && biocVers != "trunk") {
##        addl_dir = paste0("BioC_", biocVers)
        biocVers = paste("branches/RELEASE", gsub(".", "_", biocVers, fixed=TRUE), sep="_")
    } else {
        biocVers = "trunk"
        addl_dir = ""
    }

    pkgdir = file.path(destpath, name) ##file.path(destpath, addl_dir)
    repoloc = paste0("https://hedgehog.fhcrc.org/bioconductor/", biocVers, "/madman/Rpacks/", name)
    if(!file.exists(pkgdir)) {
        src = makeSource(name = name, url = repoloc, type = "svn", user = "readonly", password="readonly")
        ##ret = makePkgSourceDir(name = name, source = src, path = file.path(destpath, addl_dir), repo = repo)
        ret = makePkgDir(name = name, source = src, path = destpath, latest_only = FALSE, param = param)
        
        if(!ret)
            return(NULL)
    }         


    

    
    findSVNRev(name, version, svn_repo = repoloc, destpath, param)
    
        
}

findSVNRev = function(name, version, svn_repo, destpath, param) {

    oldwd = getwd()
    ##setwd(file.path(destpath,  name))
    setwd(destpath)
    on.exit(setwd(oldwd))
    system_w_init(paste("svn switch --ignore-ancestry", svn_repo), param = param)

    
    
    
    cmd0 = "svn log -r 1:HEAD --limit 1 DESCRIPTION"
    revs = system_w_init(cmd0, intern=TRUE, param = param)
    minrev = as.numeric(gsub("r([[:digit:]]*).*", "\\1", revs[2])) #first line is -------------------
     cmd1 = "svn log -r HEAD:1 --limit 1 DESCRIPTION"
    revs2 = system_w_init(cmd1, intern=TRUE, param = param)
    maxrev = as.numeric(gsub("r([[:digit:]]*).*", "\\1", revs2[2]))
    
    currev = floor((maxrev+minrev)/2)
    
    commit = binRevSearch(version, currev = currev, maxrev = maxrev, minrev = minrev, found = FALSE)
    cmd2 = paste("svn switch --ignore-ancestry -r", commit, svn_repo)#repoloc)
    system_w_init(cmd2, param = param)
    return(commit)
    }

    
binRevSearch = function(version, currev, maxrev, minrev, param, found = FALSE)
{
    cmd = paste("svn diff --revision", paste(currev, maxrev, sep=":"), "DESCRIPTION")
                                        #  revs = tryCatch(system_w_init(cmd, intern=TRUE, repo=repo), error=function(x) x)
    revs = tryCatch(system(cmd, intern=TRUE), error=function(x) x)
    
    revVersions = grep(".*[Vv]ersion:", revs, value=TRUE)
    if(is(revs, "error"))
        return(NULL)

    if(!length(revVersions)) {
        if(minrev == maxrev - 1) {
            if(found)
                return(minrev)
            else
                return(NULL)
        } else {
            return(binRevSearch(version, floor((minrev + currev )/2),  currev, minrev, param, found = found))
        }
    }
        
    revVNums = gsub(".*:(.*)", "\\1", revVersions)
    afterInd = grep("+", revVersions, fixed=TRUE)
    after = revVNums[afterInd]
    before = revVNums[-afterInd]
    if(compareVersion(after, version) == 0)
        found = TRUE
    if(minrev == maxrev -1) {
        if(compareVersion(after, version) == 0)
            return(maxrev)
        else if (compareVersion(before, version) == 0)
            return(minrev)
        else
            return(NULL)
    } else if(compareVersion(before, version) == -1)
        return(binRevSearch(version, floor((currev + maxrev)/2), maxrev, currev, param = param, found = found))
    else
        return(binRevSearch(version, floor((minrev + currev )/2),  currev, minrev, param = param, found = found))
}
                                        #-1 is second is later, 1 if first is later
        
        ##svn log -q VERSION | grep ^r | awk '{print $1}' | sed -e 's/^r//' 
        
setMethod("gotoVersCommit", c(dir = "character", src = "SVNSource"),
          function(dir, src, version, param = SwitchrParam){
              if(!file.exists(dir))
                  stop("checkout directory does not appear to exist")
          ##    findSVNRev = function(name, version, svn_repo, destpath, repo) {
              ret = findSVNRev(src@name, version = version, svn_repo = makeSVNURL(src),
                  destpath = dir, param = param)
              dir
          })


        

          
