
##' locatePkgVersion
##'
##' Locate and download/build the exact version of a single package.
##'
##' @param name package name
##' @param version package version string
##' @param pkg_manifest A manifest containing locations to search for the
##' package
##' @param param A SwitchrParam object
##' @param repo (optional) GRANRepository object to search
##' @param dir directory to download package into
##' @return The full path to the downloaded file , or NULL if unable to
##' locate the package
##' @note Locating and attempting to install a non-current version of a single
##' will not work in general, due to dependency issues. In most cases a
##' Just-in-Time repository should be created and used instead, e.g. via
##' \code{\link{install_packages}}
##'
##' This function is called internally during the construction of Just-in-Time
##' repositories and during the installation of specific package versions.
##' @author Gabriel Becker
##' @export
locatePkgVersion = function(name, version, pkg_manifest, param = SwitchrParam(),
    dir = notrack(repo), repo = NULL) {
    
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
    fname = findPkgVersionInRepo(repo, name, version, param = param)
    if(length(fname) && file.exists(fname))
        return(fname)

    
    ## round 2: check the manifest
    fname = findPkgVersionInManifest(name, version, pkg_manifest,
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

findPkgVersionInCRAN = function(name, version, param = SwitchrParam(), dir)
{
    destpath = dir
    pkgs = as.data.frame(available.packages( fields = c("Package", "Version"), type="source"), stringsAsFactors = FALSE)
    if(nrow(pkgs) && name %in% pkgs$Package)
    {
        pkg = pkgs[pkgs$Package == name,]
        if(pkg$Version == version){
            res = download.packages2(name, destdir = destpath, type="source")
            if(nrow(res) < 1)
                stop("Package is in CRAN but download failed")
            return(res[1,2])
        }
    }
    
    
    tarname = paste0(name, "_", version, ".tar.gz")
    
    cranurl = paste("http://cran.r-project.org/src/contrib/Archive", name, tarname, sep = "/")
    
    
    if(!file.exists(destpath))
        dir.create(destpath, recursive = TRUE)
    destfile = file.path(destpath, tarname)
    retries = 0
    while(retries < archive_retries(param)) {
        res = tryCatch(download.file2(cranurl, destfile), error = function(e) e)
        Sys.sleep(archive_timing(param))
        if(is(res, "error") || res > 0) {
            retries = retries + 1
            destfile = NULL
        } else
            break
    }
    
    destfile
}


##' findPkgVersionInRepo
##' @param repo The repository
##' @param name The name of the package
##' @param version The version of the package to find
##' @param param A SwitchrParam object
##' @param dir The directory to download the located package tarball into
##' @return A path to the downloaded tarball, or NULL
##' @docType methods
##' @rdname findPkgVersionInRepo
##' @export
setGeneric("findPkgVersionInRepo", function(repo, name, version, param, dir) standardGeneric("findPkgVersionInRepo"))
##' @rdname findPkgVersionInRepo
##' @aliases findPkgVersionInRepo,character
setMethod("findPkgVersionInRepo", "character",
          function(repo, name, version, param, dir) {
              url = paste(repo, paste0(name,"_", version,".tar.gz"), sep="/")
              if(url.exists(url)) {
                  dest = file.path(dir, basename(url))
                  res = download.file(url, method=dl_method(param), destfile = dest)
                  if(res==0L)
                      return(dest)
              }
          NULL
          })


##' @rdname findPkgVersionInRepo
##' @aliases findPkgVersionInRepo,NULL
setMethod("findPkgVersionInRepo", "NULL",
          function(repo, name, version, param, dir) NULL)


## git other than github not currently supported.
## Github allows us to checkout via svn

findPkgVersionInManifest = function(name, version, pkg_manifest, dir,
    param = SwitchrParam() ) {

    destpath = dir
    if(!name %in% manifest_df(pkg_manifest)$name)
        return(NULL)
    
    ## XXX this is a hack that should be happening elsewhere!!!
    manrow = manifest_df(pkg_manifest)[manifest_df(pkg_manifest)$name == name,]

    if(tolower(manrow$type) %in% c("cran", "bioc"))
        return(NULL)
    
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
    pkgdir = makePkgDir(name, source = src, path = dir, param = param)
    if(pkgdir)
        pkgpath = file.path(dir, name)
    else
        return(NULL)
    
    
    ## XXX branches aren't handled here!!!!
    findSVNRev(name = name, version = version,
               svn_repo = makeSVNURL(src),   pkgpath = pkgpath , param = param)
    pkgpath
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







    
findPkgVersionInBioc = function(name, version, param = SwitchrParam(), dir)
{
  
    destpath = dir
    ret = .biocTryToDL(name, version, dir = dir)
    if(!is.null(ret$file))
        return(ret$file)
    else {
        br = ret$biocVers
        ##XXX need to figure out how to deal with bioc branches.
        ## they aren't at the package level so this is difficult

        if(!file.exists(dir))
            dir.create(dir, recursive=TRUE)
        oldwd = setwd(dir)
        on.exit(setwd(oldwd))

        pkgdir = file.path(dir, name)
        ## commit = findBiocSVNRev(name, version, destpath = dir,
        ##     param = param, ret$biocVers)
        commit = findBiocGitRev(name, version, destpath = dir, param = param,
                                biocVers = ret$biocVers)
        if(is.null(commit))
            return(NULL)
        
        
        rbin = paste(file.path(R.home("bin"), "Rcmd"))
            
        system_w_init(rbin, args = c("build", noVignettesArg(), "--no-resave-data", "--no-manual",
                            pkgdir), param = param)
        ret = normalizePath2(list.files(pattern  = paste0(name, "_", version, ".tar.gz"), full.names=TRUE))
        ## XXX Once a way to reset the checkout at the correct time is figured out
        ## fix this if it's still needed. For now we're going to wipe it anyway...
        ##setwd(pkgdir)
        ## system_w_init("svn", args = "up", param = param) #this gets us back to the trunk
        ##system_w_init("git", args = "checkout master", param = param) #this gets us back to the trunk

    }
    ret
}




## tries to download the file. Returns list with two elements (file:dl'ed file or NULL and versionToSearch:bioc version)
##' @importFrom utils contrib.url available.packages compareVersion download.file
.biocTryToDL = function(name, version, param, dir, verbose = FALSE) {
    
    destpath = dir
    urls = contrib.url(highestBiocVers())
    biocVers = biocVersFromRepo(urls)
    
    pkgAvail = TRUE
    everAvail = FALSE
    ret = NULL
    while(!is.null(urls) && pkgAvail && is.null(ret)) {
        if(verbose)
            message(sprintf("Searching Bioc repository for release %s", biocVersFromRepo(urls)))
        
        pkgs = as.data.frame(available.packages(urls, fields = c("Package", "Version"), type="source", filters = "duplicates"), stringsAsFactors=FALSE)
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
                filname = paste0(name, "_", version, .getExt(pkg[1,"Repository"]))
                dstfile = file.path(dir, filname)
                ret = tryCatch(download.file2(paste(pkg[1,"Repository"], filname, sep="/"), destfile=dstfile), error=function(x) x)
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



##' Make a Bioconductor SVN url for a package
##'
##' Make SVN url for a Bioconductor package given the name, bioc version, and
##' type of package.
##' @param name A vector of bioconductor package names The name of the package
##' @param biocVers The version (release) of bioconductor, or \code{'trunk'} (the default) for
##' Bioc devel.
##' @param pkgtype character. Which type of packages to retrieve the SVN root url for. Should be
##' \code{"software"} or \code{"data"} for software and experimental data packages,
##' respectively.
##' @return A vector of urls for the specified packages within the Bioconductor SVN repository
##' @export
makeBiocSVNURL = function(name, biocVers = getBiocvrFromRvr(), pkgtype = "software") {

    biocVers = tolower(biocVers)
    if(biocVers == biocVersFromRepo(highestBiocVers()) || biocVers %in% dev_vers_aliases) {
        biocVers = "trunk"
    } else {
        biocVers = paste("branches/RELEASE", gsub(".", "_", biocVers, fixed=TRUE), sep="_")
    }

    lowerloc = switch(pkgtype,
                      software = "bioconductor",
                      data = "bioc-data",
                      stop("Unsupported pkgtype"))

    loc = switch(pkgtype,
                 software = "madman/Rpacks",
                 data = "experiment/pkgs",
                 stop("Unsupported pkg type"))
    paste("https://hedgehog.fhcrc.org", lowerloc, biocVers, loc, name, sep="/")
}

findBiocSVNRev = function(name, version, destpath, param, biocVers="devel")
{
 
    pkgdir = file.path(destpath, name) ##file.path(destpath, addl_dir)
    repoloc = makeBiocSVNURL(name, biocVers)
    if(!file.exists(pkgdir)) {
        src = makeSource(name = name, url = repoloc, type = "svn", user = "readonly", password="readonly")
        ret = makePkgDir(name = name, source = src, path = destpath, latest_only = FALSE, param = param)
        
        if(!ret)
            return(NULL)
    }         
    res = findSVNRev(name, version, svn_repo = repoloc, pkgpath = pkgdir, param = param)
    if(is.null(res) && ! biocVers %in% dev_vers_aliases) {
        trrepo = makeBiocSVNURL(name, "devel") 
        res = findSVNRev(name, version, svn_repo = trrepo, pkgpath = pkgdir, param = param)
    }
    res
}



findBiocGitRev = function(name, version, destpath, param, biocVers="devel")
{

    if(is.null(biocVers) || is.na(biocVers))
        biocVers = "devel"
        
    ## I'm paranoid about that unlink(pkgdir, recursive=TRUE) call ...
    if(is.null(name) || nchar(name) == 0 || is.na(name) || !grepl("^[[:alpha:]]", name))
        stop("invalid name")
    pkgdir = file.path(destpath, name)

  
    repoloc = paste0("https://git.bioconductor.org/packages/", name)
    ## XXX this is a hack. will reclone every time Figure out a better way. but not today
    if(TRUE || !file.exists(pkgdir) || !file.exists(file.path(pkgdir, ".git"))) {
        if(file.exists(pkgdir)) { ## pkg dir exists but isn't a git checkout...
            unlink(pkgdir, recursive=TRUE)
        }
        src = makeSource(name = name, url = repoloc, type = "git",
                         branch = biocVersAsGitBr(biocVers))
        ret = makePkgDir(name = name, source = src, path = destpath, latest_only = FALSE, param = param)
        
        if(!ret)
            return(NULL)

    }
    
    
    res = findGitRev(name, version, codir = pkgdir, param = param)
    if(is.null(res) && ! biocVers %in% dev_vers_aliases) {
        system_w_init("git", args =  "checkout master", dir = pkgdir)
        res = findGitRev(name, version, codir = pkgdir, param = param)
    }
    res
}




## destpath is the actual package directory, not the general destpath for all pkgs.
## confusing, should change this.
findSVNRev = function(name, version, svn_repo, pkgpath, param) {

    ##setwd(file.path(destpath,  name))
    oldwd = setwd(pkgpath)
    on.exit(setwd(oldwd))
    system_w_init("svn", args = c("switch", "--ignore-ancestry", svn_repo),
                        param = param)

    
    
    
    cmd0 = "svn log -r 1:HEAD --limit 1 DESCRIPTION"
    revs = system_w_init("svn", args = c("log", "-r 1:HEAD", "--limit 1", "DESCRIPTION"),
                         intern = TRUE, param = param)
    minrev = as.numeric(gsub("r([[:digit:]]*).*", "\\1", revs[2])) #first line is -------------------
    cmd1 = "svn log -r HEAD:1 --limit 1 DESCRIPTION"
    revs2 = system_w_init("svn", args = c("log", "-r HEAD:1", "--limit 1", "DESCRIPTION"),
                          intern = TRUE, param = param)
    maxrev = as.numeric(gsub("r([[:digit:]]*).*", "\\1", revs2[2]))
    
    currev = floor((maxrev+minrev)/2)
    
    commit = binRevSearch(version, currev = currev, maxrev = maxrev, minrev = minrev, found = FALSE, param = param)
    if(is.null(commit))
        return(NULL)
    cmd2 = paste("svn switch --ignore-ancestry -r", commit, svn_repo)#repoloc)
    system_w_init("svn", args = c("switch", "--ignore-ancestry", paste("-r", commit),
                                  svn_repo), param = param)
    return(commit)
    }

    
binRevSearch = function(version, currev, maxrev, minrev, param, found = FALSE)
{
    cmd = paste("svn diff --revision", paste(currev, maxrev, sep=":"), "DESCRIPTION")
    revs = tryCatch(system_w_init("svn", args = c("diff", "--revision", paste0(currev, ":", maxrev),
                                                  "DESCRIPTION"), intern = TRUE,
                                  param = param), error=function(x) x)
    if(is(revs, "error"))
        return(NULL)
   
    revVersions = grep("[-\\+].*[Vv]ersion:", revs, value=TRUE)
 
    if(!length(revVersions)) {
        if(minrev == maxrev - 1) {
            if(found)
                return(minrev)
            else
                return(NULL)
        } else {
            return(binRevSearch(version, floor((minrev + currev )/2),  currev, minrev, param=param, found = found))
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

##' @rdname gotoVersCommit
##' @aliases gotoVersCommit,character,SVNSource
setMethod("gotoVersCommit", c(dir = "character", src = "SVNSource"),
          function(dir, src, version, param = SwitchrParam()){
              
              if(!file.exists(dir))
                  stop("checkout directory does not appear to exist")
          ##    findSVNRev = function(name, version, svn_repo, destpath, repo) {
              ret = findSVNRev(src@name, version = version, svn_repo = makeSVNURL(src),
                  pkgpath = dir, param = param)
              dir
          })


##' @rdname gotoVersCommit
##' @aliases gotoVersCommit,character,CRANSource

setMethod("gotoVersCommit", c(dir = "character", src= "CRANSource"),
          function(dir, src, version, param = SwitchrParam()) {
              if(is.na(version))
                  return(dir)
              
              desc = read.dcf(file.path(dir, "DESCRIPTION"), all=TRUE)
              if(compareVersion(version, desc$Version) == 0)
                  return(dir)
              pkg = findPkgVersionInCRAN(src@name, version, param = param, dir = tempdir())
              if(is.null(pkg) || !file.exists(pkg))
                  stop("Unable to find version %s of package %s", version, src@name)
              untar(pkg, exdir = dirname(dir))
              dir
          })

##' @rdname gotoVersCommit
##' @aliases gotoVersCommit,character,BiocSource

setMethod("gotoVersCommit", c(dir = "character", src= "BiocSource"),
          function(dir, src, version, param = SwitchrParam()) {
              if(is.na(version))
                  return(dir)
              
              desc = read.dcf(file.path(dir, "DESCRIPTION"), all=TRUE)
              if(compareVersion(version, desc$Version) == 0)
                  return(dir)
              pkg = findPkgVersionInBioc(src@name, version, param = param, dir = tempdir())
              if(is.null(pkg) || !file.exists(pkg))
                  stop("Unable to find version %s of package %s", version, src@name)
              if(!file.info(pkg)$isdir)
                  untar(pkg, exdir = dirname(dir))
              dir
          })


##' @rdname gotoVersCommit
##' @aliases gotoVersCommit,character,GitSource

setMethod("gotoVersCommit", c(dir="character", src="GitSource"),
          function(dir, src, version, param = SwitchrParam()) {
    
    if(is.na(version))
        return(dir)
    desc = read.dcf(file.path(dir, "DESCRIPTION"), all=TRUE)
    if(compareVersion(version, desc$Version) == 0)
        return(dir)
    ret = findGitRev(src@name, version = version, codir = dir)
    oldwd = setwd(dir)
    on.exit(setwd(oldwd))
    
    dir
})


findGitRev = function(pkg, version, codir, param = SwitchrParam()) {
    if(!file.exists(file.path(codir, "DESCRIPTION")))
        log("Couldn't find DESCRIPTION file in git checkout")
    oldwd = setwd(codir)
    on.exit(setwd(oldwd))
    log = system_w_init("git", args = c("log", "-p", " DESCRIPTION"),
                        intern = TRUE, param = param)
    cpos = grep("commit [[:alnum:]]{40}[[:space:]]*$", log)
    line = grep(paste("\\+[vV]ersion: *", version,"$", sep=""), log)
    if(!length(line)) {
        logfun(param)(pkg, sprintf("Version %s does not appear in the git commit logs on this branch. Searching across multiple branches is not currently supported", version))
        return(NULL)
    }
    cpos = max(cpos[cpos<line])
    sha = gsub("commit ([[:alnum:]]{40})[[:space:]]*$", "\\1", log[cpos])
    cmd = sprintf("git checkout %s", sha)
    res = tryCatch(system_w_init("git", args = c("checkout", sha),
                                 param = param, intern = TRUE), error = function(e) e)
    out = system_w_init("git", args = c("clean","-f", "."),
                                 intern = TRUE, param = param)

    out2 = system_w_init("git", args = c("reset","--hard"),
                                 intern = TRUE, param = param)

    if(is(res, "error")) {
        logfun(param)(pkg, sprintf("Found commit for package version but checking out that commit failed, cmd: %s",cmd), type = "both")
        NULL
    } else {
        codir
    }
}



