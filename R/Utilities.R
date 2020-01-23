
## defaults to src

.getExt = function(repourl = "./src/contrib", regex = FALSE) {
    typ = gsub(".*/(.*)/contrib", "\\1", repourl)
    res = switch(typ,
           src = ".tar.gz",
           win = ".zip",
           mac = ".tgz",
           ## XXX this assumes source unless it gets something that matches the regex
           ## above.
           ".tar.gz")
    if(regex)
        res =  gsub(".", "\\.", res, fixed = TRUE)
    res
}




## (very far backawards compatible version of list.dirs
## list.dirs is apparently a relatively new addition, makes this function fail in R 2.12.1
list.dirs = function(path = ".", full.names = TRUE, recursive = TRUE) {
    if(exists("list.dirs", where = length(search()))) ##this looks in base pkg
        dirs = base::list.dirs(path = path, full.names = full.names,
                         recursive = recursive)
    else {
        ## very old versions of list.files don't have include.dirs or no..
        
        dirs = list.files(path, recursive = recursive,
                          full.names = full.names)
        dirs = dirs[sapply(dirs, function(x) file.info(x)$isdir)]
    }

    dirs
}


url.exists = function(x, ...) {
    if(!is(try(RCurl::url.exists, silent = TRUE), "try-error"))
        RCurl::url.exists(x, ...)
    else {
        con = url(x)
        on.exit(close(con))
        res = tryCatch(readLines(con), error = function(e) e)
        if(is(res, "error"))
            FALSE
        else
            TRUE
    }
}
##' make file url
##'
##' @param path The path to wrap in a file:// URL
##' @return A valid file URL
##' @export
makeFileURL = function(path) {
  if(Sys.info()["sysname"] == "Windows") {
      .winFileURL(path)
  } else
    paste0("file://", normalizePath2(path))
}

isWindows = function( ) tolower(Sys.info()["sysname"]) == "windows"

##' Get path from file URL
##'
##' @param fileurl A file url (beginning in file://)
##' @return The system directory path that \code{fileurl} points to
##' @export
fileFromFileURL = function(fileurl) {
    if(!isWindows())
        ret = gsub("file://",  "" , fileurl, fixed=TRUE)
    else
        ret = .winFileFromURL(fileurl)

}

.winFileFromURL = function(fileurl) {
    if(grepl("file:///", fileurl))
        ret = gsub("^file:///", "", fileurl)
    else if (grepl("^file://[[:alpha:]]", fileurl))
        ret = gsub("^file:",  "", fileurl)
    ret = gsub("%20", " ", ret)
    normalizePath2(ret)
}

.winFileURL = function(path) {
    path = normalizePath2(path, winslash="/")
    path = gsub(" ", "%20", path)
    if(grepl("^//", path))
        paste0("file:", path)
    else
        paste0("file:///", path)
}

remOtherPkgVersions = function(pkgname, version, repodir, storagedir, verbose=FALSE) {
    if(is.na(version))
        return()
    tballpat = paste0(pkgname, "_")
  
  allinrepo = list.files(repodir, pattern = tballpat, full.names=TRUE)
  wrongvers = !grepl(version, allinrepo, fixed=TRUE)
  if(all(!wrongvers))
    return()
  if(verbose)
    message(sprintf("found %d other versions of the package in the repo directory (%s). Moving them to the storage directory (%s)",
                    sum(wrongvers), repodir, storagedir))
  file.rename(allinrepo[wrongvers], 
              file.path(storagedir, basename(allinrepo[wrongvers])))
}

fileFromBuiltPkg = function(archive, files, ...) {
  filname = basename(archive)
  ext = gsub(".*_[^[:alpha:]]*(\\..*)$", "\\1", filname)
  fun = switch(ext,
         ".zip" = unzip,
         ".tar.gz"= untar,
         "tgz" = untar,
         stop(sprintf("unrecognized extension %s", ext))
  )
  
  fun(archive, files = files, ...)
}


## ##' R executable
## ##' @param cmd the R CMD to run. "build", "check", "INSTALL", or "" (for none)
## ##' @param options The options to pass to the command
## ##' @export
## Rcmd2 = function(cmd = c("build", "check", "INSTALL"), options="") {
##     cmd = match.arg(arg=cmd)
##     cmdpart = if(nchar(cmd)) paste("CMD", cmd) else ""
##     paste(file.path(R.home("bin"), "R"), cmdpart,  options)
## }

##'Check if a directory contains package sources
##' @param dir The directory
##' @export
checkIsPkgDir = function (dir)
{
    fils = list.files(dir)
    any(grepl("^DESCRIPTION$", fils))
}

##'Find a package directory within an SCM checkout
##' @param rootdir The directory of the checkout
##' @param branch The branch to navigate to
##' @param subdir The subdirectory to navigate to
##' @param param a SwitchrParam object
##' @return A path to the Package sources
##' @export
findPkgDir = function(rootdir, branch, subdir,param)
{

    if(!length(subdir))
        subdir = "."
    ret = NULL
    name = basename(rootdir)
    #does it have the trunk, branches, tags layout?
    if(checkStdSVN(rootdir))
    {
        if(is.null(branch) || branch %in% c("master", "trunk"))
        {
            ret = file.path(rootdir, "trunk")
        } else {
            ret = file.path(rootdir, "branches", branch)
        }
    } else if(file.exists(file.path(rootdir, ".git"))) {
        if(is.null(branch) || branch == "trunk")
            branch = "master"
        gitChangeBranch(rootdir, branch, param = param)
        ret = rootdir
    } else if ( is.null(branch) || branch %in% c("master", "trunk")) {
        ret = rootdir
    } else {
        warning(paste0("The svn repository at ", rootdir,
                       " does not appear to have branches. ",
                       "Unable to process this source."))
        logfun(param)(name, paste("The SCM repository does not appear to have",
                                  "branches and a non-trunk/non-master branch",
                                  "was selected"),  type="both")
        return(NULL)
    }

    ret = file.path(ret, subdir)
    ##we somehow got a return file that doesn't exist on the file system.
    ##This is a problem with GRAN logic, not with packages/user activity
    if(!file.exists(ret))
    {
        logfun(param)(name, paste("Unable to find subdirectory", subdir,
                                 "in branch", branch), type="both")
        warning(paste0("Constructed temporary package directory",ret,
                       " doesn't appear to  exist after svn checkout. ",
                       "Missing branch?"))
        return(NULL)
    }
    
    ##Find a package. First look in ret, then in ret/package and ret/pkg
    ##we could be more general and allow people to specify subdirectories...
    if(!checkIsPkgDir(ret))
    {
        logfun(param)(name, paste("Specified branch/subdirectory combination",
                                 "does not appear to contain an R package"),
                                  type="both")
        ret = NULL
    }
    ret
}



makeUserFun = function(scm_auth, url)
    {
        ind = sapply(names(scm_auth), function(pat) grepl(pat, url, fixed=TRUE))
        if(any(ind))
            scm_auth[[which(ind)]][1]
        else
            ""

    }

makePwdFun = function(scm_auth, url)
    {
        ind = sapply(names(scm_auth), function(pat) grepl(pat, url, fixed=TRUE))
        if(any(ind))
            scm_auth[[which(ind)]][2]
        else
            ""
       
    }

##' Create a PkgSource object for a package
##' @param url The url of the package sources
##' @param type The source type.
##' @param user A function which, when called, returns the username to use when
##' when checking the soources out
##' @param password A function which returns the password to use when checking
##' out the sources
##' @param scm_auth A list of username-password pairs, named with regular
##' expressions to match against url when constructing the
##' defaults for \code{user} and \code{password}
##' @param prefer_svn Currently unused.
##' @param \dots Passed directly to constructors for PkgSource superclasses
##' @export
makeSource = function(url, type, user, password, scm_auth = list(), prefer_svn = FALSE, ...) {
    if(is.na(url) && is.na(type)) {
        repos = c(getOption("repos"), getBiocRepos())
        repos = repos[!duplicated(names(repos))]
        atcran = grep("@CRAN@", repos)
        if(length(atcran) > 0 && !interactive())
            chooseCRANmirror(ind = 1L)

        ## name is passed in as a ... here, and usually not needed
        ## but it is needed if we are trying to find the url/type
        ## of a package
        
        name = list(...)$name
        stopifnot(!is.null(name))
        ## see manifestFromLib.R
        tmp = .findIt(name, repos = repos)
        type = tmp$type # either still NA or a real type
        url = tmp$url # either still NA or a real url
    }
    
    if(is.na(type)) {
        type = "unknown"
    }
    type = tolower(type)
    if(missing(user))
        user = makeUserFun(scm_auth = scm_auth, url = url)
    if(missing(password))
        password = makePwdFun(scm_auth= scm_auth, url = url)
    if(type == "git" && grepl("github", url))
        type = "github"

    ret = switch(type,
           svn  = new("SVNSource", location = url, user = user,
               password = password, ...),
           local = new("LocalSource", location = url, user = user,
               password= password, ...),
           git = new("GitSource", location = url, user = user,
               password = password,  ...),
           github = new("GithubSource", location = url, user = user,
               password = password, ...),
        cran = new("CRANSource", location = url, user = "", password = "", ...),
        bioc = new("BiocSource", location = url, user = "readonly", password = "readonly", ...),
        tarball = new("TarballSource", location = url, user = "", password ="", ...),
           stop("unsupported source type: ", type)
           )
    if( (type=="git" || type == "github") && is.na(ret@branch))
        ret@branch = "master"
    else if (type=="svn" && (!length(ret@branch) || is.na(ret@branch)))
        ret@branch = "trunk"
    if(is(ret, "GitSource") && prefer_svn) {
        ret2 = tryCatch(as(ret, "SVNSource"), error = function(x) x)
        if(!is(ret2, "error"))
            ret = ret2
    }
    ret
}

##' Construct pockage directory path
##' @param basepath The parent directory for the package directory
##' @param name The name of the package
##' @param subdir The subdirectory within a package source that
##' the actual package root directory will reside in.
##' @param scm_type Tye type of scm the package sources will be
##' checked out from
##' @param branch The branch from which the package will be retrieved.
##' @return A path
##' @note Unlike \code{\link{findPkgDir}} this does not look for existing
##' package source directories. It only constructs the path.
##' 
##' @export
getPkgDir = function(basepath,name,  subdir, scm_type, branch)
{

    
    basepath = normalizePath2(basepath)
    if(!file.exists(file.path(basepath, name)))
        stop("directory not found")
    ##svn
##    if(file.exists(file.path(basepath, name, ".svn")))
    if(scm_type == "svn")
    {
        if(checkStdSVN(file.path(basepath, name)))
        {
            if(is.na(branch) || branch == "trunk" || branch == "master")
                brdir = "trunk"
            else
                brdir = file.path("branches", branch)

        } else {
            brdir = "."

        }
    } else ## git or local, neither have explicit dirs for branching
        brdir = "."

    normalizePath2(file.path(basepath,name, brdir, subdir))
}


##' normalizePath2
##'
##' Attempt to normalize a relative path to an absolute one. Optionally without
##' resolving symlinks on non-Windows systems
##' @param path The path to normalize
##' @param follow.symlinks Should symlinks (other than . and ..)
##' be resolved to their physical locations? (FALSE)
##' @param winslash The value of winslash to be passed down to normalizePath
##' on windows systems
##' @param mustWork logical. Passed to normalizePath on windows. Ignored otherwise.
##' @return The normalized path.
##' @export
normalizePath2 = function(path, follow.symlinks=FALSE, winslash = "\\", mustWork = NA)
    {
        
        if(follow.symlinks || Sys.info()["sysname"]=="Windows")
            return(normalizePath(path, winslash = winslash, mustWork = mustWork))
        else {
            if(substr(path, 1, 1) == "~")
                path = path.expand(path)
            ##paths starting with / for example
            else if(substr(path, 1, 1) == .Platform$file.sep)

                path  = path
            else if (substr(path, 1, 2) == "..") {
                tmppath = getwd()
                while(substr(path, 1, 2) == "..") {
                    tmppath = dirname(tmppath)
                    path = substr(path, 3, nchar(path))
                    if(substr(path, 1, 1) == .Platform$file.sep)
                        path = substr(path, 2, nchar(path))
                }
                path = file.path(tmppath, path)
            } else if(grepl("^\\.*[[:alnum:]]", path))
                path = file.path(getwd(), path)
            else if (substr(path, 1,1) == ".")
                path = file.path(getwd(), substr(path,2, nchar(path)))
            path = gsub(paste(rep(.Platform$file.sep, 2), collapse=""), .Platform$file.sep, path, fixed=TRUE)
            path
            
        }
    }





##source an initialization script (e.g. .bashrc) if specified
## in sh_init_script(repo)
##' system_w_init
##'
##' Run a system command with an optional intialization script (e.g. a .bashrc
##' sourced first).
##' @param cmd The text of the command. Must be length 1.
##' @param dir The directory that the command should be executed in. The working directory will be temporarily changed to this dir,
##' but will be changed back upon exit of system_w_init.
##' @param init (optional) a character value indicating the
##' location of an initialization shell script.
##' @param args character. Arguments to be passed to the command
##' @param env character. Environmental variables to be set when running the command
##' @param \dots additional parameters passed directly to \code{\link{system}}.
##' @param param A SwitchrParam object. The shell initialization
##' script associated with this object is used when \code{init} is
##' not specified (length 0).
##' @return Depends, see \code{\link{system}} for details.
##' @export
system_w_init = function(cmd, dir,
    init = character(), args = NULL, env = NULL, ..., param = SwitchrParam())
{
    pause = shell_timing(param) > 0

    if(length(cmd) > 1)
        stop("Vectors of commands not supported by system_w_init")
    
    
    if(!length(init) && !is.null(param))
        init = sh_init_script(param)

    ## hacky barebones  recreation of system2
    
    if(isWindows())
        cmd = paste( c(shQuote(cmd), env, args), collapse = " ")
    else
        cmd = paste( c(env, shQuote(cmd), args), collapse = " ")
    
    
    if(length(init) && nchar(init))         
        paste(shQuote(paste("source", init, ";")), cmd)
    
    if(!missing(dir)) {
        oldwd  = getwd()
        setwd(dir)
        on.exit(setwd(oldwd))
    }
    system(cmd, ...)
}

    


beforeBiocInstaller = function() {
    Rver = R.Version()
    vstr = paste(Rver$major, Rver$minor, sep = ".")
    compareVersion(vstr, "2.14.0") < 0
}


#system(..., intern=TRUE) throws an error if the the command fails,
#and has attr(out, "status") > 0 if the called program returns non-zero status.
##' Identify error states from R or external programs
##' @param out An R object representing output
##' @return TRUE if out is an error object, or has an attribute called "status" which is > 0
##' @export
errorOrNonZero = function(out)
{
    if(is(out, "error") ||
       (!is.null(attr(out, "status")) && attr(out, "status") > 0))
        TRUE
    else
        FALSE
}


## NB this will give different behavior in R versions that
## provide requireNamespace and thsoe that don't re the search path.
## Not ideal, but otherwise switchr will fail to install at all.
requireNamespace2 = function(...) {
    if(exists("requireNamespace"))
        requireNamespace(...)
    else
        require(...)
}

## this will give identical behavior but will be less efficient
## when paste0 doesn't exist.
if(!exists("paste0"))
    paste0 = function(...) paste(..., sep="")
        

sourceFromManifest = function(pkg, manifest, scm_auths = list(bioconductor=c("readonly", "readonly")), ...) {
    mandf = manifest_df(manifest)
    manrow = mandf[mandf$name == pkg, ]
    if(nrow(manrow) == 0)
        return(NULL)
    manrow = manrow[1,]
    ##https://github.com/gmbecker/ProteinVis/archive/IndelsOverlay.zip
    ## for IndelsOverlay branch
    src = makeSource(name = pkg,
        type = manrow$type,
        url = manrow$url, branch = manrow$branch,
        subdir = manrow$subdir,
        scm_auth = scm_auths,...)
    src
}

isWindows = function() {
  Sys.info()["sysname"] == "Windows"
}

haveGit = function() nchar(Sys.which("git")) > 0

haveSVN = function() nchar(Sys.which("svn")) > 0

download.file2 = function(url, destfile, method, ...) {
    if(missing(method)) {
        lc = capabilities("libcurl")
        if(length(lc) && lc)
            method = "libcurl"
        else if(nchar(Sys.which("wget")) > 0)
            method = "wget"
        else if(nchar(Sys.which("curl")) > 0)
            method = "curl"
        else
            method = "auto"
    }

    download.file(url, destfile, method, ...)
}



download.packages2 = function(pkgs, destdir, avail = NULL,
                              repos = getOption("repos"),
                              contrib = contrib.url(repos, type),
                              method, type = getOption("pkgType"),
                              ...) {
    if(missing(method)) {
        lc = capabilities("libcurl")
        if(length(lc) && lc)
            method = "libcurl"
        else if(nchar(Sys.which("wget")) > 0)
            method = "wget"
        else if(nchar(Sys.which("curl")) > 0)
            method = "curl"
        else
            method = "auto"
    }

    download.packages(pkgs = pkgs, destdir = destdir, available = avail,
                  repos = repos, contriburl = contrib,
                  method = method, type=type, ...)
}


noVignettesArg = function() {
    Rvers = paste(R.version$major, R.version$minor, sep=".")
    
    if(compareVersion(Rvers, "3.1.0") < 0)
        "--no-vignettes"
    else
        "--no-build-vignettes"
}




## .build_repository_package_db_update = function (dir, fields = NULL,
##                                                 type = c("source", "mac.binary", 
##                                                          "win.binary"),
##                                                 verbose = getOption("verbose"),
##                                                 unpacked = FALSE,
##                                                 cur.db) 
## {
##     if (unpacked) 
##         return(tools:::.build_repository_package_db_from_source_dirs(dir, 
##             fields, verbose))
##     type <- match.arg(type)
##     package_pattern <- switch(type, source = "_.*\\.tar\\..*$", 
##         mac.binary = "_.*\\.tgz$", win.binary = "_.*\\.zip$")
##     files <- list.files(dir, pattern = package_pattern)

##     filepkgvers = gsub("(\\.tar\\..*|\\.tgz|\\.zip)$", "", basename(files))

##     dbpkgvers = paste(cur.db[,"Package"], cur.db[,"Version"], sep = "_")

##     files = files[!(filepkgvers %in% dbpkgvers)]
    
    
##     if (!length(files)) 
##         return(list())
##     fields <- unique(c(tools:::.get_standard_repository_db_fields(type), 
##         fields))
##     packages <- sapply(strsplit(files, "_", fixed = TRUE), "[", 
##         1L)
##     db <- vector(length(files), mode = "list")
##     names(db) <- files
##     op <- options(warn = -1)
##     on.exit(options(op))
##     if (verbose) 
##         message("Processing packages:")
##     if (type == "win.binary") {
##         files <- file.path(dir, files)
##         for (i in seq_along(files)) {
##             if (verbose) 
##                 message(paste(" ", files[i]))
##             con <- unz(files[i], file.path(packages[i], "DESCRIPTION"))
##             temp <- tryCatch(read.dcf(con, fields = fields)[1L, 
##                 ], error = identity)
##             if (inherits(temp, "error")) {
##                 close(con)
##                 next
##             }
##             db[[i]] <- temp
##             close(con)
##         }
##     }
##     else {
##         dir <- file_path_as_absolute(dir)
##         files <- file.path(dir, files)
##         cwd <- getwd()
##         if (is.null(cwd)) 
##             stop("current working directory cannot be ascertained")
##         td <- tempfile("PACKAGES")
##         if (!dir.create(td)) 
##             stop("unable to create ", td)
##         on.exit(unlink(td, recursive = TRUE), add = TRUE)
##         setwd(td)
##         for (i in seq_along(files)) {
##             if (verbose) 
##                 message(paste(" ", files[i]))
##             p <- file.path(packages[i], "DESCRIPTION")
##             temp <- try(utils::untar(files[i], files = p))
##             if (!inherits(temp, "try-error")) {
##                 temp <- tryCatch(read.dcf(p, fields = fields)[1L, 
##                   ], error = identity)
##                 if (!inherits(temp, "error")) {
##                   if ("NeedsCompilation" %in% fields && is.na(temp["NeedsCompilation"])) {
##                     l <- utils::untar(files[i], list = TRUE)
##                     temp["NeedsCompilation"] <- if (any(l == 
##                       file.path(packages[i], "src/"))) 
##                       "yes"
##                     else "no"
##                   }
##                   temp["MD5sum"] <- md5sum(files[i])
##                   db[[i]] <- temp
##                 }
##                 else {
##                   message(gettextf("reading DESCRIPTION for package %s failed with message:\n  %s", 
##                     sQuote(basename(dirname(p))), conditionMessage(temp)), 
##                     domain = NA)
##                 }
##             }
##             unlink(packages[i], recursive = TRUE)
##         }
##         setwd(cwd)
##     }
##     if (verbose) 
##         message("done")
##     db
## }



## update_PACKAGES = function(dir = ".", fields = NULL,
##                            type = c("source", "mac.binary", 
##                                     "win.binary"),
##                            verbose = FALSE, unpacked = FALSE,
##                            subdirs = FALSE, latestOnly = TRUE,
##                            addFiles = FALSE) {
    
##     if(!file.exists(file.path(dir, "PACKAGES"))) {
##         if(verbose)
##             message("No existing PACKAGES file found, delegating to write_PACKAGES")
        
##         ret = tools::write_PACKAGES(dir = dir, fields = fields, type = type,
##                                     verbose = verbose, unpacked = unpacked,
##                                     subdirs = subdir, latestOnly = latestOnly,
##                                     addFiles = addFiles)
##         return(ret)
##     } else if (unpacked) {
##         if(verbose)
##             message("unpacked is TRUE, delegating to write_PACKAGES")
##         ret = tools::write_PACKAGES(dir = dir, fields = fields, type = type,
##                                     verbose = verbose, unpacked = unpacked,
##                                     subdirs = subdir, latestOnly = latestOnly,
##                                     addFiles = addFiles)
##         return(ret)
##     }
    
##     type <- match.arg(type)
##     nfields <- 0
##     out <- file(file.path(dir, "PACKAGES"), "wt")
##     outgz <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")

##     paths <- ""
##     existing = read.dcf(out)
##     if (is.logical(subdirs) && subdirs) {
##         owd <- setwd(dir)
##         paths <- list.dirs(".")
##         setwd(owd)
##         paths <- c("", paths[paths != "."])
##     }
##     else if (is.character(subdirs)) 
##         paths <- c("", subdirs)


    
## }
