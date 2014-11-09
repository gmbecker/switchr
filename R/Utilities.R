##' @export
checkIsPkgDir = function (dir)
{
    fils = list.files(dir)
    any(grepl("^DESCRIPTION$", fils))
}

##' @export
findPkgDir = function(rootdir, branch, subdir, repo, logfun)
{
                   
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
    } else if(is.null(branch) || branch %in% c("master", "trunk")) {
        ret = rootdir
    } else {
        warning(paste0("The svn repository at ", location(source),
                       " does not appear to have branches. ",
                       "Unable to process this source."))
        logfun(name, paste("The SVN repository does not appear to have",
                                 "branches and a non-trunk/non-master branch",
                                 "was selected"), repo = repo, type="both")
        return(NULL)
    }

    ret = file.path(ret, subdir)
    ##we somehow got a return file that doesn't exist on the file system.
    ##This is a problem with GRAN logic, not with packages/user activity
    if(!file.exists(ret))
    {
        logfun(name, paste("Unable to find subdirectory", subdir,
                                 "in branch", branch), repo, type="both")
        warning(paste0("Constructed temporary package directory",ret,
                       " doesn't appear to  exist after svn checkout. ",
                       "Missing branch?"))
        return(NULL)
    }
    
    ##Find a package. First look in ret, then in ret/package and ret/pkg
    ##we could be more general and allow people to specify subdirectories...
    if(!checkIsPkgDir(ret))
    {
        logfun(name, paste("Specified branch/subdirectory combination",
                                 "does not appear to contain an R package"),
                                 repo, type="both")
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

##' @export
makeSource = function(url, type, user, password, scm_auth, prefer_svn = FALSE, ...) {
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
        cran = new("CRANSource", location = url, user = "", password = ""),
        bioc = new("BiocSource", location = url, user = "readonly", password = "readonly"),
        tarball = new("TarballSource", location = url, user = "", password =""),
           stop("unsupported source type")
           )
    if( (type=="git" || type == "github") && is.na(ret@branch))
        ret@branch = "master"
    else if (type=="svn" && is.na(ret@branch))
        ret@branch = "trunk"
    if(is(ret, "GitSource") && prefer_svn) {
        ret2 = tryCatch(as(ret, "SVNSource"), error = function(x) x)
        if(!is(ret2, "error"))
            ret = ret2
    }
    ret
}

##' @export
getPkgDir = function(basepath,name,  subdir, scm_type, branch)
{

    
    basepath = normalizePath2(basepath)
    if(!file.exists(file.path(basepath, name)))
        stop("directory not found")
    ##svn
    if(file.exists(file.path(basepath, name, ".svn")))
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



##' @export
normalizePath2 = function(path, follow.symlinks=FALSE)
    {
        
        if(follow.symlinks || Sys.info()["sysname"]=="Windows")
            normalizePath(path)
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
##' @export
system_w_init = function(cmd,
    init = character(), ..., param = SwitchrParam())
{
    if(!length(init) && !is.null(param))
        init = sh_init_script(param)
    if(length(cmd) > 1)
        stop("cmd should be of length 1")
    if(length(init) && nchar(init))
        cmd = paste(paste("source", init), cmd, sep = " ; ")
    system(cmd, ...)
}

highestVs = c(9, 14, 0)
develVers = 3.0

decrBiocVersion = function(biocVers) {
    vals = strsplit(biocVers, ".", fixed=TRUE)[[1]]
    vals  = as.numeric(vals)
    if(identical(vals, c(1,0))) {
        NULL
    } else if (vals[2] == 0) {
        vals[1] = vals[1] - 1 #decrement major version
        vals[2] = highestVs[ vals[1] ] #set to highest minor version for that major
    } else {
        vals[2] = vals[2] - 1
    }
    paste(vals, collapse=".")
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

highestBiocVers = function(repos = biocinstallRepos()[-length(biocinstallRepos())] ) {
    majvers = length(highestVs)
##    if(highestVs[majvers] > 0)
##        vers = paste(majvers, highestVs[majvers] - 1, sep=".")
##    else
##        vers = paste(majvers -1, highestVs[majvers-1], sep=".")
    vers = paste(majvers, highestVs[majvers], sep=".")
    bef= gsub("(.*/)[0-9][^/]*/.*", "\\1", repos)
    af = gsub(".*/[0-9][^/]*(/.*)", "\\1", repos)
    paste0(bef, vers, af)
}
