                                        #setMethod("makePkgSourceDir", c(name = "ANY", source = "SVNSource"), function(name, source, path, repo) {
##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,SVNSource
setMethod("makePkgDir", c(name = "ANY", source = "SVNSource"),
          function(name, source, path, latest_only = FALSE, param,
                   forceRefresh = FALSE) {
              lfun = logfun(param)
              if(!file.exists(path))
                  dir.create(path, recursive = TRUE)
              oldwd = setwd(path)
              on.exit(setwd(oldwd))              
              if(missing(name))
                  name = basename(location(source))
              
              opts = character()
              if(length(source@user) && nchar(source@user))
                  opts = paste(opts, "--username", source@user)
              if(length(source@password) && nchar(source@password))
                  opts = paste(opts, "--password", source@password)
              
                                        #did we already check it out?
              if(file.exists(name) && file.exists(name, ".svn") && !forceRefresh)
              {
                  lfun(name, "Existing temporary checkout found at this location. Updating")
                  up = updateSVN(file.path(path, name), source,  param = param)
              } else {
                  ## clean up the directory if it was created from some other type of source
                  if(file.exists(name))
                      unlink(name, recursive = TRUE)
                  cmd = paste("svn co", location(source), name, opts)
                  lfun(name, paste("Attempting to create temporary source",
                                   "directory from SVN repo", location(source),
                                   "(branch", source@branch, "; cmd", cmd, ")"))
                  out = tryCatch(system_w_init(cmd, param = param), error = function(x) x)
                  if(is(out, "error"))
                  {
                      msg = c(paste("Temporary SVN checkout failed. cmd:", cmd), out$message)
                      logfun(param)(name, msg, type="error")
                      return(FALSE)
                  }
              }
              rtdir = file.path(path, name)
              
              ret = !is.null(findPkgDir(rtdir, branch(source), source@subdir, param = param))
              
                                        #success log
              if(ret)
              {
                  logfun(param)(name,
                                paste("Temporary source directory successfully created:",
                                      ret)
                                )
              }
              ret
          })

##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,GithubSource
##' @importFrom utils unzip
setMethod("makePkgDir", c(name = "ANY", source = "GithubSource"),
          function(name, source, path, latest_only = FALSE,  param, forceRefresh = FALSE)
      {
          if(latest_only) {
              ##https://github.com/gmbecker/ProteinVis/archive/IndelsOverlay.zip
              ## for IndelsOverlay branch
              
              reponm = gsub("\\.git", "", location(source))
              reponm = gsub(".*/([^/]+)/{0,1}$", "\\1", reponm)
              zipUrl = paste(gsub("\\.git", "", location(source)),
                  "/archive/", branch(source), ".zip", sep="")
              zipUrl = gsub("git://", "http://", zipUrl, fixed=TRUE)
              zpfile = normalizePath(file.path(path,
                  paste(name, "-", branch(source), ".zip", sep = "")))
              if(!file.exists(zpfile) || forceRefresh)
                  success = download.file(zipUrl, zpfile, method = "wget")
              else
                  success = 0
              if(success > 0)
                  stop("Unable to get package zip file from git")
              destdir = file.path(path, name)
              if(file.exists(destdir))
                  unlink(destdir, recursive=TRUE)
              unzip(zpfile, exdir = path)
              file.rename(file.path(path, paste(reponm, branch(source), sep="-")),
                          destdir)
                                        #              uzdir
          } else {
          #    source = as(source, "GitSource", strict = TRUE)
           #   makePkgDir(name, source, path, latest_only, param = param, forceRefresh)
              callNextMethod()
          }
      })

                                        #setMethod("makePkgSourceDir", c(name = "ANY", source = "GitSource"), function(name, source, path,  repo) {
##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,GitSource
setMethod("makePkgDir", c(name = "ANY", source = "GitSource"),
          function(name, source, path, latest_only = FALSE, param, forceRefresh=FALSE)
      {

          if(!file.exists(path))
              dir.create(path, recursive = TRUE)
          oldwd = setwd(path)
          if(!is.null(oldwd))
              on.exit(setwd(oldwd))
          else
              warning("working directory returned as NULL, unable to reset it after creating pkg directory")
          sdir = location(source)
          if(file.exists(name) &&
             file.exists(file.path(name, ".git")) &&
               file.exists(file.path(name, "DESCRIPTION"))) {
              logfun(param)(name, "Existing temporary checkout found at this location. Updating")
              up = updateGit(file.path(path, name), source, param = param)
          } else {
              if(file.exists(name))
                  unlink(name, recursive=TRUE )
              cmd = paste("git clone", sdir, name)
              res = tryCatch(system_w_init(cmd, intern=TRUE, param = param),
                  error=function(x) x)
              if(is(res, "error") || (!is.null(attr(res, "status")) && attr(res, "status") > 0))
              {
                  logfun(param)(name, paste("Failed to clone package source using command:", cmd),
                               type="both")
                  logfun(param)(name, res, type="error")
                  return(FALSE)
              }
              medwd = setwd(name)
              cmd2 = paste( " git checkout", branch(source))
              res2 = tryCatch(system_w_init(cmd2, intern=TRUE, param = param), error = function(e) e)
              setwd(medwd)
              if(is(res, "error") || (!is.null(attr(res, "status")) && attr(res, "status") > 0))
              {
                logfun(param)(name, paste("Failed to check out package source using command:", cmd2),
                              type="both")
                logfun(param)(name, res, type="error")
                return(FALSE)
              }
              
              logfun(param)(name, paste0("Successfully checked out package source from ",
                                        sdir, " on branch ", branch(source)))
          }
          rtdir = file.path(path, name)
          ret = file.exists(rtdir)
                                        #success log
          if(ret)
          {
              logfun(param)(name, paste("Temporary source directory successfully created:", ret))
          } 
          
          ret
      })
##stub for everyone else
##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,ANY
setMethod("makePkgDir", c(name = "ANY", source = "ANY"),
          function(name, source, path, latest_only, param, forceRefresh = FALSE) {
    warning("Source type not supported yet.")
    FALSE
})
##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,CRANSource
##' @importFrom utils untar download.packages
setMethod("makePkgDir", c(name="ANY", source="CRANSource"), function(name, source, path, latest_only, param, forceRefresh = FALSE) {

    if(!file.exists(file.path(path, name)))
        dir.create(file.path(path, name), recursive=TRUE)
    
    pkg =  download.packages(name, destdir = path, method = "wget")[,2]
    untar(pkg,exdir = path)
    TRUE
    
})

##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,BiocSource

setMethod("makePkgDir", c(name="ANY", source="BiocSource"), function(name, source, path, latest_only, param, forceRefresh = FALSE) {

    if(!requireNamespace2("BiocInstaller", quietly=TRUE))
        stop("Can't handle BiocSource without BiocInstaller installed")
    if(!file.exists(file.path(path, name)))
        dir.create(file.path(path, name), recursive=TRUE)

    pkg =  download.packages(name, destdir = path, method = "wget", repos = BiocInstaller::biocinstallRepos())[,2]
    untar(pkg,exdir = path)
    return(TRUE)
    
    
})




##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,TarballSource

setMethod("makePkgDir", c(name = "ANY", source = "TarballSource"),
          function(name, source, path, latest_only, param, forceRefresh = FALSE) {
              loc = location(source)
              if(grepl("://", loc)) {
                  destfile = file.path(path, basename(loc))
                  download.file(loc, destfile = destfile)
                  loc = destfile
              }
              untar(loc, exdir = path)
              file.exists(file.path(path, name))
                 
          })





##'@rdname makePkgDir
##' @aliases makePkgDir,ANY,LocalSource

setMethod("makePkgDir", c(name="ANY",source="LocalSource"),
          function(name, source, path,  latest_only, param, forceRefresh = FALSE) {

    if(file.exists(file.path(path, name)))
        unlink(file.path(path, name), recursive=TRUE, force=TRUE)
    if(!file.exists(path))
        dir.create(path, recursive = TRUE)
    oldwd = setwd(path)
    on.exit(setwd(oldwd))
        
    if(missing(name))
        name = basename(location(source))
    if(file.exists(file.path(path, name)))
        unlink(file.path(path, name), recursive=TRUE, force=TRUE)
    logfun(param)(name, "Copying local source directory into temporary checkout directory.")
    
   # ok= file.copy(location(source), file.path(path, name), recursive = TRUE)
    ok = file.copy(normalizePath2(location(source)), file.path(path), recursive=TRUE)
    if(basename(location(source)) != name)
    {
        logfun(param)(name, "Renamed copied directory to package name.")
        ok = file.rename(file.path(path, basename(location(source))), file.path(path, name))
    }
    ret = normalizePath2(file.path(path, name, source@subdir))
    
                                        #success log
    if(ok)
    {
        logfun(param)(name, paste("Temporary source directory successfully created:",
                                  ret))
    }
    ok
    
})
