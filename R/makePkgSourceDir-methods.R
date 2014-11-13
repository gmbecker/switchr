#setMethod("makePkgSourceDir", c(name = "ANY", source = "SVNSource"), function(name, source, path, repo) {
setMethod("makePkgDir", c(name = "ANY", source = "SVNSource"),
          function(name, source, path, latest_only = FALSE, param,
                   forceRefresh = FALSE) {
              lfun = logfun(param)
              oldwd = getwd()
              on.exit(setwd(oldwd))
              if(!file.exists(path))
                  dir.create(path, recursive = TRUE)
              setwd(path)
              
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
                      logfun(param)(name, msg, type="error", param)
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


setMethod("makePkgDir", c(name = "ANY", source = "GithubSource"),
          function(name, source, path, latest_only = FALSE,  param, forceRefresh = FALSE)
      {
          if(latest_only) {
              ##https://github.com/gmbecker/ProteinVis/archive/IndelsOverlay.zip
              ## for IndelsOverlay branch
              zipUrl = paste0(gsub("\\.git", "", location(source)), "/archive/",
                  branch(source), ".zip")
              zipUrl = gsub("git://", "http://", zipUrl, fixed=TRUE)
              zpfile = normalizePath(file.path(path,
                  paste(name, "-", branch(source), ".zip", sep = "")))
              if(!file.exists(zpfile) || forceRefresh)
                  success = download.file(zipUrl, zpfile, method = "wget")
              else
                  success = 0
              if(success > 0)
                  stop("Unable to get package zip file from git")
              uzdir = file.path(path, paste(name, branch(source), sep="-"))
              unzip(zpfile, exdir = path)
              uzdir
          } else {
              source = as(source, "SVNSource", strict = TRUE)
              makePkgDir(name, source, path, latest_only, param = param, forceRefresh)
          }
      })

#setMethod("makePkgSourceDir", c(name = "ANY", source = "GitSource"), function(name, source, path,  repo) {
setMethod("makePkgDir", c(name = "ANY", source = "GitSource"),
          function(name, source, path, latest_only = FALSE, param, forceRefresh=FALSE)
      {
          oldwd = getwd()
          on.exit(setwd(oldwd))
          if(!file.exists(path))
              dir.create(path, recursive = TRUE)
          setwd(path)
          sdir = location(source)
          if(file.exists(name) &&
             file.exists(file.path(name, ".git"))) {
              logfun(param)(name, "Existing temporary checkout found at this location. Updating")
              up = updateGit(file.path(path, name), source, param = param)
          } else {
              if(file.exists(name))
                  unlink(name)
              cmd = paste("git clone", sdir, name, ";cd", name, "; git checkout", branch(source))
              res = tryCatch(system_w_init(cmd, intern=TRUE, param = param),
                  error=function(x) x)
              if(is(res, "error") || (!is.null(attr(res, "status")) && attr(res, "status") > 0))
              {
                  logfun(param)(name, paste("Failed to check out package source using command:", cmd),
                               type="both")
                  logfun(param)(name, res, type="error")
                  return(FALSE)
              }
              logfun(param)(name, paste0("Successfully checked out package source from ",
                                        sdir, " on branch ", branch(source)))
          }
          rtdir = file.path(path, name)
          ret = !is.null(findPkgDir(rtdir, branch(source), source@subdir, param = param))
                                        #success log
          if(ret)
          {
              logfun(param)(name, paste("Temporary source directory successfully created:", ret))
          }
          ret
      })
                                        #stub for everyone else
setMethod("makePkgDir", c(name = "ANY", source = "ANY"),
          function(name, source, path, latest_only, param, forceRefresh = FALSE) {
    warning("Source type not supported yet.")
    FALSE
})

setMethod("makePkgDir", c(name = "ANY", source = "TarballSource"),
          function(name, source, path, latest_only, param, forceRefresh = FALSE) {
              loc = location(source)
              if(grepl("://", loc)) {
                  destfile = file.path(path, basename(loc))
                  download.file(loc, destfile = destfile)
                  loc = destfile
              }
              untar(loc, exdir = path)
              file.path(path, name)
          })



#setMethod("makePkgSourceDir", c(source="LocalSource"), function(name, source, path, latest_only = FALSE, repo) {
setMethod("makePkgDir", c(source="LocalSource"),
          function(name, source, path,  latest_only, param, forceRefresh = FALSE) {
    oldwd = getwd()
    on.exit(setwd(oldwd))
    if(!file.exists(path))
        dir.create(path, recursive = TRUE)
    setwd(path)
    
    if(missing(name))
        name = basename(location(source))
    
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
