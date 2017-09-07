
basepkgs = installed.packages(priority="base")[, "Package"]


##' @rdname lazyRepo
##' @aliases lazyRepo,SessionManifest,ANY
setMethod("lazyRepo", c(pkgs = "SessionManifest", pkg_manifest = "ANY"),
          function(pkgs,
                   pkg_manifest,
                   versions,
                   dir = tempdir(),
                   rep_path = file.path(dir, "repo"),
                   get_suggests = FALSE,
                   verbose = FALSE,
                   scm_auths = list(bioconductor = c("readonly", "readonly")),
                   param = SwitchrParam(),
                   force_refresh = FALSE){

              
              lazyRepo(pkgs = versions_df(pkgs)$name,
                       pkg_manifest = manifest(pkgs),
                       versions = versions_df(pkgs)$version,
                       dir = dir,
                       rep_path = rep_path,
                       get_suggests = get_suggests,
                       verbose = verbose,
                       scm_auths = scm_auths,
                       param = param,
                       force_refresh = force_refresh)
          })

##' @rdname lazyRepo
##' @aliases lazyRepo,PkgManifest,ANY
setMethod("lazyRepo", c(pkgs = "PkgManifest", pkg_manifest = "ANY"),
          function(pkgs,
                   pkg_manifest,
                   versions,
                   dir = tempdir(),
                   rep_path = file.path(dir, "repo"),
                   get_suggests = FALSE,
                   verbose = FALSE,
                   scm_auths = list(bioconductor = c("readonly", "readonly")),
                   param = SwitchrParam(),
                   force_refresh = FALSE){

              
              lazyRepo(pkgs = manifest_df(pkgs)$name,
                       pkg_manifest = pkgs,
                       versions = rep(NA, length(nrow(manifest_df(pkgs)))),
                       dir = dir,
                       rep_path = rep_path,
                       get_suggests = get_suggests,
                       verbose = verbose,
                       scm_auths = scm_auths,
                       param = param,
                       force_refresh = force_refresh)
          })



##' @rdname lazyRepo
##' @aliases lazyRepo,character,SessionManifest

setMethod("lazyRepo", c(pkgs = "character", pkg_manifest = "SessionManifest"),
          function(pkgs,
                   pkg_manifest,
                   versions,
                   dir = tempdir(),
                   rep_path = file.path(dir, "repo"),
                   get_suggests = FALSE,
                   verbose = FALSE,
                   scm_auths = list(bioconductor = c("readonly", "readonly")),
                   param = SwitchrParam(),
                   force_refresh = FALSE){

              vers = versions_df(pkg_manifest)$version
              inds = match(pkgs, versions_df(pkg_manifest)$name)
              inds = inds[!is.na(inds)]
              vers = rep(NA, times = length(pkgs))
              vers[inds] = versions_df(pkg_manifest)$version[inds]
              lazyRepo(pkgs = pkgs,
                       pkg_manifest = manifest(pkg_manifest),
                       versions = versions,
                       dir = dir,
                       rep_path = rep_path,
                       get_suggests = get_suggests,
                       verbose = verbose,
                       scm_auths = scm_auths,
                       param = param,
                       force_refresh = force_refresh)
          })


##' @rdname lazyRepo
##' @aliases lazyRepo,character,PkgManifest

setMethod("lazyRepo", c(pkgs = "character", pkg_manifest = "PkgManifest"),
          function(pkgs,
                   pkg_manifest,
                   versions = rep(NA, times = length(pkgs)),
                   dir = tempdir(),
                   rep_path = file.path(dir, "repo"),
                   get_suggests = FALSE,
                   verbose = FALSE,
                   scm_auths = list(bioconductor = c("readonly", "readonly")),
                   param = SwitchrParam(),
                   force_refresh = FALSE){

             
              pkgsNeeded = pkgs

              mandf = manifest_df(pkg_manifest)
              avail = available.packages(contrib.url(dep_repos(pkg_manifest), type="source"), type="source")

              repdir = normalizePath2(file.path(rep_path, "src", "contrib"))
              if(!file.exists(repdir))
                  dir.create(repdir, recursive = TRUE)
              fakerepo = makeFileURL(repdir)
              innerFun = function(src, pkgname, version, dir, param, force_refresh= FALSE) {
                  ## if we only select 1 row we get a character :(
                  if(is.null(dim((avail))))
                      avail = t(as.matrix(avail))

                  if(pkgname %in% avail[,"Package"] || pkgname %in% basepkgs) {
                      if(verbose)
                          message(paste("Package", pkgname, "already available from",
                                        "repository at",
                                        avail[avail[,"Package"] == pkg, "Repository"]))
                      pkgsNeeded <<- setdiff(pkgsNeeded, pkgname)
                      return()
                  }

                  if(is(src, "TarballSource") && !is.na(version)) {
                      v = gsub(".*_(.*)\\.(tar|zip).*", "\\1", location(src))
                      if(compareVersion(v, version) != 0)
                          stop("Got a tarball source (direct link) to the wrong package version for package ", pkgname, ". Likely inconsistent Seeding Manifest")
                  }
                  tballpat =  paste(pkgname, "_", version,sep="")

                  tmpdir = tempdir()
                  pkgdir = file.path(dir, pkgname)    
                  
                  remOtherPkgVersions(pkgname, version, repdir, tmpdir, verbose = verbose)
                  ## check if the exact package version we want has already been retreived in lazy repo
                  exInRepo  = list.files(repdir, pattern = tballpat, full.names=TRUE)
                  exInTmpdir =  list.files(tmpdir, pattern = tballpat, full.names=TRUE)                        
                  if(length(exInRepo) && !force_refresh) {
                      if(verbose)
                          message(sprintf("Package %s (Version %s) found already in repo.",
                                          pkgname, version))
                      
                      pkgsNeeded <<- setdiff(pkgsNeeded, pkgname)
                      desc = fileFromBuiltPkg(exInRepo[1], files = .descInTB(src, FALSE),
                            exdir = tmpdir)
        
                      dcf = read.dcf(file.path(tmpdir, pkgname,subdir(src), "DESCRIPTION")) 
                  } else if(length(exInTmpdir)) {
                     if(verbose)
                       message(sprintf("Package %s (Version %s) found in pkg storage",
                                      pkgname, version))
                     pkgfile = file.path(repdir, basename(exInTmpdir[1]))
                     file.rename(exInTmpdir[1], pkgfile)
                     desc = fileFromBuiltPkg(exInRepo[1], files = .descInTB(src, FALSE),
                                             exdir = tmpdir)
                     dcf = read.dcf(file.path(tmpdir, pkgname,subdir(src), "DESCRIPTION")) 

                     ## tarballs don't really support versions. We will check if it
                     ## is the right version below and throw an error if it isn't
                 } else if(!is.na(version) && !is(src, "TarballSource")) {
       
                      pkgfile = locatePkgVersion( src@name, version, pkg_manifest = pkg_manifest,
                          dir = dir, param = param)
                      if(is.null(pkgfile))
                          stop("Unable to locate the specified version  of package",
                               src@name)
                      if(file.info(pkgfile)$isdir)
                          desc = file.path(pkgfile, subdir(src),"DESCRIPTION")
                      else {
                          ## this was getting hit and "built" later but only had the
                          ## DESCRIPTION file in it. Best to just untar the whole thing
                          ## succ= fileFromBuiltPkg(pkgfile, files = .descInTB(src),
                          ##     exdir = file.path(tempdir())
                          succ = untar(pkgfile, exdir = tmpdir)
                          if(!succ)
                              desc = file.path(tmpdir, pkgname, subdir(src),"DESCRIPTION")
                          else
                              stop("problem extracting DESCRIPTION from tarred package")
                      }
                      dcf = read.dcf(desc)
                  }  else {                         
                      
                      if(verbose)
                          message(sprintf("Retrieving package %s from %s (branch %s)",
                                          pkgname, location(src), branch(src)))
                      
                      success = makePkgDir(pkgname, src, path = dir,
                          latest_only = is.na(version), param = param)
                      if(!success)
                          stop("Unable to make package directory")

                      dcf = read.dcf(file.path(pkgdir, subdir(src),"DESCRIPTION"))
                  }
                  
                  fields = colnames(dcf)
                  .dcfField = function(field, default = NA) {
                      if(field %in% colnames(dcf)) unname(dcf[1, field]) else NA
                  }
                  row = c(Package = pkgname,
                      Version = .dcfField("Version"),
                      Priority = NA,
                      Depends = .dcfField("Depends"),
                      Imports = .dcfField("Imports"),
                      LinkingTo = .dcfField("LinkingTo"),
                      Suggests = .dcfField("Suggests"),
                      Enhances = NA,
                      License = .dcfField("License"),
                      License_is_FOSS = NA,
                      License_restricts_use = NA,
                      OS_type = .dcfField("OS_type"),
                      Archs = NA,
                      MD5sum = .dcfField("MD5sum"),
                      NeedsCompilation = .dcfField("NeedsCompilation"),
                      File = NA,
                      Repository = fakerepo)
                  
                  rawdeps = c(row["Depends"],
                      row["Imports"],
                      row["LinkingTo"],
                      if(get_suggests) row["Suggests"] else NULL)
                  rawdeps = rawdeps[!is.na(rawdeps)]
                  newreqs = unlist(sapply(rawdeps, extract_dep_pkg_names))
                  newreqs = unique(newreqs[!newreqs %in% c(avail[,"Package"],
                      pkgsNeeded, basepkgs)])

                  if(length(list.files(repdir, pattern = tballpat)) == 0) {
                      if(verbose)
                          message(sprintf("Building package %s", pkgname))
                      args = c("build", "--no-resave-data",  noVignettesArg(), file.path(pkgdir, subdir(src)))
                      res = tryCatch(system_w_init(paste(R.home("bin"), "Rcmd", sep="/"), args = args,
                                                   dir = repdir, intern=TRUE,
                                                   param = param),
                        error = function(x) x)
                  if(is(res, "error"))
                      stop(paste("Unable to build package", res))
                  }
                  ##update
                  pkgsNeeded <<- setdiff(c(pkgsNeeded, newreqs), pkgname)
                  
                  avail <<- rbind(avail, t(as.matrix(row)))
                  ##   matrix(row, nrow = 1, dimnames = list(pkgname, names(row))))
              }
              force(avail)
              avail = avail[!avail[,"Package"] %in% mandf$name,]
              if(is.null(dim(avail))) ##if there's only 1 pkg it gives a character :(
                  avail = t(avail)
              pkgsNeeded = setdiff(pkgsNeeded, avail[,"Package"])
              cnt =1 
              while(length(pkgsNeeded) && cnt < 1000){
                  
                  pkg = pkgsNeeded[1]
                  ## base packages are not installable and can't be safely replaced
                  ## within an R installation
                  if(pkg %in% basepkgs)
                      pkgsNeeded <<- setdiff(pkgsNeeded, pkg)
                  else {
                      vers = versions[pkgs == pkg]
                      if(!length(vers))
                          vers = NA
                      
                      if(pkg %in% mandf$name) {
                          src = sourceFromManifest(pkg, pkg_manifest)
                    ##      innerFun(src, pkg, version = vers, dir = repdir,
                                innerFun(src, pkg, version = vers, dir = dir,
                                   param = param,
                                   force_refresh = force_refresh) 
                      } else if(pkg %in% avail[,"Package"])
                            pkgsNeeded <<- setdiff(pkgsNeeded, pkg)
                        else
                            stop(sprintf("Unable to locate package %s", pkg))
                                        #    }
                  }
                  cnt = cnt + 1
              }
              write_PACKAGES(repdir, type="source")
              fakerepo
          })


## tar chokes when /./ appears in internal paths, so prevent that.
.descInTB = function(src, use_subdir=TRUE) {
    if(use_subdir && !is.na(subdir(src)) && subdir(src) != ".")
        file.path(pkgname(src), subdir(src), "DESCRIPTION")
    else
        file.path(pkgname(src), "DESCRIPTION")
}
