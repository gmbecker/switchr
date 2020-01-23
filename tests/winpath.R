library(switchr)
## if(getOption("repos")["CRAN"] == "@CRAN@")
##     chooseCRANmirror(ind=1L)
graceful_inet(TRUE)
checkUrlRoundtrip = function(pth) {
    pth = switchr:::normalizePath2(pth)
    furl = switchr:::makeFileURL(pth)
    pth2 = switchr:::fileFromFileURL(furl)
    if(pth != pth2)
        stop("Round trip result (", pth2, ") does not match original (", pth, ") when creating file URLs")
}

## regression test to ensure that the roundtrip
## between path and file url is working properly
checkUrlRoundtrip(getwd())
checkUrlRoundtrip(tempdir())
if(switchr:::isWindows())
    checkUrlRoundtrip("\\\\laptop\\My Documents\\")

## regreossion test to ensure that package dependencies
## are added to the dontunload list when a package
## is specified
bef = switchrDontUnload()
af = switchrDontUnload("knitr")
stopifnot(all(c("stringr", "stringi", "magrittr") %in% af))


## regression test for unnecessary (and expensive)
## lazy repo construction when install_packages
## is passed urls for existing repos (the default
## behavior).
tmplib = tempdir()
trace(lazyRepo, stop)
install_packages("switchr", repos = defaultRepos(), lib = tmplib)
untrace("lazyRepo", where = getNamespace("switchr"))

## regression test for DESCRIPTION annotation with dups in manifest
## This fails on the CRAN build systems but works locally, so disabled
## man = PkgManifest(name = c("fastdigest", "fastdigest"), type = "cran")
## install_packages("fastdigest", man, lib = tmplib)


## regression test for generating empty manifest df and empty manifest
mandf = ManifestRow()
stopifnot(nrow(mandf) == 0)

man = PkgManifest()
stopifnot(nrow(manifest_df(man)) == 0)

sman = SessionManifest(man)
stopifnot(nrow(manifest_df(sman)) == 0 && nrow(versions_df(sman)) == 0)

## make sure bioc version stuff works
isFALSE = function(x) identical(x, FALSE)
thing = switchr:::develVers
if(!isFALSE(graceful_inet())) ## absent forced failure bioc urls shoould work...
    stopifnot(is(thing, "character"))

#stopifnot(nrow(available.packages(contrib.url(switchr:::highestBiocVers())))>0)

## regression test for publishManifest
mantests = function(man) {
    tmpdir = tempdir()
    manfil = publishManifest(man, file.path(tmpdir, "sman.rman"))
    mantab = read.csv(manfil, comment.char="#")
    stopifnot(nrow(mantab) > 0)
    man2 = loadManifest(manfil)
    stopifnot(identical(man, man2))
}


## this tests the ability to make a seedManifest
## including looking up the types
sman = makeSeedMan()
man = manifest(sman)
mantests(sman)
mantests(man)


dir = file.path(tempdir(), "gittst")
if(!file.exists(dir))
    dir.create(dir)

pkgdir = normalizePath2(file.path(dir, "GRANBase"), mustWork = FALSE)
unlink(pkgdir, recursive=TRUE)
param= SwitchrParam(logfun = function(...) cat(paste(..., sep=" - "), "\n" ))
dfile = file.path(pkgdir, "DESCRIPTION")


test_ghupdate_dirty = function() {
    ghsource = makeSource("https://github.com/gmbecker/gRAN", type = "git", scm_auth=list(), branch = "master", name = "GRANBase")
    res = switchr:::inet_handlers(makePkgDir("GRANBase", ghsource, dir, FALSE, param = param))
    if(is(res, "error")) {
        message("skipping github update test due to connectivity issues")
        return(TRUE)
    }
 
    oldwd = getwd()
    on.exit(setwd(oldwd))
    setwd(pkgdir)
    oldD = readLines(dfile)
    
    writeLines("lol", con = dfile)
    newD = readLines(dfile)
    upd1 = switchr:::updateGit(pkgdir, ghsource, param)
    newD2 = readLines(dfile)
    if(newD != newD2)
        stop( "Local changes not preserved during updateGit call from within correct branch")
    system("git checkout -f DESCRIPTION")
    
    TRUE
}


test_ghupdate_branch = function() {
    ghsourcebr = makeSource("https://github.com/gmbecker/gRAN", type = "git", scm_auth=list(), branch = "API_refactor", name = "GRANBase")

    res = switchr:::inet_handlers(makePkgDir("GRANBase", ghsourcebr, dir, FALSE))
    if(is(res, "error")) {
        message("skipping github update test due to connectivity issues")
        return(TRUE)
    }

    oldwd = setwd(pkgdir)
    on.exit(setwd(oldwd))
    system2("git", args = c("checkout master"))
    
    upd2 = switchr:::updateGit(pkgdir, ghsourcebr, param)
    if(!upd2)
        stop("Update of source checkout when currently on different branch failed")
    system("git checkout master")
    oldD = readLines(dfile)
    writeLines("lol", con=dfile)
    upd3 = tryCatch(switchr:::updateGit(pkgdir, ghsourcebr, param), error = function(x) x)
    if(!is(upd3, "error"))
        stop("switchr did not throw an error non-current branch was updated with dirty wd")
    system("git checkout -f DESCRIPTION")
    TRUE
}
    

        
        
if(switchr:::haveGit()) {    
    ## with clean/non-existent directory
    test_ghupdate_dirty()
    
    ## updating existing directory
    test_ghupdate_dirty()

    test_ghupdate_branch()

}
    



## Bioconductor does not live in SVN anymore...

## biocman <<- tryCatch(BiocSVNManifest(), error = function(e) e)
## test_biocsvnman = function() {
##     if(is(biocman, "error") || nrow(biocman) == 0) {
##         message("Unable to retrieve bioc manifest. problem with Https url? skipping Bioc manifest and SVN-based tests")
##         return(TRUE)
##     }
##     biocman2 <- BiocSVNManifest(software_only=FALSE)
##     if(nrow(manifest_df(biocman)) >= nrow(manifest_df(biocman2)) - 100) #at time of writing there are ~300 experimental data packages
##         stop("BiocSVNManifest does not appear to have including experimental data packages properly")
    
##     TRUE
## }

## ## This is causing problems on a system we use switchr on, so temporarily
## ## disabled
## test_biocsvnman()


## test_svncheckout = function() {
##     pkgdir2 = normalizePath2(file.path(dir, "Biobase"), mustWork = FALSE)
##     unlink(pkgdir2, recursive=TRUE)
##     dfile2 = file.path(pkgdir2, "DESCRIPTION")
    
##     svnsrc = switchr:::sourceFromManifest("Biobase", biocman)
##     res = makePkgDir("Biobase", svnsrc, dir, FALSE)
##     if(!file.exists(dfile2))
##         stop("SVN checkout test does not appear to have worked")
## }
        
## if(switchr:::haveSVN() && !is(biocman, "error") && nrow(biocman) > 0) {
##     test_svncheckout()
## }



## test_.grabdeps = function() {
##     desc = read.dcf(system.file("DESCRIPTION", package="switchr"))
##     deps = switchr:::.grabdeps(desc, FALSE)
##     deps2 = switchr:::.grabdeps(desc, TRUE)
##     if(!identical(sort(deps), sort(c("RCurl", "RJSONIO", "methods", "tools" ))))
##         stop("deps returned non-zero for switchr with suggests=FALSE")
##     if( length(deps2) != 5 || length(union(deps2, c("BiocInstaller", "RCurl", "methods", "RJSONIO", "tools"))) != 5)
##         stop("Didn't get BiocIntaller, RCurl, and RJSONIO for deps of switchr including suggests, got ", paste(sort(deps2), collapse=", ") )
##     avl = available.packages("http://cran.rstudio.com/src/contrib")
##     deps3 = switchr:::.grabdeps(avl["switchr", , drop=FALSE], FALSE)
##     deps4 = switchr:::.grabdeps(avl["switchr", , drop=FALSE], TRUE)
##  #   if(!identical(deps, deps3) || !identical(deps2, deps4))
##   #      stop(".grabdeps did not give the same behavior for consuming available pkgs matrix and description file")
        
##     TRUE
    
## }

#test_.grabdeps()

    
## Test replace argument to addPkg

test_addReplace = function() {

    man = makeManifest(name = c("switchr", "ggplot2"),
                   type="cran")
    
    man2 = addPkg(man,
                  name = "fastdigest",
                  url = "http://github.com/gmbecker/fastdigest",
                  type = "git")
    if(nrow(man2) !=3)
        stop("addPkg with replace=FALSE did not work for new package")

    ## different order than they appear in the manifest
    man3 = addPkg(man2,
                  name = c("fastdigest", "switchr"),
                  url = c("fakefd", "fakeswitchr"),
                  replace = TRUE)
    man3df = manifest_df(man3)
    if(nrow(man3) != 3 ||
       man3df$url[man3df$name == "switchr"] != "fakeswitchr" ||
       man3df$url[man3df$name == "fastdigest"] != "fakefd")
        stop("Replace argument in addPkgs failed when replacing multiple rows")

    man4 = addPkg(man2,
                  name = c("fastdigest", "GRANBase"),
                  url = "fake",
                  replace=TRUE)
    man4df = manifest_df(man4)
    
    if(nrow(man4) != 4 ||
       man4df$url[man4df$name == "fastdigest"] != "fake")
        stop("Replace argument in addPkgs failed with mixed replace and new rows")
       
    TRUE
       
}

test_addReplace()

