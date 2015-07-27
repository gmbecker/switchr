library(switchr)
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

## regression test to ensure that package dependencies
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

## regression test for generating empty manifest df and empty manifest
mandf = ManifestRow()
stopifnot(nrow(mandf) == 0)

man = PkgManifest()
stopifnot(nrow(manifest_df(man)) == 0)

sman = SessionManifest(man)
stopifnot(nrow(manifest_df(sman)) == 0 && nrow(versions_df(sman)) == 0)

## make sure bioc version stuff works
thing = switchr:::develVers
stopifnot(is(thing, "character"))

stopifnot(nrow(available.packages(contrib.url(switchr:::highestBiocVers())))>0)

## regression test for publishManifest
mantests = function(man) {
    tmpdir = tempdir()
    manfil = publishManifest(man, file.path(tmpdir, "sman.rman"))
    mantab = read.csv(manfil, comment.char="#")
    stopifnot(nrow(mantab) > 0)
    man2 = loadManifest(manfil)
    stopifnot(identical(man, man2))
}


sman = makeSeedMan()
man = manifest(sman)
mantests(sman)
mantests(man)


test_ghupdate_dirty = function() {
    ghsource = makeSource("https://github.com/gmbecker/gRAN", type = "git", scm_auth=list(), branch = "master", name = "GRANBase")
    dir = file.path(tempdir(), "gittst")
    if(!file.exists(dir))
        dir.create(dir)
    res = makePkgDir("GRANBase", ghsource, dir, FALSE)
    oldwd = getwd()
    on.exit(setwd(oldwd))
    pkgdir = file.path(dir, "GRANBase")
    setwd(pkgdir)
    dfile = file.path(pkgdir, "DESCRIPTION")
    oldD = readLines(dfile)
    
    writeLines("lol", con = dfile)
    newD = readLines(dfile)
    upd1 = switchr:::updateGit(pkgdir, ghsource, SwitchrParam())
    newD2 = readLines(dfile)
    if(newD != newD2)
        stop( "Local changes not preserved during updateGit call from within correct branch")
    
    setwd(oldwd)
    unlink(pkgdir, recursive=TRUE)
    ghsourcebr = makeSource("https://github.com/gmbecker/gRAN", type = "git", scm_auth=list(), branch = "API_refactor", name = "GRANBase")
    res = makePkgDir("GRANBase", ghsourcebr, dir, FALSE)
    curbr = switchr:::gitCurrentBranch(SwitchrParam())
    system("git checkout master")
    oldwd = setwd(pkgdir)
    upd2 = switchr:::updateGit(pkgdir, ghsourcebr, SwitchrParam())
    if(!upd2)
        stop("Update of source checkout when currently on different branch failed")
    system("git checkout master")
    oldD = readLines(dfile)
    writeLines("lol", con=dfile)
    upd3 = tryCatch(switchr:::updateGit(pkgdir, ghsourcebr, SwitchrParam()), error = function(x) x)
    if(!is(upd3, "error"))
        stop("switchr did not throw an error non-current branch was updated with dirty wd")
    system("git checkout DESCRIPTION")
    setwd(oldwd)
    on.exit(NULL)
    TRUE
}
test_ghupdate_dirty()



test_.grabdeps = function() {
    desc = read.dcf(system.file("DESCRIPTION", package="switchr"))
    deps = .grabdeps(desc, FALSE)
    deps2 = .grabdeps(desc, TRUE)
    if(!identical(sort(deps), c("methods", "tools")))
        stop("deps returned non-zero for switchr with suggests=FALSE")
    if( !identical(sort(deps2), c("BiocInstaller", "methods", "RCurl", "RJSONIO", "tools")))
        stop("Didn't get BiocIntaller, RCurl, and RJSONIO for deps of switchr including suggests")
    avl = available.packages()
    deps3 = .grabdeps(avl["switchr", , drop=FALSE], FALSE)
    deps4 = .grabdeps(avl["switchr", , drop=FALSE], TRUE)
    if(!identical(deps, deps3) || !identical(deps2, deps4))
        stop(".grabdeps did not give the same behavior for consuming available pkgs matrix and description file")
    
    TRUE
   
}



    
