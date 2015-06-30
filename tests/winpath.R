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
