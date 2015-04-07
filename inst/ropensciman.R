library(XML)
pg = htmlParse("http://ropensci.org/packages/")
linkaroos = getNodeSet(pg, "//a[./span/text()='github']/@href")
library(switchr)
supathang = gsub("https://github.com/", "", unlist(linkaroos))
supathang = gsub("/$", "", supathang)
names(supathang) = NULL
man = GithubManifest(pkgrepos = supathang)
man2 = GithubManifest(dataone = "DataONEorg/rdataone/dataone",
    dataonelibs = "DataONEorg/rdataone/dataonelibs",
    datapackage = "ropensci/datapackage",
    redland = "ropensci/redland-bindings/R/redland")

man3 = c(as(man, "PkgManifest"), man2)
install_packages("EML", man3, dep=TRUE)
install_packages("dataone", man3, dep=TRUE)
install_packages("datapackage", man3, dep=TRUE)


library(GRANBase)

para = RepoBuildParam(basedir="~/myrepo", repo_name="testrepo")
repo = GRANRepository(man3, param= para)
makeRepo(repo, cores = 3)
