library(XML)
pg = htmlParse("http://ropensci.org/packages/")
linkaroos = getNodeSet(pg, "//a[./span/text()='github']/@href")
library(switchr)
supathang = gsub("https://github.com/", "", unlist(linkaroos))
man = GithubManifest(pkgrepos = supathang)

man2 = PkgManifest(name=c("dataone", "dataonelibs"),
    subdir=c("dataone","dataonelibs"),
    url = c("http://github.com/ropensci/rdataone/",
        "http://github.com/DataONEorg/rdataone/"),
    type="git")

man3 = c(as(man, "PkgManifest"), man2)
install_packages("EML", man3)


library(GRANBase)

para = RepoBuildParam(basedir="~/myrepo", repo_name="testrepo")
repo = GRANRepository(man3, param= para)
makeRepo(repo, cores = 3)
