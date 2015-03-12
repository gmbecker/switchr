library(XML)
pg = htmlParse("http://ropensci.org/packages/")
linkaroos = getNodeSet(pg, "//a[./span/text()='github']/@href")
^
library(switchr)
supathang = gsub("https://github.com/", "", unlist(linkaroos))
man = GithubManifest(pkgrepos = supathang)
