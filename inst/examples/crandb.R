library(crandb)
library(magrittr)
skip_lines <- function(text, head = 1e6, tail = 1e6) {
    text <- strsplit(text, "\n")[[1]]
    tail <- min(tail, max(0, length(text) - head))
    skip_text <- if (length(text) > head + tail) {
        paste("\n... not showing", length(text) - head - tail, "lines ...\n")
    } else {
        character()
    }
    c(head(text, head), skip_text, tail(text, tail)) %>%
        paste(collapse = "\n")
}
DB <- function(api, head = 1e6, tail = head) {
  paste0("http://db.r-pkg.org", "/", api) %>%
    httr::GET() %>%
    httr::content(as = "text", encoding = "UTF-8") 
}


library(RJSONIO)
vers = fromJSON(DB("/-/release/2.15.3", head=20))

tarnames = paste0(names(vers), "_", vers, ".tar.gz")

cranurls = paste("http://cran.r-project.org/src/contrib/Archive",
    names(vers), tarnames, sep = "/")

man = Manifest(name = names(vers), type = "tarball", subdir = ".", branch = NA, url = cranurls,
    dep_repos = character())

switchTo("R2.15.3Lib")
Install("nlme", man)
