library(switchr)

  ghman = GithubManifest("gmbecker/rpath", "hadley/lazyeval",
      "hadley/dplyr", "rstudio/ggvis")

res = lazyRepo("ggvis", manifest = ghman)

  Install(c("Rcpp", "ggvis"), ghman)
