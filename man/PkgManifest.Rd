% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\name{PkgManifest}
\alias{PkgManifest}
\alias{PkgManifest-class}
\title{PkgManifest}
\usage{
PkgManifest(
  manifest = ManifestRow(...),
  dep_repos = defaultRepos(),
  ...,
  dl_method
)
}
\arguments{
\item{manifest}{The manifest (data.frame) of packages and their locations}

\item{dep_repos}{A list of traditional pkg repositories which can contain dependencies
for the packages listed in \code{manifest}.}

\item{\dots}{Arguments passed to \code{\link{ManifestRow}} if \code{manifest} is not specified}

\item{dl_method}{Download method. Ignored unless \code{manifest} is a
character scalar containing a URL to a serialized manifest}
}
\description{
Construct a PkgManifest, which can be installed from using \code{\link{install_packages}}
}
\details{
If a package is found in both the manifest dataf.frame and the dependency
repositories, the version in the manifest will always take precidence within the
switchr framework.
}
