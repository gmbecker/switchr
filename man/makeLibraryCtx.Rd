% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeLibraryCtx.R
\name{makeLibraryCtx}
\alias{makeLibraryCtx}
\title{makeLibraryCtx}
\usage{
makeLibraryCtx(
  name,
  seed = NULL,
  pkgs = NULL,
  exclude.site = TRUE,
  contains,
  rvers = NULL,
  verbose = FALSE
)
}
\arguments{
\item{name}{The name for the library}

\item{seed}{The object to seed the library from}

\item{pkgs}{Pkgs to install upon creation. Deprecated, use a seeding object
instead.}

\item{exclude.site}{Whether the site library should be excluded when
switching to this library}

\item{contains}{Currently unused.}

\item{rvers}{Optional R version. If specified, existing libraries much be
associated with the same R version to be considered a match.}

\item{verbose}{Should informative messages be emitted to the console}
}
\description{
Locate or create a specified switchr library
}
\details{
This function is not intended to be called directly in most cases; switchTo
calls it automatically.
}
