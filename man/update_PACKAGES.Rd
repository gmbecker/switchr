% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_PACKAGES2.R
\name{update_PACKAGES}
\alias{update_PACKAGES}
\title{update existing package repository}
\usage{
update_PACKAGES(
  dir = ".",
  fields = NULL,
  type = c("source", "mac.binary", "win.binary"),
  verbose = dryrun,
  unpacked = FALSE,
  subdirs = FALSE,
  latestOnly = TRUE,
  addFiles = FALSE,
  strict = TRUE,
  dryrun = FALSE,
  logfun = message,
  ...
)
}
\arguments{
\item{dir}{See \code{write_PACKAGES}}

\item{fields}{See \code{write_PACKAGES}}

\item{type}{See \code{write_PACKAGES}}

\item{verbose}{Should informative messages be displayed throughout
the proccess. Defaults to the value of \code{dryrun} (whose
own default is \code{FALSE}) NOT passed to \code{write_PACKAGES}}

\item{unpacked}{See \code{write_PACKAGES}}

\item{subdirs}{See \code{write_PACKAGES}}

\item{latestOnly}{See \code{write_PACKAGES}}

\item{addFiles}{See \code{write_PACKAGES}}

\item{strict}{logical. Should 'strict mode' be used when checking
existing PACKAGES entries. See details. Defaults to
\code{TRUE}.}

\item{dryrun}{logical. Should should the necessary updates be
calculated but NOT applied. (default \code{FALSE})}

\item{logfun}{function. If \code{verbose} is \code{TRUE}, the
function to be used to emit the informative messages. Defaults
to \code{message}}

\item{\dots}{Additional arguments to \code{write_PACKAGES} -  e.g., the relatively new
rds_compress argument.}
}
\description{
Update an existing repository by reading the PACKAGES
file and only processing built package tarballs which do not
match existing entries.

\code{update_PACKAGES} can be much faster than
\code{write_PACKAGES} for small-moderate changes to large
repository indexes.
}
\details{
Throughout this section, \emph{package tarball} is taken to mean a tarball
file in \code{dir} whose name can be interpreted as
\code{<package>_<version>.<ext>} (or that is pointed to by the \code{File}
field of an existing PACKAGES entry). \emph{Novel package tarballs} are
those which do not match an existing \code{PACKAGES} file entry.

\code{update_PACKAGES} avoids (re)processing package tarballs in cases where
a \code{PACKAGES} file entry already exists and appears to remain valid. The
logic for detecting still-valid entries is as follows:

Currently \code{update_PACKAGES} calls directly down to
\code{write_PACKAGES} (and thus no speedup should be expected)
if any of the following conditions hold:
\itemize{
    \item No \code{PACKAGES} file exists under \code{dir}
    \item \code{unpacked} is \code{TRUE}
    \item \code{subdirs} is anything other than \code{FALSE}
    \item \code{fields} is not \code{NULL} and one or more specified fields
      are not present in the existing \code{PACKAGES} file
 }

All package tarballs whose last modify times are later than that
of the existing PACKAGES file are considered novel and no attempt
is made to identify or retain any corresponding \code{PACKAGES}
entries. Similarly, all \code{PACKAGES} entries which have no
corresponding package tarball are definitionally invalid.

When \code{strict = TRUE}, \code{PACKAGES} entries which appear to
match a package tarball are confirmed via MD5 checksum; those that
pass are retained as valid. All novel package tarballs are fully
proccessed by the standard \code{write_PACKAGES} machinery, and
the resulting entries are added. Finally, if \code{latestOnly =
TRUE}, package-version pruning is performed across the entries.

When \code{strict = FALSE}, package tarballs are assumed to encode
correct metadata in their filenames. \code{PACKAGES} entries which
appear to match a package tarball are retained as valid (No MD5sum
checking occurs). If \code{latestOnly = TRUE}, package-version
pruning across the full set of retained entries and novel package
tarballs \emph{before} the processing of the novel tarballs, at
significant computational and time savings in some
situations. After the optional pruning, any relevant novel package
tarballs are processed via \code{write_PACKAGES} and added to the
set of retained entries.

After the above process concludes, the final database of
\code{PACKAGES} entries is written to all three PACKAGES files,
overwriting the existing files.
}
\note{
While both strict and nonstrict modes offer speedups when
    updating small percentages of large repositories, non-strict
    mode is \emph{much} faster and is recommended in situations where the
    assumptions it makes are safe.
}
\seealso{
\link[tools]{write_PACKAGES}
}
\author{
Gabriel Becker
}
