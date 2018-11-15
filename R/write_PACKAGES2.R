
## this might be really slow. If so, move to numeric_version objects
.newestPkgVersion = function(inds, df, verscol = "version") {
    if(length(inds) ==1)
        return(inds)
    
    curind = inds[1]
    ## head(x, -1) is all but the last element
    ## walk the versions once to get the latest one
    for(i in head(seq(along = inds), -1)) {
        if(compareVersion(df[[verscol]][ inds[ i ] ],
                          df[[verscol]][ inds[ i + 1 ] ]) < 0)
            curind = inds[i+1]
        }
    curind
}

##' Find newest packages in a package info data.frame
##' @param df data.frame. Table of package information
##' @param pkgcol string. Name of column containing package name
##' @param verscol string. Name of column containing package version in version-string form.
##' @return a data.frame with the same columns as \code{df} which contains
##' only the most recent row for each unique package name, as determined
##' by the contents of \code{df[[verscol]]}
##' @rdname findnewestpkg
##' @return for \code{findNewestPkgInds}, the indices of the rows representing
##' the newest version of each package within \code{df}. For
##' \code{findNewestPkgRows}, the rows themselves from \code{df} representing
##' the newest version of each package.
##' @export
findNewestPkgInds = function(df, pkgcol = "package", verscol = "version") {
    indsbypkg = split(seq(along = df[[1]]), df[[pkgcol]])
    res = lapply(indsbypkg, function(inds)  .newestPkgVersion(inds, df, verscol))
    unlist(res)
}

##' @rdname findnewestpkg
##' @param newcol character. Experimental. column name for the column indicating that the version is new.
##' @param verbose logical. Should debugging information be written using \code{logfun} during this process.
##' @param logfun function. Logging function (closure) which should be called to write verbose logging messages during the process.
##' @export
findNewestPkgRows = function(df, pkgcol = "package", verscol = "version", newcol = "new",
                             verbose = FALSE, logfun = message) {
    havenew = !is.null(df[[newcol]])
    
    numbefore = nrow(df)
    if(havenew)
        newbefore = sum(df$new)
    inds = findNewestPkgInds(df, "Package", "Version")
    retdat = df[inds,]
    numafter = nrow(retdat)
    if(havenew)
        newafter = sum(retdat[[newcol]])
    
    if(verbose) {
        msg = paste((numbefore - numafter), "non-latest entries pruned")
        if(havenew)
            msg = paste(msg, sprintf("(%d novel, %d pre-existing)",
                                     newbefore - newafter,
                             (numbefore - newbefore) - (numafter - newafter)))
        
        logfun(msg)
        
    }
   
    retdat
}



##' @title update existing package repository
##' @description  Update an existing repository by reading the PACKAGES
##' file and only processing built package tarballs which do not
##' match existing entries.
##'
##' \code{update_PACKAGES} can be much faster than
##' \code{write_PACKAGES} for small-moderate changes to large
##' repository indexes.
##' 
##' @param dir See \code{write_PACKAGES}
##' @param fields See \code{write_PACKAGES}
##' @param type See \code{write_PACKAGES}
##' @param verbose Should informative messages be displayed throughout
##'     the proccess. Defaults to the value of \code{dryrun} (whose
##'     own default is \code{FALSE}) NOT passed to \code{write_PACKAGES}
##' @param unpacked See \code{write_PACKAGES}
##' @param subdirs See \code{write_PACKAGES}
##' @param latestOnly See \code{write_PACKAGES}
##' @param addFiles See \code{write_PACKAGES}
##' @param strict logical. Should 'strict mode' be used when checking
##'     existing PACKAGES entries. See details. Defaults to
##'     \code{TRUE}.
##' @param dryrun logical. Should should the necessary updates be
##'     calculated but NOT applied. (default \code{FALSE})
##' @param logfun function. If \code{verbose} is \code{TRUE}, the
##'     function to be used to emit the informative messages. Defaults
##'     to \code{message}
##' @param \dots Additional arguments to \code{write_PACKAGES} -  e.g., the relatively new
##'     rds_compress argument.
##' @details
##' Throughout this section, \emph{package tarball} is taken to mean a tarball
##' file in \code{dir} whose name can be interpreted as
##' \code{<package>_<version>.<ext>} (or that is pointed to by the \code{File}
##' field of an existing PACKAGES entry). \emph{Novel package tarballs} are
##' those which do not match an existing \code{PACKAGES} file entry.
##'
##' \code{update_PACKAGES} avoids (re)processing package tarballs in cases where
##' a \code{PACKAGES} file entry already exists and appears to remain valid. The
##' logic for detecting still-valid entries is as follows:
##' 
##' Currently \code{update_PACKAGES} calls directly down to
##' \code{write_PACKAGES} (and thus no speedup should be expected)
##' if any of the following conditions hold:
##' \itemize{
##'     \item No \code{PACKAGES} file exists under \code{dir}
##'     \item \code{unpacked} is \code{TRUE}
##'     \item \code{subdirs} is anything other than \code{FALSE}
##'     \item \code{fields} is not \code{NULL} and one or more specified fields
##'       are not present in the existing \code{PACKAGES} file
##'  }
##'
##' All package tarballs whose last modify times are later than that
##' of the existing PACKAGES file are considered novel and no attempt
##' is made to identify or retain any corresponding \code{PACKAGES}
##' entries. Similarly, all \code{PACKAGES} entries which have no
##' corresponding package tarball are definitionally invalid.
##'
##' When \code{strict = TRUE}, \code{PACKAGES} entries which appear to
##' match a package tarball are confirmed via MD5 checksum; those that
##' pass are retained as valid. All novel package tarballs are fully
##' proccessed by the standard \code{write_PACKAGES} machinery, and
##' the resulting entries are added. Finally, if \code{latestOnly =
##' TRUE}, package-version pruning is performed across the entries.
##'
##' When \code{strict = FALSE}, package tarballs are assumed to encode
##' correct metadata in their filenames. \code{PACKAGES} entries which
##' appear to match a package tarball are retained as valid (No MD5sum
##' checking occurs). If \code{latestOnly = TRUE}, package-version
##' pruning across the full set of retained entries and novel package
##' tarballs \emph{before} the processing of the novel tarballs, at
##' significant computational and time savings in some
##' situations. After the optional pruning, any relevant novel package
##' tarballs are processed via \code{write_PACKAGES} and added to the
##' set of retained entries.
##'
##' After the above process concludes, the final database of
##' \code{PACKAGES} entries is written to all three PACKAGES files,
##' overwriting the existing files.
##' 
##' @note While both strict and nonstrict modes offer speedups when
##'     updating small percentages of large repositories, non-strict
##'     mode is \emph{much} faster and is recommended in situations where the
##'     assumptions it makes are safe.
##' @seealso \link[tools]{write_PACKAGES}
##' @author Gabriel Becker
##' @importFrom tools md5sum
##' @export
##' 
update_PACKAGES <- function(dir = ".", fields = NULL, type = c("source", "mac.binary", 
    "win.binary"), verbose = dryrun, unpacked = FALSE, subdirs = FALSE, 
    latestOnly = TRUE, addFiles = FALSE, strict = TRUE,
    dryrun = FALSE, logfun = message, ...)
    {
        type = match.arg(type)
        PKGSfile = file.path(dir, "PACKAGES")
        ## if it doesn't exist we'll be hitting the premature return
        ## in the next if, so its ok that these don't get created.
        ## ugly, I know.
        if(file.exists(PKGSfile)) {
            pmtime = file.info(PKGSfile)$mtime
            ## read without fields restriction, because reducing number
            ## of fields is ok, adding fields means we need reprocessing
            ##
            ## read is as data.frame. We will convert back to
            ## a matrix before writing
            retdat = as.data.frame(read.dcf(PKGSfile),
                                   stringsAsFactors = FALSE)
            okfields = names(retdat)
        }
        
       
        
        ## call straight down to write_PACKAGES if:
        ## 1. no PACKAGES file already exists or its empty
        ## 2. unpacked == TRUE (for now)
        ## 3. subdirs (for now)
        ## 4. field not present in existing PACKAGES file
        if(!file.exists(PKGSfile) || nrow(retdat) == 0 || 
           unpacked ||
           (!is.logical(subdirs) || subdirs) ||
           (!is.null(fields) && !all(fields %in% okfields))) {
            logfun("No PACKAGES file to update or unable to update it. Calling write_PACKAGES directly.")
            return(write_PACKAGES(dir = dir, fields = fields, type = type,
                                  verbose = FALSE, unpacked = unpacked,
                                  subdirs = subdirs, latestOnly = latestOnly,
                                  addFiles = addFiles, ...))
        }
        if(verbose) {
            msg = paste("Detected existing PACKAGES file with ", nrow(retdat), " entries.")
            logfun(msg)
        }
        
        if(!is.null(fields))
            retdat = retdat[, fields]

        retdat$new = FALSE ## mark these as coming from existing PACKAGES

        
        ext = .getExt(dir, regex = TRUE)
        ## need to make sure we only get pkg_vers.ext.
        ## Don't want to hit PACKAGE.tar.gz
        pkgfiles = list.files(dir, pattern = paste0("[[:alnum:]]*_.*", ext),
                              full.names = TRUE)
        if(!length(pkgfiles))
            stop("unable to find any built package files.")

                
        ext2 = gsub("\\.", ".", ext, fixed = TRUE)
        if(is.null(retdat$File))
            retdat$tarball = file.path(dir,paste0(retdat$Package, "_", retdat$Version, ext2))
        else
            retdat$tarball = retdat$File
        
        keeprows = file.exists(retdat$tarball)
        if(verbose) {
            msg = paste("Tarballs found for", sum(keeprows), " of ",
                    nrow(retdat), "PACKAGES entries.")
            logfun(msg)
        }
        
        ## remove entries whose files have been deleted
        retdat = retdat[keeprows,]


        ## check for tarballs that are too new
        ## remove entries which might appear to match them
        ## because the new tarball takes precedence.
        tbmtimes = file.info(retdat$tarball)$mtime
        toonew = which(tbmtimes > pmtime)
        if(verbose){
            msg = paste(length(toonew), " tarball(s) matching existing entries are ",
                    "newer than PACKAGES file and must be reprocessed.")
            logfun(msg)
        }
        if(length(toonew) > 0)
            retdat = retdat[-toonew, ]
        
        ## If in strict mode we confirm that the MD5 sums match for
        ## tarballs which match pre-existing PACKAGES entries.
        ##
        ## Otherwise we skip this check for speed, assuming that
        ## any tarball we find is the one used to create the entry.
        ##
        ## Note: skipping the check can lead to a 'bad' repo in rare
        ## cases, but the installation machinery would still protect
        ## against non-malicious cases of this by failing out when the
        ## MD5 sum didn't match what PACKAGES said it should be.
        if(strict) {
            
            if(verbose) {
                msg = paste("[strict mode] Checking if MD5sums match ",
                        "for existing tarballs")
                logfun(msg)
            }
            curMD5sums = md5sum(normalizePath(retdat$tarball))
            ## There are no NAs in retdat$MD5sum here, as the only data in
            ## there now is from the existing PACKAGES file.
            notokinds = which(retdat$MD5sum != curMD5sums)
            if(length(notokinds)) {
                msg = paste0("Detected ", length(notokinds), " MD5sum mismatches",
                             " between existing PACKAGES file and tarballs")
                if(verbose)
                    logfun(msg)
                warning(msg)
            } else if(verbose) {
                logfun("All existing entry MD5sums match tarballs.") 
            }
            ## tarballs that don't already ahve an entry
            ## OR that mismatched their existing entry
            ## possibly needing to be added
            if(length(notokinds)) {
                retdat = retdat[-notokinds,]
            }
        }
        
        newpkgfiles = setdiff(normalizePath(pkgfiles),
                              normalizePath(retdat$tarball))
        
        ## If we're willing to assume the filenames are honest and
        ## accurate, we can skip non-newest package versions without
        ## ever untaring them and reading their DESCRIPTION files.
        ##
        ## this is not the default because it is technically speaking
        ## less safe than what write_PACKAGES(,latestOnly=TRUE) does
        ## which is always process everything then prune.
        
        if(!strict && length(newpkgfiles) > 0) {
            #strip extension, left with pkgname_version
            newpkgtmp = gsub(ext2, "", basename(newpkgfiles))
            newpkgspl = strsplit(basename(newpkgtmp), "_")
            newpkgdf = do.call(rbind.data.frame,c(newpkgspl,
                                                  stringsAsFactors = FALSE))
            ## Package and Version
            newpkgdf = newpkgdf[,1:2]
            names(newpkgdf) = c("Package", "Version")
            newpkgdf = .filldfcols(newpkgdf, retdat)
            ## for accounting purposes, taken back off later
            newpkgdf$new = TRUE
            newpkgdf$tarball = newpkgfiles
            retdat = rbind(retdat, newpkgdf) 
            ## remove non-latest ones now to avoid the expensive stuff
            ## this is non-strict because it assumes the package name and
            ## version in the filename are accurate. Technically, not
            ## guaranteed.
            if(latestOnly)
                retdat = findNewestPkgRows(retdat, "Package",
                                           "Version",
                                           verbose = verbose,
                                           logfun = logfun)
            newpkgfiles = retdat$tarball[is.na(retdat$MD5sum)]
        }
        
        ## Do any packages/package versions need to be added?
        numnew = length(newpkgfiles)
        if(numnew > 0) {
            if(verbose) {
                msg = paste("Found ", numnew, " package versions to process.")
                logfun(msg)
            }
            indstofix = is.na(retdat$MD5sum)
            ## tempdir thats absolutely guaranteed to be empty
            ## regardless of how many times this session this function has been
            ## called 
            tmpdir = .getEmptyTempDir()
            res = file.symlink(newpkgfiles, file.path(tmpdir,
                                                      basename(newpkgfiles)))
            if(!all(res)) ## fall back to copying if symlinking fails somehow
                res = file.copy(newpkgfiles, tmpdir)
            if(!all(res))
                stop("unable to symlink or copy new package taballs to ",
                     "temp directory.")
            
            if(verbose) {
                msg = paste("Writing temporary PACKAGES files for new package versions in ",
                        tmpdir)
                logfun(msg)
            }
            write_PACKAGES(tmpdir, type = type,
                           verbose = FALSE, unpacked = unpacked,
                           subdirs = subdirs, latestOnly = latestOnly,
                           addFiles = addFiles, ...)
            newpkgdf = as.data.frame(read.dcf(file.path(tmpdir, "PACKAGES")),
                                     stringsAsFactors = FALSE)
            if(!identical(names(newpkgdf), names(retdat))) {
                ## make sure we catch columns only present in one or
                ## the other, regardless of direction.
                ##
                ## the order of columns that comes out of this is columns
                ## in retdat (ie the original PACKAGES) in the order
                ## they appear there, THEN fields unique to the new tarballs
                ## appended in the order they appear there.
                
                retdat = .filldfcols(retdat, newpkgdf)
                newpkgdf = .filldfcols(newpkgdf, retdat)
                
            }
            ## if the File field is present, fix it to point at the
            ## right file instead of the temp symlink/copy.
            if(!is.null(newpkgdf$File)){
                inds = match(basename(newpkgdf$File), basename(newpkgfiles))
                if(anyNA(inds))
                    stop("Missing entry or unexpected, non-empty File field",
                         "in temporary PACKAGES file.")
                newpkgdf$File = newpkgfiles[inds]
            }
            if(verbose) {
                msg = paste("Read ", nrow(newpkgdf), " entries from ",
                        "temporary PACKAGES file")
                logfun(msg)
            }
            
            ## just for accounting purposes
            ## taken back off later
            retdat$new = FALSE
            newpkgdf$new = TRUE
            retdat = rbind(retdat[!is.na(retdat$MD5sum),],
                           newpkgdf)
            if(latestOnly) {
                retdat = findNewestPkgRows(retdat, "Package", "Version",
                                           verbose = verbose,
                                           logfun = logfun)
            }
            ## if(verbose) {
            ##     msg = paste(sum(retdat$new), "entries added or updated, ",
            ##             sum(!retdat$new), " entries retained unchanged.")
            ##     logfun(msg)
            ##  }
            ## clean up accounting column so it doesn't get
            ## written to PACKAGES file
        
            
            
        } else if (verbose) {
            logfun("No new package(s)/package version(s) detected")
        }
        ## clean up temp columns (note this works even if they aren't
        ## there so we don't need to worry about ones that are only
        ## defined within if blocks
        retdat$new = NULL
        retdat$tarball = NULL

        if(verbose) {
            msg = paste("Final updated PACKAGES db contains ",
                    nrow(retdat), " entries.")
            logfun(msg)
        }
        
        if(dryrun) {
            if(verbose)
                logfun("[dryrun mode] Dryrun complete.")
        } else {
            if(verbose)
                logfun("Writing final updated PACKAGES files.")
            ## crucial that db is written as a matrix
            ## otherwise available.packages, etc will fail
            db <- as.matrix(retdat)
            ## copied from the tail end of write_PACKAGES
            con <- file(file.path(dir, "PACKAGES"), "wt")
            write.dcf(db, con)
            close(con)
            con <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
            write.dcf(db, con)
            close(con)
            rownames(db) <- db[, "Package"]
	    rds_compress <- list(...)$rds_compress
	    if(is.null(rds_compress))
		rds_compress <- TRUE
            saveRDS(db, file.path(dir, "PACKAGES.rds"), compress = rds_compress)
            if(verbose)
                logfun("update_PACKAGES complete.")
        }
    }



.filldfcols = function(df, srcdf) {
    newcols = setdiff(names(srcdf), names(df))
    unqcols = setdiff(names(df), names(srcdf) )
    for(ic in newcols) {
        ## NA_integer_ lookup is a hack to give us 1
        ## NA value of the right type for the column
        df[[ic]] = srcdf[[ic]][NA_integer_]
    }
    df = df[,c(names(srcdf), unqcols)]
    df
}

.getEmptyTempDir = function(dirname = "tempPACKAGES") {
    tmpdir0 = file.path(tempdir(), dirname)
    i = 0
    tmpdir = file.path(tmpdir0,i) 
    while(dir.exists(tmpdir)) {
        tmpdir = file.path(tmpdir0, i)
        i = i + 1
    }
    dir.create(tmpdir, recursive = TRUE)
    tmpdir
}
