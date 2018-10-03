
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
##' @export
findNewestPkgRows = function(df, pkgcol = "package", verscol = "version") {
    inds = findNewestPkgInds(df, pkgcol, verscol)
    df[inds,]
}



##' @title update existing package repository
##' @description  Update an existing repository by reading the PACKAGES
##' file and only processing built package tarballs which do not
##' match existing entries.
##'
##' \code{update_PACKAGES} can be much faster than \code{write_PACKAGES} for
##' (relatively) small changes to large repositories.
##' @param dir See \code{write_PACKAGES}
##' @param fields See \code{write_PACKAGES}
##' @param type See \code{write_PACKAGES}
##' @param verbose See \code{write_PACKAGES}
##' @param unpacked See \code{write_PACKAGES}
##' @param subdirs See \code{write_PACKAGES}
##' @param latestOnly See \code{write_PACKAGES}
##' @param addFiles See \code{write_PACKAGES}
##' @param rds_compress See \code{write_PACKAGES}
##' @param strict logical. Should all package tarballs without existing matching
##' entries (by MD5) be processed. if \code{FALSE} package name and version
##' are extracted from the file name and assumed to be true (This saves timef
##' for \code{latestOnly = TRUE}.
##' @param  md5_miss_ok should processing continue if a package version has
##' an existing PACKAGES entry and a file in \code{dir} with mismatching MD5
##' sums. If \code{FALSE} (default) an error is thrown, otherwise a warning.
##' @importFrom tools md5sum
##' @export
##' 
update_PACKAGES <- function(dir = ".", fields = NULL, type = c("source", "mac.binary", 
    "win.binary"), verbose = FALSE, unpacked = FALSE, subdirs = FALSE, 
    latestOnly = TRUE, addFiles = FALSE, rds_compress = "xz", strict = TRUE,
    md5_miss_ok = FALSE, dryrun = FALSE)
    {
        type = match.arg(type)
        PKGSfile = file.path(dir, "PACKAGES")
        ## read without fields restriction, because reducing number
        ## of fields is ok, adding fields means we need reprocessing
        olddat = as.data.frame(read.dcf(PKGSfile),
                               stringsAsFactors = FALSE)
       
        
        ## call straight down to write_PACKAGES if:
        ## 1. no PACKAGES file already exists
        ## 2. unpacked == TRUE (for now)
        ## 3. subdirs (for now)
        ## 4. field in fields that wasn't present in existing PACKAGES file
        if(!file.exists(PKGSfile) || unpacked || !(is.logical(subdirs) || subdirs) ||
           ## field not already present
           (!is.null(fields) && !all(fields %in% names(olddat))))
            return(write_PACKAGES(dir = dir, fields = fields, type = type,
                                  verbose = verbose, unpacked = unpacked,
                                  subdirs = subdirs, latestOnly = latestOnly,
                                  addFiles = addFiles, rds_compress = rds_compress))

        if(verbose)
            message("Detected existing PACKAGES file with ", nrow(olddat), " entries.")
        
        if(!is.null(fields))
            olddat = olddat[, fields]
        
        
        ext = .getExt(dir, regex = TRUE)
        ## need to make sure we only get pkg_vers.ext.
        ## Don't want to hit PACKAGE.tar.gz
        pkgfiles = list.files(dir, pattern = paste0("[[:alnum:]]*_.*", ext),
                              full.names = TRUE)
        if(!length(pkgfiles))
            stop("unable to find any built package files.")

                
        oldpkgs = olddat[, "Package"]
        oldvers = olddat[, "Version"]
        ext2 = gsub("\\.", ".", ext, fixed = TRUE)
        if(is.null(olddat$File))
            oldfilenames = file.path(dir,paste0(oldpkgs, "_", oldvers, ext2))
        else
            oldfilenames = olddat$File
        
        keeprows = file.exists(oldfilenames)

        ## remove entries whose files have been deleted
        retdat = olddat[keeprows,]
        retdat$tarball = oldfilenames = oldfilenames[keeprows]
        
        
        
        ## check which existing tarballs match their listed MD5sums
        ## The data for those is (assumed to be) up-to-date so those files
        ## and entries are ignored beyond this point.
        
        maybeokinds = which(!is.na(retdat$MD5sum))
        notokinds = integer()
        if(length(maybeokinds) > 0) {
            if(verbose)
                message("Found ", length(maybeokinds), " tarballs which match existing PACKAGES entries.",
                        " Checking if MD5sums match.")
            curMD5sums = md5sum(normalizePath(retdat$tarball[maybeokinds]))
            notokinds = maybeokinds[retdat$MD5sum[maybeokinds] != curMD5sums]
            if(length(notokinds)) {
                msg = paste0("Detected ", length(notokinds), " MD5sum mismatches",
                        " between existing PACKAGES file and tarballs")
                if(md5_miss_ok)
                    warning(msg)
                else
                    stop(msg)
                retdat$MD5sum[notokinds] = NA_character_
            } else if(verbose) {
                message("All existing entries OK.")
            }
        }

        
        
        ## tarballs that don't already ahve an entry
        ## OR that mismatched their existing entry
        ## possibly needing to be added
        newpkgfiles = setdiff(normalizePath(pkgfiles),
                              normalizePath(oldfilenames[-notokinds]))


        ## If we're willing to assume the filenames are honest and
        ## accurate, we can skip non-newest package versions without
        ## ever untaring them and reading their DESCRIPTION files.
        ##
        ## this is not the default because it is technically speaking
        ## less safe than what write_PACKAGES(,latestOnly=TRUE) does
        ## which is always process everything then prune.
                
        if(!strict && latestOnly) {
            newpkgtmp = gsub(ext2, "", basename(newpkgfiles))
            newpkgspl = strsplit(basename(newpkgtmp), "_")
            newpkgdf = do.call(rbind.data.frame,c(newpkgspl,
                                                  stringsAsFactors = FALSE))
            ## Package and Version, the rest is junk from the extension
            newpkgdf = newpkgdf[,1:2]
            names(newpkgdf) = c("Package", "Version")
            newpkgdf = .filldfcols(newpkgdf, retdat)
            newpkgdf$tarball = newpkgfiles
            ## remove non-latest ones now to avoid the expensive stuff
            ## this is non-strict because it assumes the package name and
            ## version in the filename are accurate. Technically, not
            ## guaranteed.
            retdat = rbind(retdat, newpkgdf)
            retdat = findNewestPkgRows(retdat, "Package", "Version")
            newpkgfiles = retdat$tarball[is.na(retdat$MD5sum)]
        }

        ## no longer needed
        retdat$tarball = NULL
        ## Do any packages/package versions need to be added?
        numnew = length(newpkgfiles)
        if(numnew > 0) {
            if(verbose)
                message("Found ", numnew, " package versions to process.")     
            indstofix = is.na(retdat$MD5sum)
            tmpdir = .getEmptyTempDir()
            file.copy(newpkgfiles, tmpdir)
            if(verbose)
                message("Writing temporary PACKAGES files for new package versions in ",
                        tmpdir)
            write_PACKAGES(tmpdir, type = type,
                           verbose = verbose, unpacked = unpacked,
                           subdirs = subdirs, latestOnly = latestOnly,
                           addFiles = addFiles, rds_compress = rds_compress)
            newpkgdf = as.data.frame(read.dcf(file.path(tmpdir, "PACKAGES")),
                                     stringsAsFactors = FALSE)
            if(!identical(names(newpkgdf), names(retdat))) {
                ## make sure we catch columns only in one or the other, regardless
                ## of direction.
                ##
                ## the order of columns that comes out of this is columns
                ## in retdat (ie the original PACKAGES) in the order
                ## they appear there, THEN fields unique to the new tarballs
                ## appended in the order they appear there.
                
                retdat = .filldfcols(retdat, newpkgdf)
                newpkgdf = .filldfcols(newpkgdf, retdat)

            }
            if(verbose)
                message("Read ", nrow(newpkgdf), " entries from ",
                        "temporary PACKAGES file")

            ## just for accounting purposes
            ## taken back off later
            retdat$new = FALSE
            newpkgdf$new = TRUE
            retdat = rbind(retdat[!is.na(retdat$MD5sum),],
                           newpkgdf)
            if(latestOnly)
                retdat = findNewestPkgRows(retdat, "Package", "Version")
            if(verbose)
                message(sum(retdat$new), "entries added or updated, ",
                        sum(!retdat$new), " entries unchanged.")
            retdat$new = NULL

            
            ## ## have to make sure we get the right order here...
            ## newmtch = paste(newpkgdf$Package, newpkgdf$Version)
            ## oldmtch = paste(retdat[indstofix, "Package"],
            ##                 retdat[indstofix, "Version"])
            ## hits = match(oldmtch, newmtch)
            ## if(anyNA(hits))
            ##     stop("mismatch between temporarily written PACKAGES file",
            ##          " and expected entries")
            
            ## ## below is NULL if no File column.
            ## oldfils = retdat$File[indstofix]
            ## retdat[ indstofix, ] = newpkgdf[ hits,]
            ## ## fix File field to point to correct location
            ## ## (not temporary location)
            ## if(!is.null(oldfils))
            ##     retdat[indstofix, "File"] = oldfils
            
        } else if (verbose) {
            message("No new package(s)/package version(s) detected")
        }
        if(!dryrun) {
            if(verbose)
                message("Writing final updated PACKAGES files containing ",
                        nrow(retdat), " entries.")
            db <- retdat
            ## copied from the tail end of write_PACKAGES
            con <- file(file.path(dir, "PACKAGES"), "wt")
            write.dcf(db, con)
            close(con)
            con <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
            write.dcf(db, con)
            close(con)
            rownames(db) <- db[, "Package"]
            saveRDS(db, file.path(dir, "PACKAGES.rds"), compress = rds_compress)
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
