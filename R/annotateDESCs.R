annotateDESCs = function(pkgs, man, type = "source") {
    avl = available.packages(contrib.url(dep_repos(man), type = type),
                             type = type)
    if(any(pkgs %in% switchDeps)) {
        warning("Not annotating newly installed versions of switchr dependencies")
        pkgs = pkgs[!pkgs %in% switchDeps]
    }
    sapply(pkgs, .annDESC, mandf = manifest_df(man), avl = avl)
    NULL
}

.annDESC = function(pkg, mandf, avl) {
    fil = system.file("DESCRIPTION", package = pkg)
    desc = as.data.frame(read.dcf(fil), stringsAsFactors = FALSE)
    if (pkg %in% mandf$name) {
        row = mandf[mandf$name==pkg,]
        if(nrow(row) > 1) {
            warning("Manifest contained package ", pkg, "more than once. Using first match for DESCRIPTION file annotation")
            row = row[1,]
        }
        loc = row$url
        subdir = row$subdir
        type = row$type
        branch = row$branch
    } else {
        loc = gsub("/(src|bin).*/contrib", "", avl[pkg, "Repository"])
        subdir = "."
        branch = "none"
        if(grepl("cran", loc, ignore.case=TRUE))
            type = "cran"
        else if(grepl("bioconductor.org", loc, ignore.case=TRUE))
            type = "bioc"
        else
            type = "repository"
    }
    
    desc$SourceType = type
    desc$SourceLocation = loc
    desc$SourceBranch = branch
    desc$SourceSubdir = subdir
    tryCatch(write.dcf(desc, fil), error = function(e) warning("Failed to annotate DESCRIPTION file for package ", pkg, " with error", e))
      
}

