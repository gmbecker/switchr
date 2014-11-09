annotateDESCs = function(pkgs, man) {
    avl = available.packages(contrib.url(depRepos(man)))
    sapply(pkgs, .annDESC, mandf = manifest_df(man), avl = avl)
    NULL
}

.annDESC = function(pkg, mandf, avl) {
    fil = system.file("DESCRIPTION", package = pkg)
    desc = as.data.frame(read.dcf(fil), stringsAsFactors = FALSE)
    if (pkg %in% mandf$name) {
        row = mandf[mandf$name==pkg,]
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
        else if(grep("bioconductor.org", loc, ingore.case=TRUE))
            type = "bioc"
        else
            type = "repository"
    }

    desc$SourceType = type
    desc$SourceLocation = loc
    desc$SourceBranch = branch
    desc$SourceSubdir = subdir
    write.dcf(desc, fil)
}

