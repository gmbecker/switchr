annotateDESCs = function(pkgs, man, type = "source") {
    avl = available.packages(contrib.url(dep_repos(man), type = type),
                             type = type)
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
        else if(grepl("bioconductor.org", loc, ignore.case=TRUE))
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

